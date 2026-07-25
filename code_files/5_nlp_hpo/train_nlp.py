# /// script
# requires-python = ">=3.11,<3.12"
# dependencies = ["torch>=2.2", "numpy", "scikit-learn"]
# ///
"""
NLP HPO objective: train a small from-scratch text classifier and return
validation accuracy.

This is the expensive black-box the BO methods optimize over. Every
hyperparameter is a discrete choice (see HP_SPACE), so the whole search space is
categorical. The choices deliberately mix two kinds:

  * genuinely UNORDERED categoricals: arch, optimizer, activation, pooling.
    These are where a Gaussian process, which sees each categorical coordinate
    as its ordered bin position on [0,1] in this pipeline, is actually
    handicapped, because bin adjacency carries no real meaning.
  * discretized-ORDERED categoricals: lr, embed_dim, dropout, batch_size.
    These are continuous quantities cut into buckets. Their order is real, so a
    GP's ordered relaxation of them is faithful and NOT a handicap.

Training is from scratch (no pretrained model) for a fixed small epoch budget,
so the optimizer/lr/architecture choices genuinely determine whether the model
learns at all within budget. That produces a wide spread of validation accuracy
across configurations, which is exactly what the elastic-net case study lacked.

Usage (one evaluation):
    uv run train_nlp.py --config '{"arch":"cnn","optimizer":"adam",...}' --seed 1
Prints one line: RESULT <validation_error>   (error = 1 - accuracy, MINIMIZED)

Data is tokenized once and cached to data_cache.pt so repeated evaluations are
fast and network-independent after the first run.
"""
import argparse
import hashlib
import json
import os
import sys
import time

import numpy as np
import torch
import torch.nn as nn

HERE = os.path.dirname(os.path.abspath(__file__))
CACHE = os.path.join(HERE, "data_cache.pt")
RESULT_CACHE = os.path.join(HERE, "eval_cache.json")

# ---- the categorical hyperparameter space -----------------------------------
HP_ORDER = ["arch", "optimizer", "activation", "pooling",
            "lr", "embed_dim", "dropout", "batch_size"]
HP_SPACE = {
    "arch":       ["meanpool", "cnn", "bilstm"],          # unordered
    "optimizer":  ["sgd", "adam", "rmsprop", "adamw"],    # unordered
    "activation": ["relu", "tanh", "gelu"],               # unordered
    "pooling":    ["mean", "max", "last"],                # unordered
    "lr":         [1e-3, 3e-3, 1e-2, 3e-2, 1e-1],         # ordered (discretized)
    "embed_dim":  [32, 64, 128],                          # ordered
    "dropout":    [0.0, 0.3, 0.6],                        # ordered
    "batch_size": [32, 128, 512],                         # ordered
}

MAX_LEN = 45
VOCAB_SIZE = 20000
N_TRAIN = 12000
N_VAL = 4000
EPOCHS = 3
DEVICE = "cpu"  # tiny models: CPU beats MPS transfer overhead here


def build_cache():
    """Fetch AG News CSVs directly (no datasets/pyarrow), tokenize, cache once.
    Falls back to 20 Newsgroups (headers kept, for a higher accuracy ceiling)."""
    from collections import Counter
    import csv, io, urllib.request

    AG = ("https://raw.githubusercontent.com/mhjabreel/CharCnn_Keras/master/"
          "data/ag_news_csv/{}.csv")

    def load_ag(split):
        raw = urllib.request.urlopen(AG.format(split), timeout=30).read().decode("utf-8")
        texts, labels = [], []
        for row in csv.reader(io.StringIO(raw)):
            if len(row) < 3:
                continue
            labels.append(int(row[0]) - 1)             # classes 1..4 -> 0..3
            texts.append((row[1] + " " + row[2]))
        return texts, labels

    try:
        texts_tr, labels_tr = load_ag("train")
        texts_va, labels_va = load_ag("test")
        texts_tr, labels_tr = texts_tr[:N_TRAIN], labels_tr[:N_TRAIN]
        texts_va, labels_va = texts_va[:N_VAL], labels_va[:N_VAL]
        n_classes, source = 4, "ag_news"
    except Exception as e:
        print(f"ag_news download failed ({e}); using 20newsgroups", file=sys.stderr)
        from sklearn.datasets import fetch_20newsgroups
        tr = fetch_20newsgroups(subset="train")
        va = fetch_20newsgroups(subset="test")
        texts_tr, labels_tr = tr.data[:N_TRAIN], list(tr.target[:N_TRAIN])
        texts_va, labels_va = va.data[:N_VAL], list(va.target[:N_VAL])
        n_classes, source = 20, "20newsgroups"

    def toks(x):
        return x.lower().split()

    counter = Counter()
    for x in texts_tr:
        counter.update(toks(x))
    vocab = {w: i + 2 for i, (w, _) in enumerate(counter.most_common(VOCAB_SIZE - 2))}

    def encode(x):
        ids = [vocab.get(w, 1) for w in toks(x)[:MAX_LEN]]
        ids += [0] * (MAX_LEN - len(ids))
        return ids

    Xtr = torch.tensor([encode(x) for x in texts_tr], dtype=torch.long)
    Xva = torch.tensor([encode(x) for x in texts_va], dtype=torch.long)
    ytr = torch.tensor(labels_tr, dtype=torch.long)
    yva = torch.tensor(labels_va, dtype=torch.long)
    torch.save({"Xtr": Xtr, "ytr": ytr, "Xva": Xva, "yva": yva,
                "n_classes": n_classes, "source": source}, CACHE)
    print(f"cached {source}: {Xtr.size(0)} train / {Xva.size(0)} val, "
          f"{n_classes} classes", file=sys.stderr)
    return torch.load(CACHE, weights_only=True)


def get_data():
    if not os.path.exists(CACHE):
        return build_cache()
    return torch.load(CACHE, weights_only=True)


ACT = {"relu": nn.ReLU, "tanh": nn.Tanh, "gelu": nn.GELU}


class TextClassifier(nn.Module):
    def __init__(self, cfg, vocab_size, n_classes):
        super().__init__()
        d = cfg["embed_dim"]
        self.arch = cfg["arch"]
        self.pooling = cfg["pooling"]
        self.emb = nn.Embedding(vocab_size, d, padding_idx=0)
        act = ACT[cfg["activation"]]
        self.drop = nn.Dropout(cfg["dropout"])
        if self.arch == "meanpool":
            self.enc = None
            hid = d
        elif self.arch == "cnn":
            self.enc = nn.Conv1d(d, d, kernel_size=3, padding=1)
            self.enc_act = act()
            hid = d
        elif self.arch == "bilstm":
            self.enc = nn.LSTM(d, d, batch_first=True, bidirectional=True)
            hid = 2 * d
        self.head = nn.Sequential(nn.Linear(hid, hid), act(), self.drop,
                                  nn.Linear(hid, n_classes))

    def _pool(self, h, mask):
        if self.pooling == "mean":
            s = (h * mask.unsqueeze(-1)).sum(1)
            return s / mask.sum(1, keepdim=True).clamp(min=1)
        if self.pooling == "max":
            h = h.masked_fill(~mask.unsqueeze(-1).bool(), -1e9)
            return h.max(1).values
        # last real token
        idx = mask.sum(1).long().clamp(min=1) - 1
        return h[torch.arange(h.size(0)), idx]

    def forward(self, x):
        mask = (x != 0).float()
        h = self.emb(x)
        if self.arch == "cnn":
            h = self.enc_act(self.enc(h.transpose(1, 2)).transpose(1, 2))
        elif self.arch == "bilstm":
            h, _ = self.enc(h)
        h = self._pool(h, mask)
        return self.head(self.drop(h))


OPT = {"sgd": lambda p, lr: torch.optim.SGD(p, lr=lr, momentum=0.9),
       "adam": lambda p, lr: torch.optim.Adam(p, lr=lr),
       "rmsprop": lambda p, lr: torch.optim.RMSprop(p, lr=lr),
       "adamw": lambda p, lr: torch.optim.AdamW(p, lr=lr)}


def train_eval(cfg, seed):
    torch.manual_seed(seed)
    np.random.seed(seed)
    data = get_data()
    Xtr, ytr, Xva, yva = data["Xtr"], data["ytr"], data["Xva"], data["yva"]
    n_classes = int(data["n_classes"])

    model = TextClassifier(cfg, VOCAB_SIZE, n_classes).to(DEVICE)
    opt = OPT[cfg["optimizer"]](model.parameters(), cfg["lr"])
    lossf = nn.CrossEntropyLoss()
    bs = cfg["batch_size"]
    Xva_d, yva_d = Xva.to(DEVICE), yva.to(DEVICE)

    n = Xtr.size(0)
    for _ in range(EPOCHS):
        model.train()
        perm = torch.randperm(n)
        for i in range(0, n, bs):
            idx = perm[i:i + bs]
            xb, yb = Xtr[idx].to(DEVICE), ytr[idx].to(DEVICE)
            opt.zero_grad()
            out = model(xb)
            loss = lossf(out, yb)
            if not torch.isfinite(loss):   # diverged (bad lr/optimizer)
                return 1.0                 # worst possible error
            loss.backward()
            opt.step()

    model.eval()
    with torch.no_grad():
        preds = []
        for i in range(0, Xva_d.size(0), 1024):
            preds.append(model(Xva_d[i:i + 1024]).argmax(1))
        acc = (torch.cat(preds) == yva_d).float().mean().item()
    return 1.0 - acc


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--config", help="JSON dict of hyperparameters (manual use)")
    ap.add_argument("--levels", help="comma-separated 0-based level index per HP, in HP_ORDER "
                                     "(the pipeline path: Python owns the index->value map)")
    ap.add_argument("--seed", type=int, default=0)  # objective is deterministic; seed fixed by caller
    a = ap.parse_args()
    if a.levels is not None:
        idx = [int(v) for v in a.levels.split(",")]
        assert len(idx) == len(HP_ORDER), f"need {len(HP_ORDER)} levels, got {len(idx)}"
        cfg = {k: HP_SPACE[k][i] for k, i in zip(HP_ORDER, idx)}
    else:
        cfg = json.loads(a.config)

    # result cache keyed by (config, seed): categorical BO revisits configs often
    key = hashlib.md5(json.dumps(cfg, sort_keys=True).encode()).hexdigest()  # config-only: f is deterministic
    cache = {}
    if os.path.exists(RESULT_CACHE):
        try:
            cache = json.load(open(RESULT_CACHE))
        except Exception:
            cache = {}
    if key in cache:
        print(f"RESULT {cache[key]}")
        return

    t0 = time.time()
    err = train_eval(cfg, a.seed)
    cache[key] = err
    tmp = RESULT_CACHE + f".tmp.{os.getpid()}"
    with open(tmp, "w") as fh:
        json.dump(cache, fh)
    os.replace(tmp, RESULT_CACHE)   # atomic: never leaves a half-written cache
    print(f"# cfg={cfg} seed={a.seed} err={err:.4f} time={time.time()-t0:.1f}s",
          file=sys.stderr)
    print(f"RESULT {err}")


if __name__ == "__main__":
    main()
