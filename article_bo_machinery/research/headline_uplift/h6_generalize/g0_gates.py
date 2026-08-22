#!/usr/bin/env python3
"""G0 validation gates for the H6 benchmark families (protocol: PLAN.md).

Per family: (V1) determinism — same config twice gives bit-equal values;
(V2) exhaustive ground truth where the space is enumerable (K <= 2^20),
recorded to g0_ground_truth.json and, where an analytic optimum exists,
checked against it; (V3) duplicate injection through AuditedObjective
(true dup counted, near-miss not, within-rounding counted on mixed
spaces); (V4) per-eval timing. Exits nonzero if any gate fails.
"""
import itertools
import json
import sys
import time
from pathlib import Path

import numpy as np

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
from bo_audit.benchmarks_g import G_BENCH
from bo_audit.core import AuditedObjective

ANALYTIC0 = {"catf_rastrigin_d4L7", "catf_rosen_d4L7", "catf_griewank_d5L7"}
results, ok = {}, True


def report(label, passed, detail):
    global ok
    print(f"[{'PASS' if passed else 'FAIL'}] {label}: {detail}")
    results[label] = {"pass": bool(passed), "detail": detail}
    ok &= passed


def space_size(space):
    n = 1
    for s in space:
        if s[1] != "cat":
            return None
        n *= len(s[2])
    return n


def exhaustive_min(fn, space, name):
    """Vectorized where the family exposes internals; generic loop else."""
    raw = G_BENCH[name]()[0]
    if hasattr(raw, "_nk"):
        N, K, neighbors, tables = raw._nk
        X = ((np.arange(2 ** N)[:, None] >> np.arange(N)) & 1).astype(np.int8)
        total = np.zeros(2 ** N)
        for i in range(N):
            idx = X[:, i].astype(np.int64)
            for j, nb in enumerate(neighbors[i]):
                idx |= X[:, nb].astype(np.int64) << (j + 1)
            total += tables[i][idx]
        vals = -total / N
        return float(vals.min())
    if hasattr(raw, "_W"):
        W = raw._W
        n = W.shape[0]
        X = ((np.arange(2 ** n)[:, None] >> np.arange(n)) & 1).astype(np.int8)
        best = np.inf
        for chunk in np.array_split(np.arange(2 ** n), 64):
            Xc = X[chunk].astype(float)
            diff = Xc[:, :, None] != Xc[:, None, :]
            cuts = np.einsum("kij,ij->k", diff, W) / 2.0
            best = min(best, float((-cuts).min()))
        return best
    names = [s[0] for s in space]
    levels = [s[2] for s in space]
    best = np.inf
    for combo in itertools.product(*levels):
        v = fn(dict(zip(names, combo)))
        if v < best:
            best = v
    return float(best)


ground = {}
for name in sorted(G_BENCH):
    fn, space = G_BENCH[name]()
    names = [s[0] for s in space]
    rng = np.random.default_rng(99)
    cfg = {}
    for s in space:
        cfg[s[0]] = (s[2][rng.integers(len(s[2]))] if s[1] == "cat"
                     else int(rng.integers(s[2], s[3] + 1)) if s[1] == "int"
                     else float(rng.uniform(s[2], s[3])))

    # V1 determinism + V4 timing
    t0 = time.time()
    v1, v2 = fn(dict(cfg)), fn(dict(cfg))
    dt = (time.time() - t0) / 2
    report(f"{name}/V1-determinism", v1 == v2, f"{v1:.6g} twice, {dt*1000:.0f} ms/eval")

    # V2 exhaustive ground truth (enumerable pure-cat spaces only)
    K = space_size(space)
    if K is not None and K <= 2 ** 20:
        t0 = time.time()
        gmin = exhaustive_min(fn, space, name)
        ground[name] = {"K": K, "min": gmin, "sec": round(time.time() - t0, 1)}
        if name in ANALYTIC0:
            report(f"{name}/V2-analytic0", abs(gmin) < 1e-9,
                   f"exhaustive min {gmin:.3g} vs analytic 0 (K={K})")
        else:
            print(f"[INFO] {name}/V2: exhaustive min {gmin:.6f} over K={K} "
                  f"({ground[name]['sec']}s) -> ground truth recorded")
    else:
        print(f"[INFO] {name}/V2: not enumerable (K={'inf/mixed' if K is None else K}) "
              "— best-value reporting only, no solve threshold")

    # V3 duplicate injection via the audit wrapper
    a = AuditedObjective(fn, space)
    a(dict(cfg)); a(dict(cfg))                     # true duplicate
    cfg2 = dict(cfg)
    fkeys = [s for s in space if s[1] == "float"]
    if fkeys:
        k = fkeys[0][0]
        cfg2[k] = min(fkeys[0][3], cfg[k] + 1e-3); a(dict(cfg2))   # near-miss: new
        cfg3 = dict(cfg); cfg3[k] = cfg[k] + 1e-9; a(dict(cfg3))   # within-rounding: dup
        want_rev, want_uni = 2, 2
    else:
        first = space[0]
        alt = [v for v in first[2] if v != cfg[first[0]]][0]
        cfg2[first[0]] = alt; a(dict(cfg2))
        want_rev, want_uni = 1, 2
    report(f"{name}/V3-injection",
           a.n_revisits == want_rev and len(a.seen) == want_uni,
           f"revisits {a.n_revisits} (want {want_rev}), unique {len(a.seen)} (want {want_uni})")

json.dump({"gates": results, "ground_truth": ground},
          open(HERE / "g0_ground_truth.json", "w"), indent=1)
print("\nG0 GATES:", "ALL PASS" if ok else "FAILURES — fix before G1")
sys.exit(0 if ok else 1)
