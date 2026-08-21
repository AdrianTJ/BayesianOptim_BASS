"""Python reimplementation of the R BO machinery (code_files/R/).

Faithful to candidates.R / bo_loop.R / objectives semantics; RNG streams
necessarily differ from R, so validation is statistical (see
exp01_harness_validation/DESIGN.md). Shared by all article_loop experiments.
"""
from dataclasses import dataclass, field
import numpy as np
from scipy.stats import qmc


# --- schema & encoding -------------------------------------------------------

@dataclass
class Schema:
    types: list          # "cat" | "cont" per dimension
    levels: list         # int for "cat", None for "cont"

    @property
    def cat_idx(self):
        return [j for j, t in enumerate(self.types) if t == "cat"]

    @property
    def pure_cat(self):
        return all(t == "cat" for t in self.types)


def decode_levels(u, L):
    """[0,1] -> 1..L (right endpoint maps to L), as objective_utils.R."""
    return np.minimum(np.floor(np.asarray(u) * L).astype(int) + 1, L)


def canonicalize(X, schema):
    """Snap categorical coords to bin centres (level-0.5)/L."""
    X = np.atleast_2d(np.asarray(X, dtype=float)).copy()
    if schema is None:
        return X
    for j in schema.cat_idx:
        L = schema.levels[j]
        X[:, j] = (decode_levels(X[:, j], L) - 0.5) / L
    return X


def min_sqdist(X_cand, X_eval):
    d = X_cand[:, None, :] - X_eval[None, :, :]
    return np.min(np.sum(d * d, axis=2), axis=1)


# --- candidate generation ----------------------------------------------------

def local_scale(X_eval, best_idx):
    if X_eval.shape[0] < 2:
        return 0.1
    others = np.delete(X_eval, best_idx, axis=0)
    dmin = np.sqrt(np.min(np.sum((others - X_eval[best_idx]) ** 2, axis=1)))
    return float(np.clip(dmin, 1e-2, 0.5))


def _local_categorical_moves(X_local, x_best, schema, rng, force_flip):
    """Hamming-local moves on categorical coords.

    force_flip=True  -> historical 'flip' variant: >=1 flip always forced.
    force_flip=False -> 'keep' variant: >=1 flip forced only on pure-cat
                        schemas (zero-flip rows allowed on mixed schemas).
    """
    cat_idx = schema.cat_idx
    n_cat = len(cat_idx)
    inc_lev = [int(decode_levels(x_best[j], schema.levels[j])) for j in cat_idx]
    must_flip = force_flip or schema.pure_cat

    for r in range(X_local.shape[0]):
        flips = np.nonzero(rng.random(n_cat) < 1.0 / n_cat)[0]
        if must_flip and flips.size == 0:
            flips = np.array([rng.integers(n_cat)])
        for c, j in enumerate(cat_idx):
            L = schema.levels[j]
            lev = inc_lev[c]
            if c in flips:
                choices = [l for l in range(1, L + 1) if l != lev]
                lev = choices[rng.integers(len(choices))]
            X_local[r, j] = (lev - 0.5) / L
    return X_local


def hybrid_candidates(X_eval, y_eval, n_cand, schema, rng, variant="keep"):
    """Global LHS half + local Gaussian/Hamming half (candidates.R).

    variant: 'keep' (current library, zero-flip allowed on mixed schemas) or
    'flip' (historical restricted generator, >=1 categorical flip forced).
    """
    d = X_eval.shape[1]
    n_local = n_cand // 2
    n_global = n_cand - n_local

    sampler = qmc.LatinHypercube(d=d, seed=rng)
    X_global = sampler.random(n_global)

    best_idx = int(np.argmin(y_eval))
    x_best = X_eval[best_idx]
    s = local_scale(X_eval, best_idx)
    X_local = np.clip(rng.normal(x_best, s, size=(n_local, d)), 0.0, 1.0)

    if schema is not None and n_local > 0 and schema.cat_idx:
        X_local = _local_categorical_moves(
            X_local, x_best, schema, rng, force_flip=(variant == "flip"))

    return np.vstack([X_global, X_local])


# --- the loop ----------------------------------------------------------------

@dataclass
class Config:
    budget: int = 80
    n_cand: int = 1000
    dup_tol: float = 1e-10


def shared_init(objective, seed):
    """Seeded space-filling initial design, n0 = max(2d+1, 8)."""
    d = objective["d"]
    n0 = max(2 * d + 1, 8)
    X = qmc.LatinHypercube(d=d, seed=seed).random(n0)
    return X, objective["fn"](X)


def run_bo(objective, method, cfg, X_init, y_init, rng, dedup="combination"):
    """Generic loop (bo_loop.R): propose -> score -> dedup mask -> argmax
    (random tie-break) -> evaluate. method = dict(candidates=..., acquire=...)
    with None acquire meaning Random Search.

    dedup: 'combination' (mask on canonicalized points — current library) or
    'encoding' (mask on raw encodings — the historical pre-fix leak).
    `revisits` counts picks whose canonicalized representation duplicated an
    already-evaluated point (waste on a deterministic objective), regardless
    of the dedup mode in force.
    """
    f, schema = objective["fn"], objective.get("schema")
    X_eval = np.atleast_2d(np.asarray(X_init, dtype=float))
    y_eval = np.asarray(y_init, dtype=float).ravel()
    d = X_eval.shape[1]

    best = np.empty(cfg.budget + 1)
    best[0] = y_eval.min()
    revisits = 0

    for t in range(1, cfg.budget + 1):
        X_canon = canonicalize(X_eval, schema)
        X_seen = X_canon if dedup == "combination" else X_eval
        rep = canonicalize if dedup == "combination" else (lambda X, s: np.atleast_2d(X))
        if method["acquire"] is None:
            for _ in range(100):
                x_next = rng.random((1, d))
                if min_sqdist(rep(x_next, schema), X_seen)[0] > cfg.dup_tol ** 2:
                    break
        else:
            X_cand = method["candidates"](X_eval, y_eval, rng)
            score = np.asarray(method["acquire"](X_eval, y_eval, X_cand), dtype=float)
            dup = min_sqdist(rep(X_cand, schema), X_seen) <= cfg.dup_tol ** 2
            score[dup] = -np.inf
            top = np.nonzero(score == score.max())[0]
            x_next = X_cand[top[rng.integers(top.size)] if top.size > 1 else top[0]][None, :]

        if min_sqdist(canonicalize(x_next, schema), X_canon)[0] <= cfg.dup_tol ** 2:
            revisits += 1
        y_next = f(x_next)
        X_eval = np.vstack([X_eval, x_next])
        y_eval = np.append(y_eval, y_next)
        best[t] = y_eval.min()

    return {"best": best, "X": X_eval, "y": y_eval, "revisits": revisits}


def oracle_method(objective, cfg, variant):
    """Acquisition = true objective (score -f): the oracle-ceiling arm."""
    schema = objective.get("schema")
    return {
        "name": f"oracle+{variant}",
        "candidates": lambda X, y, rng: hybrid_candidates(
            X, y, cfg.n_cand, schema, rng, variant=variant),
        "acquire": lambda X, y, Xc: -objective["fn"](Xc),
    }


RANDOM = {"name": "Random", "candidates": None, "acquire": None}


# --- objectives (categorical.R, exact constants) -----------------------------

def _rosen(x1, x2):
    return (100 * (x2 - x1 ** 2) ** 2 + (x1 - 1) ** 2) / 300


def _camel(x1, x2):
    return ((4 - 2.1 * x1 ** 2 + x1 ** 4 / 3) * x1 ** 2 + x1 * x2
            + (-4 + 4 * x2 ** 2) * x2 ** 2) / 10


def _beale(x1, x2):
    return ((1.5 - x1 + x1 * x2) ** 2 + (2.25 - x1 + x1 * x2 ** 2) ** 2
            + (2.625 - x1 + x1 * x2 ** 3) ** 2) / 50


def _apply_fn(which, x1, x2):
    out = np.empty_like(x1)
    for k, fn in ((1, _rosen), (2, _camel), (3, _beale)):
        m = which == k
        out[m] = fn(x1[m], x2[m])
    return out


def make_func2C():
    def fn(X):
        X = np.atleast_2d(X)
        h1 = decode_levels(X[:, 0], 3)
        h2 = decode_levels(X[:, 1], 5)
        x1, x2 = 4 * X[:, 2] - 2, 4 * X[:, 3] - 2
        h2_fn = np.where(h2 == 1, 1, np.where(h2 == 2, 2, 3))
        return _apply_fn(h1, x1, x2) + _apply_fn(h2_fn, x1, x2)
    return {"name": "func2C", "fn": fn, "d": 4, "optimum": -0.2063,
            "schema": Schema(["cat", "cat", "cont", "cont"], [3, 5, None, None])}


def make_func3C():
    def fn(X):
        X = np.atleast_2d(X)
        h1 = decode_levels(X[:, 0], 3)
        h2 = decode_levels(X[:, 1], 5)
        h3 = decode_levels(X[:, 2], 4)
        x1, x2 = 4 * X[:, 3] - 2, 4 * X[:, 4] - 2
        h2_fn = np.where(h2 == 1, 1, np.where(h2 == 2, 2, 3))
        base = _apply_fn(h1, x1, x2) + _apply_fn(h2_fn, x1, x2)
        extra = np.where(h3 == 1, 5 * _camel(x1, x2),
                         np.where(h3 == 2, 2 * _rosen(x1, x2),
                                  (h3 - 1) * _beale(x1, x2)))
        return base + extra
    return {"name": "func3C", "fn": fn, "d": 5, "optimum": -0.7216,
            "schema": Schema(["cat", "cat", "cat", "cont", "cont"],
                             [3, 5, 4, None, None])}


def make_cat_ackley(d, L=11, seed=1):
    """Permuted-categorical Ackley; optimum 0. Permutations are numpy-seeded
    (NOT the R instance -- same family, different fixed permutation)."""
    assert L % 2 == 1, "odd L so 0 lies on the grid"
    grid = np.linspace(-32.768, 32.768, L)
    perms = [np.random.default_rng(seed * 1000 + j).permutation(L) for j in range(d)]

    def fn(X):
        X = np.atleast_2d(X)
        g = np.empty_like(X, dtype=float)
        for j in range(d):
            lev = decode_levels(X[:, j], L)          # 1..L
            g[:, j] = grid[perms[j][lev - 1]]
        rms = np.sqrt(np.mean(g ** 2, axis=1))
        return (-20 * np.exp(-0.2 * rms)
                - np.exp(np.mean(np.cos(2 * np.pi * g), axis=1)) + 20 + np.e)
    return {"name": f"cat_ackley_d{d}_L{L}", "fn": fn, "d": d, "optimum": 0.0,
            "schema": Schema(["cat"] * d, [L] * d)}


OBJECTIVES = {
    "func2C": make_func2C,
    "func3C": make_func3C,
}
