"""Vendored fallback copy of the H1 objective-builder closure from
article_bo_machinery/research/article_loop/experiments/machinery.py.

bo_audit.benchmarks.bench_by_name needs `Schema`, `make_cat_ackley`,
`OBJECTIVES` (which supplies `make_func2C`/`make_func3C`) and their
private helpers to build the cat_ackley/func2C/func3C objectives, but
machinery.py lives under article_bo_machinery/ (a research tree, not
part of the installed bo-audit package) so it is not importable for a
plain `pip install`. This file vendors exactly that closure so the
installed package is self-sufficient.

bench_by_name prefers `from machinery import ...` when machinery.py is
on the path (so reproducing a published run executes the identical
code that produced it) and falls back to this module otherwise.

Everything below this docstring is copied byte-for-byte from
machinery.py (as of this vendoring) and MUST be kept identical to it.
Do not reformat, rename, "clean up", modernise, add type hints, or
change any constant. bo-audit/tests/test_benchmarks_h1_parity.py
checks the two copies stay in exact agreement whenever machinery.py is
importable; if that test fails, this file has drifted from its source
and must be re-synced, not "fixed" independently.

Deliberately NOT vendored: the BO harness (`run_bo`, `hybrid_candidates`,
`oracle_method`, `shared_init`, `Config`, `canonicalize`, `min_sqdist`,
RNG-driven candidate generation, etc.) -- none of that is needed to
evaluate an objective, and pulling it in would vendor far more than
bench_by_name actually uses.
"""
from dataclasses import dataclass
import numpy as np


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
