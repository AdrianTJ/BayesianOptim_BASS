#!/usr/bin/env python3
"""H4 strong-surrogate generator attempt. Protocol per DESIGN.md (committed
before this runner produced any result; fresh seeds 1051-1080).

Modes: `pilot` (20 runs, seeds 1051-1055, budget 80) then `confirm`
(200 runs, seeds 1056-1080 + 50 oracle-240 ceiling references).
Arms carry fixed indices for the E3 rng formula
(default_rng(seed*1000 + bench_ix*100 + arm_ix)):
  0 = keep-b80, 1 = flip-b80, 2 = keep-b240, 3 = flip-b240, 4 = oracle-keep-b240.

The strengthened GP is E3's gp_ei_acquire with exactly two pre-named knob
changes (DESIGN.md): n_restarts_optimizer 1 -> 10 and random_state 0 ->
seed % 2**31. Kernel, normalize_y, EI, and everything else are untouched.
"""
import os
for _v in ("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS",
           "NUMEXPR_NUM_THREADS"):
    os.environ.setdefault(_v, "1")
import csv
import sys
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np
from scipy.stats import wilcoxon

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from machinery import (Config, oracle_method, hybrid_candidates, make_func2C,
                       make_func3C, run_bo, shared_init)

BENCH = [("func2C", make_func2C), ("func3C", make_func3C)]
N_CAND = 50
PILOT_SEEDS = list(range(1051, 1056))
CONF_SEEDS = list(range(1056, 1081))
ARMS = [("keep", 80, 0), ("flip", 80, 1), ("keep", 240, 2), ("flip", 240, 3)]


def gp_ei_acquire_strong(X_eval, y_eval, X_cand, seed):
    """E3's GP-EI with the two pre-named strengthening knobs (DESIGN.md)."""
    import warnings
    from sklearn.exceptions import ConvergenceWarning
    from sklearn.gaussian_process import GaussianProcessRegressor
    from sklearn.gaussian_process.kernels import Matern, WhiteKernel
    from surrogates import _ei

    kernel = Matern(nu=2.5, length_scale=0.2, length_scale_bounds=(1e-2, 1e1)) \
        + WhiteKernel(1e-6, (1e-10, 1e-1))
    gp = GaussianProcessRegressor(kernel=kernel, normalize_y=True,
                                  n_restarts_optimizer=10,
                                  random_state=seed % 2**31)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=ConvergenceWarning)
        gp.fit(X_eval, y_eval)
    mu, sd = gp.predict(X_cand, return_std=True)
    return _ei(mu, sd, y_eval.min())


def job(args):
    bench_ix, bname, seed, arm_ix, gen, budget = args
    obj = BENCH[bench_ix][1]()
    cfg = Config(budget=budget, n_cand=N_CAND)
    rng = np.random.default_rng(seed * 1000 + bench_ix * 100 + arm_ix)
    X0, y0 = shared_init(obj, seed)
    schema = obj.get("schema")

    if gen == "oracle":
        method = oracle_method(obj, cfg, "keep")
        r = run_bo(obj, method, cfg, X0, y0, rng)
        name = "oracle"
    else:
        method = {"name": f"gp+{gen}+combination",
                  "candidates": lambda X, y, g: hybrid_candidates(
                      X, y, cfg.n_cand, schema, g, variant=gen),
                  "acquire": lambda X, y, Xc: gp_ei_acquire_strong(
                      X, y, Xc, seed)}
        r = run_bo(obj, method, cfg, X0, y0, rng, dedup="combination")
        name = "gp"

    b = r["best"]
    return {"objective": bname, "seed": seed, "surrogate": name,
            "generator": gen, "dedup": "combination", "budget": budget,
            "best_b10": b[10], "best_b40": b[40], "best_b80": b[80],
            "best_b240": b[240] if budget >= 240 else "",
            "revisits": r["revisits"]}


def run_mode(mode):
    jobs = []
    if mode == "pilot":
        seeds = PILOT_SEEDS
        arms = [("keep", 80, 0), ("flip", 80, 1)]
        out = Path(__file__).parent / "results_pilot.csv"
    else:
        seeds = CONF_SEEDS
        arms = ARMS + [("oracle", 240, 4)]
        out = Path(__file__).parent / "results.csv"

    for bench_ix, (bname, _) in enumerate(BENCH):
        for seed in seeds:
            for arm_ix, (gen, budget, rng_ix) in enumerate(arms):
                jobs.append((bench_ix, bname, seed, rng_ix, gen, budget))

    rows = []
    with ProcessPoolExecutor(max_workers=8) as ex:
        for i, row in enumerate(ex.map(job, jobs, chunksize=2)):
            rows.append(row)
            if (i + 1) % 25 == 0:
                print(f"{i+1}/{len(jobs)} runs done", flush=True)

    with out.open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print(f"wrote {out} ({len(rows)} runs)")
    return rows


if __name__ == "__main__":
    mode = sys.argv[1] if len(sys.argv) > 1 else "pilot"
    assert mode in ("pilot", "confirm"), mode
    run_mode(mode)
