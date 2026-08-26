#!/usr/bin/env python3
"""E8: one-shot K10 final test. Protocol per DESIGN.md (fresh seeds,
win-rate-only criteria, benchmark-indexed noise rng)."""
import csv
import sys
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np
from scipy.stats import wilcoxon

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from machinery import Config, hybrid_candidates, make_func2C, make_func3C, run_bo, shared_init

SEEDS = list(range(1026, 1051))          # fresh, never used before
BENCH = [("func2C", make_func2C), ("func3C", make_func3C)]
SIGMAS = [0.0, 1.0, 3.0, 10.0, 30.0, 100.0]
POOLS = [1000, 50]
ANCHOR = {"func2C": 10.0, "func3C": 30.0}


def job(args):
    bench_ix, seed, arm_ix, gen, sigma, n_cand = args
    bname, make = BENCH[bench_ix]
    obj = make()
    X0, y0 = shared_init(obj, seed)
    cfg = Config(budget=80, n_cand=n_cand)
    schema = obj["schema"]
    nrng = np.random.default_rng(seed * 7919 + bench_ix * 1000 + arm_ix)
    m = {"name": "dial",
         "candidates": lambda X, y, g: hybrid_candidates(X, y, cfg.n_cand, schema, g, variant=gen),
         "acquire": lambda X, y, Xc: -obj["fn"](Xc) + sigma * nrng.standard_normal(len(Xc))}
    r = run_bo(obj, m, cfg, X0, y0,
               np.random.default_rng(seed * 1000 + bench_ix * 100 + arm_ix))
    return {"objective": bname, "seed": seed, "generator": gen, "sigma": sigma,
            "n_cand": n_cand, "best_b80": r["best"][80]}


def all_jobs():
    for bench_ix, _ in enumerate(BENCH):
        for seed in SEEDS:
            arm_ix = 0
            for gen in ("keep", "flip"):
                for sigma in SIGMAS:
                    for n_cand in POOLS:
                        yield (bench_ix, seed, arm_ix, gen, sigma, n_cand); arm_ix += 1


if __name__ == "__main__":
    jobs = list(all_jobs())
    rows = []
    with ProcessPoolExecutor(max_workers=4) as ex:
        for i, row in enumerate(ex.map(job, jobs, chunksize=8)):
            rows.append(row)
            if (i + 1) % 200 == 0:
                print(f"{i+1}/{len(jobs)} done", flush=True)

    with (Path(__file__).parent / "results.csv").open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        w.writeheader(); w.writerows(rows)

    def sel(**kw):
        return np.array([r["best_b80"] for r in rows
                         if all(r[k] == v for k, v in kw.items())])

    stats = {}
    print("\n== W(sigma) table (fresh seeds 1026-1050) ==")
    for bname, _ in BENCH:
        for n_cand in POOLS:
            line = []
            for sigma in SIGMAS:
                k = sel(objective=bname, generator="keep", sigma=sigma, n_cand=n_cand)
                f = sel(objective=bname, generator="flip", sigma=sigma, n_cand=n_cand)
                w_ = int(np.sum(k < f))
                p = wilcoxon(k, f).pvalue if np.any(k != f) else 1.0
                stats[(bname, n_cand, sigma)] = (w_, p)
                line.append(f"s{sigma:g}: W={w_:>2} p={p:.1e}")
            print(f"{bname} n={n_cand}: " + " | ".join(line))

    # Pre-registered decision, evaluated by the letter
    support = all(
        stats[(b, n, 0.0)][0] >= 20 and stats[(b, n, 0.0)][1] < 0.05
        and stats[(b, n, ANCHOR[b])][0] <= 17 and stats[(b, n, ANCHOR[b])][1] >= 0.05
        for b, _ in BENCH for n in POOLS)
    refute = all(
        stats[(b, 50, ANCHOR[b])][0] >= 20 and stats[(b, 50, ANCHOR[b])][1] < 0.05
        for b, _ in BENCH)
    verdict = ("H-REGIME SUPPORTED" if support else
               "H-REGIME REFUTED" if refute else "INCONCLUSIVE-FINAL")
    print("\nPre-registered verdict:", verdict)
    for b, _ in BENCH:
        for n in POOLS:
            w0, p0 = stats[(b, n, 0.0)]
            ws, ps = stats[(b, n, ANCHOR[b])]
            print(f"  {b} n={n}: W(0)={w0} p={p0:.1e} | W(sigma*={ANCHOR[b]:g})={ws} p={ps:.1e}")
