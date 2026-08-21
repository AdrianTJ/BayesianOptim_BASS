#!/usr/bin/env python3
"""E3 surrogate x machinery matrix. Protocol per DESIGN.md.

10 arms x 3 benchmarks x 25 seeds, budget 80, parallel across runs.
Writes results.csv and prints H1-H4 checks.
"""
import csv
import sys
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np
from scipy.stats import wilcoxon

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from machinery import (Config, RANDOM, hybrid_candidates, make_cat_ackley,
                       make_func2C, make_func3C, run_bo, shared_init)
from surrogates import gp_ei_acquire, rf_ei_acquire, run_tpe

SEEDS = list(range(1001, 1026))
BENCH = [("func2C", make_func2C), ("func3C", make_func3C),
         ("cat_ackley_d3_L5", lambda: make_cat_ackley(3, 5))]
CELLS = [(gen, ded) for gen in ("keep", "flip") for ded in ("combination", "encoding")]


def job(args):
    bench_ix, bname, seed, arm_ix, arm = args
    make = BENCH[bench_ix][1]
    obj = make()
    cfg = Config(budget=80, n_cand=1000)
    rng = np.random.default_rng(seed * 1000 + bench_ix * 100 + arm_ix)
    X0, y0 = shared_init(obj, seed)

    if arm == "tpe":
        r = run_tpe(obj, cfg.budget, seed)
        gen = ded = "own"
    elif arm == "random":
        r = run_bo(obj, RANDOM, cfg, X0, y0, rng)
        gen, ded = "random", "combination"
    else:
        surro, gen, ded = arm
        schema = obj.get("schema")
        acquire = (gp_ei_acquire if surro == "gp" else
                   lambda X, y, Xc: rf_ei_acquire(X, y, Xc, seed=seed % 2**31))
        method = {"name": f"{surro}+{gen}+{ded}",
                  "candidates": lambda X, y, g: hybrid_candidates(X, y, cfg.n_cand, schema, g, variant=gen),
                  "acquire": acquire}
        r = run_bo(obj, method, cfg, X0, y0, rng, dedup=ded)

    b = r["best"]
    name = arm if isinstance(arm, str) else arm[0]
    return {"objective": bname, "seed": seed, "surrogate": name,
            "generator": gen, "dedup": ded,
            "best_b10": b[10], "best_b40": b[40], "best_b80": b[80],
            "revisits": r.get("revisits", -1)}


def all_jobs():
    for bench_ix, (bname, _) in enumerate(BENCH):
        for seed in SEEDS:
            arm_ix = 0
            for surro in ("gp", "rf"):
                for gen, ded in CELLS:
                    yield (bench_ix, bname, seed, arm_ix, (surro, gen, ded)); arm_ix += 1
            yield (bench_ix, bname, seed, arm_ix, "tpe"); arm_ix += 1
            yield (bench_ix, bname, seed, arm_ix, "random"); arm_ix += 1


if __name__ == "__main__":
    jobs = list(all_jobs())
    rows = []
    with ProcessPoolExecutor(max_workers=4) as ex:
        for i, row in enumerate(ex.map(job, jobs, chunksize=4)):
            rows.append(row)
            if (i + 1) % 50 == 0:
                print(f"{i+1}/{len(jobs)} runs done", flush=True)

    out = Path(__file__).parent / "results.csv"
    with out.open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        w.writeheader(); w.writerows(rows)

    # ---- hypothesis checks --------------------------------------------------
    def sel(col="best_b80", **kw):
        return np.array([r[col] for r in rows if all(r[k] == v for k, v in kw.items())])

    def report(label, ok, detail):
        print(f"[{'PASS' if ok else 'FAIL'}] {label}: {detail}"); return ok

    ok = True
    # H1: keep beats flip for GP and RF (combination dedup), mixed benchmarks
    for o in ("func2C", "func3C"):
        for s in ("gp", "rf"):
            k = sel(objective=o, surrogate=s, generator="keep", dedup="combination")
            f = sel(objective=o, surrogate=s, generator="flip", dedup="combination")
            w_, p = int(np.sum(k < f)), wilcoxon(k, f).pvalue if np.any(k != f) else 1.0
            ok &= report(f"H1 {o}/{s}", w_ >= 17 and p < 0.05,
                         f"keep wins {w_}/25, p={p:.2e}, means {k.mean():.4f} vs {f.mean():.4f}")
    # H2: dedup cost on d3L5 (keep generator)
    for s in ("gp", "rf"):
        enc_r = sel(objective="cat_ackley_d3_L5", surrogate=s, generator="keep", dedup="encoding", col="revisits")
        com_r = sel(objective="cat_ackley_d3_L5", surrogate=s, generator="keep", dedup="combination", col="revisits")
        enc_f = sel(objective="cat_ackley_d3_L5", surrogate=s, generator="keep", dedup="encoding")
        com_f = sel(objective="cat_ackley_d3_L5", surrogate=s, generator="keep", dedup="combination")
        ok &= report(f"H2 {s} dedup cost",
                     np.median(enc_r) >= 10 and np.median(com_r) == 0
                     and np.median(com_f - enc_f) <= 0,
                     f"revisits enc median {np.median(enc_r):.0f} (mean {enc_r.mean():.1f}) vs comb {np.median(com_r):.0f}; "
                     f"final enc {enc_f.mean():.3f} vs comb {com_f.mean():.3f}")
    # H3: standing vs Random moves with machinery
    for o in ("func2C", "func3C"):
        rnd = sel(objective=o, surrogate="random")
        for s in ("gp", "rf"):
            best_cell = sel(objective=o, surrogate=s, generator="keep", dedup="combination")
            worst_cell = sel(objective=o, surrogate=s, generator="flip", dedup="encoding")
            wb, ww = int(np.sum(best_cell < rnd)), int(np.sum(worst_cell < rnd))
            ok &= report(f"H3 {o}/{s}", wb >= ww,
                         f"wins vs Random: keep+comb {wb}/25 >= flip+enc {ww}/25")
    # H4: TPE anchor
    for o in ("func2C", "func3C"):
        t = sel(objective=o, surrogate="tpe")
        rnd = sel(objective=o, surrogate="random")
        w_ = int(np.sum(t < rnd))
        ok &= report(f"H4 TPE {o}", w_ >= 17, f"TPE beats Random {w_}/25")
    t_rev = sel(objective="cat_ackley_d3_L5", surrogate="tpe", col="revisits")
    print(f"[INFO] TPE d3L5 combo revisits: median {np.median(t_rev):.0f}, mean {t_rev.mean():.1f} of 80 (exploratory)")

    print("\nE3 OVERALL:", "PASS" if ok else "SEE FAILURES ABOVE")
