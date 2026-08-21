#!/usr/bin/env python3
"""E7 guidance dial + shared-init TPE. Protocol per DESIGN.md."""
import csv
import sys
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np
from scipy.stats import wilcoxon

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from machinery import Config, hybrid_candidates, make_func2C, make_func3C, run_bo, shared_init
from surrogates import run_tpe

SEEDS = list(range(1001, 1026))
BENCH = [("func2C", make_func2C), ("func3C", make_func3C)]
SIGMAS = [0.0, 1.0, 3.0, 10.0, 30.0, 100.0]
POOLS = [1000, 50]
ANCHOR = {"func2C": 10.0, "func3C": 30.0}   # pre-named GP-level sigma*
SIGMA_RAND = 100.0                           # pre-named Random-level sigma†


def job(args):
    kind, bench_ix, seed, arm_ix, payload = args
    bname, make = BENCH[bench_ix]
    obj = make()
    X0, y0 = shared_init(obj, seed)
    if kind == "dial":
        gen, sigma, n_cand = payload
        cfg = Config(budget=80, n_cand=n_cand)
        schema = obj["schema"]
        nrng = np.random.default_rng(seed * 7919 + arm_ix)
        m = {"name": "dial",
             "candidates": lambda X, y, g: hybrid_candidates(X, y, cfg.n_cand, schema, g, variant=gen),
             "acquire": lambda X, y, Xc: -obj["fn"](Xc) + sigma * nrng.standard_normal(len(Xc))}
        r = run_bo(obj, m, cfg, X0, y0,
                   np.random.default_rng(seed * 1000 + bench_ix * 100 + arm_ix))
        return {"objective": bname, "seed": seed, "arm": "dial", "generator": gen,
                "sigma": sigma, "n_cand": n_cand, "best_b80": r["best"][80]}
    else:  # shared-init tpe
        r = run_tpe(obj, 80, seed, init=(X0, y0))
        return {"objective": bname, "seed": seed, "arm": "tpe_shared", "generator": "own",
                "sigma": -1, "n_cand": -1, "best_b80": r["best"][80]}


def all_jobs():
    for bench_ix, _ in enumerate(BENCH):
        for seed in SEEDS:
            arm_ix = 0
            for gen in ("keep", "flip"):
                for sigma in SIGMAS:
                    for n_cand in POOLS:
                        yield ("dial", bench_ix, seed, arm_ix, (gen, sigma, n_cand)); arm_ix += 1
            yield ("tpe", bench_ix, seed, arm_ix, None)


if __name__ == "__main__":
    jobs = list(all_jobs())
    rows = []
    with ProcessPoolExecutor(max_workers=4) as ex:
        for i, row in enumerate(ex.map(job, jobs, chunksize=8)):
            rows.append(row)
            if (i + 1) % 100 == 0:
                print(f"{i+1}/{len(jobs)} done", flush=True)

    with (Path(__file__).parent / "results.csv").open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        w.writeheader(); w.writerows(rows)

    def sel(**kw):
        return np.array([r["best_b80"] for r in rows
                         if all(r[k] == v for k, v in kw.items())])

    print("\n== dial table: wins W(sigma) [keep<flip] and gap G(sigma)=mean(flip-keep) ==")
    stats = {}
    for bname, _ in BENCH:
        for n_cand in POOLS:
            line = []
            for sigma in SIGMAS:
                k = sel(objective=bname, generator="keep", sigma=sigma, n_cand=n_cand)
                f = sel(objective=bname, generator="flip", sigma=sigma, n_cand=n_cand)
                w_ = int(np.sum(k < f))
                p = wilcoxon(k, f).pvalue if np.any(k != f) else 1.0
                stats[(bname, n_cand, sigma)] = (w_, p, (f - k).mean())
                line.append(f"s{sigma:g}: W={w_:>2} p={p:.1e} G={(f-k).mean():+.4f}")
            print(f"{bname} n={n_cand}: " + " | ".join(line))

    print("\n== pre-registered decision ==")
    support = refute = True
    for bname, _ in BENCH:
        s_star = ANCHOR[bname]
        for n_cand in POOLS:
            w0, p0, g0 = stats[(bname, n_cand, 0.0)]
            ws, ps, _ = stats[(bname, n_cand, s_star)]
            _, _, gr = stats[(bname, n_cand, SIGMA_RAND)]
            support &= (w0 >= 20 and p0 < 0.05) and (ws <= 17 and ps >= 0.05) and (gr < g0)
        wsl, psl, _ = stats[(bname, 50, s_star)]
        refute &= (wsl >= 20 and psl < 0.05)
    verdict = ("H-REGIME SUPPORTED" if support else
               "H-REGIME REFUTED" if refute else "INCONCLUSIVE")
    print("verdict:", verdict)

    print("\n== shared-init TPE vs E3 Random (same seeds/inits) ==")
    e3 = list(csv.DictReader(open(Path(__file__).parent.parent / "exp03_surrogate_matrix" / "results.csv")))
    tpe_wins = {}
    for bname, _ in BENCH:
        rnd = {float(r["seed"]): float(r["best_b80"]) for r in e3
               if r["objective"] == bname and r["surrogate"] == "random"}
        t = {r["seed"]: r["best_b80"] for r in rows
             if r["objective"] == bname and r["arm"] == "tpe_shared"}
        tpe_wins[bname] = sum(t[s] < rnd[s] for s in t)
        tm = np.mean(list(t.values())); rm = np.mean(list(rnd.values()))
        print(f"{bname}: TPE_shared wins {tpe_wins[bname]}/25 vs Random | means {tm:.4f} vs {rm:.4f}")
    # DESIGN.md's criterion is JOINT: >=17/25 on BOTH benchmarks (review fix)
    closed = all(w >= 17 for w in tpe_wins.values())
    print("K-TPE pre-registered joint criterion:",
          "MET -> closes" if closed else "NOT MET -> stays open (fallback branch)")
