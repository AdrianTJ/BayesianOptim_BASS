# E3 — Surrogate × machinery matrix (design, written before running)

**Date:** 2026-08-21 · **Purpose:** Test K5 — does the machinery confound
generalize beyond BASS-vs-GP? Run real surrogate families through the SAME
pool-based loop and machinery cells the oracle used in E2, and measure the
surrogate-level dedup cost (E5 piggyback).

## Arms (per benchmark × seed, shared init)

- **GP-EI** (sklearn GaussianProcessRegressor, Matérn-5/2 + white noise,
  normalize_y, closed-form EI on posterior mean/sd) over the continuous
  relaxation (raw [0,1] coords — the Garrido-Merchán-style baseline
  treatment, as the thesis's GP-BO): × {keep, flip} generator ×
  {combination, encoding} dedup = 4 cells.
- **RF-EI** (sklearn RandomForestRegressor, 100 trees; SMAC-style EI with
  mean/sd across trees): same 4 cells.
- **TPE** (optuna TPESampler, categorical dims as suggest_categorical,
  continuous as suggest_float; its OWN machinery — no pool, no shared
  generator; n_startup_trials = n0, total trials = n0 + budget): 1 arm.
  TPE is deliberately NOT crossed with the machinery cells: bespoke
  machinery is the phenomenon under study; TPE serves as the
  "method-with-own-machinery" reference. Revisits counted from its trial
  history.
- **Random**: 1 arm (combination dedup), the shared baseline.

10 arms × 3 benchmarks (func2C, func3C, cat_ackley d3/L5) × 25 seeds
(1001–1025), budget 80, n_cand 1000, n0 = max(2d+1, 8). RNG seed formula
now includes the benchmark index (E2 review finding):
`default_rng(seed*1000 + bench_ix*100 + arm_ix)`. Parallelized across runs
(ProcessPoolExecutor, 4 cores).

## Pre-registered hypotheses

| # | Hypothesis | Support criterion |
|---|---|---|
| H1 | The generator ceiling binds real surrogates, not just the oracle: keep beats flip for GP-EI and RF-EI on mixed benchmarks (combination dedup) | paired wins ≥ 17/25 and Wilcoxon p < 0.05, per surrogate, on func2C and func3C |
| H2 | The dedup leak costs real surrogates budget on the solvable categorical benchmark | median revisits under encoding dedup ≥ 10/80 for GP-EI and RF-EI on d3/L5 (keep generator); ≈0 under combination; and combination-dedup final ≤ encoding-dedup final in paired median |
| H3 | Machinery moves each surrogate's standing vs Random: wins-vs-Random under keep+combination ≥ under flip+encoding, per surrogate, on both mixed benchmarks | non-strict inequality on paired win counts, all 4 (surrogate × benchmark) cases |
| H4 | Stock TPE beats Random on the mixed benchmarks (thesis anchor) | paired wins vs Random ≥ 17/25 on func2C and func3C; d3/L5 revisits reported (exploratory, no threshold) |

Falsifications re-center K5: if H1 fails for a surrogate, the generator
ceiling is oracle-specific for that family (weaker but still publishable —
the audit's ceiling would then *overestimate* machinery sensitivity); if H2
fails, the dedup cost is surrogate-dependent and R's BASS 25–29/40 needs
that framing.

## Acknowledged deviations & threats

- sklearn GP ≠ GPfit (kernel family, MLE details). K5 is about surrogate
  *families* under shared machinery, not about reproducing thesis GP-BO
  numbers; no thesis-number comparisons will be made from E3.
- TPE's initial design is its own random startup, not the shared LHS —
  another bespoke-machinery fact, disclosed; its "paired" wins vs Random
  share only the seed, not the init.
- GP hyperparameter optimization uses limited restarts (cost); noted, same
  setting across all cells so it cancels within comparisons.
- E2's oracle ceilings bound what any of these arms can achieve per cell;
  E3 does not re-derive them.
