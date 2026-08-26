# E7 — Guidance dial (design, written before the confirmatory run)

**Date:** 2026-08-22 · **Purpose:** Decide the K10 hypothesis, which E3's
review refuted as confounded: does the generator restriction bind a search
process *in proportion to its guidance quality* (H-regime), or is the
keep-vs-flip effect specific to (near-)omniscient scoring (H-oracle-
specific)? The dial de-confounds by varying guidance quality **within the
identical pool-argmax search type**: acquisition score = −f(candidates) +
σ·ε, ε i.i.d. N(0,1) per candidate/iteration.

Also in this cycle: the **shared-init TPE re-run** (closes K-TPE) — optuna
seeded with the same maximin-LHS initial design via add_trial, exactly as
the thesis's `tpe.R` does.

## Calibration pilot (run before this design was frozen; 5 seeds)

Mean finals for the keep generator: σ=0 is the E2 ceiling; σ=3 begins
degrading; σ=10 ≈ GP-level on func2C/n1000 (−0.082 vs GP −0.105); σ=30–100
≈ Random level on both benchmarks. Frozen σ set: **{0, 1, 3, 10, 30, 100}**.
Named anchor points (chosen from the pilot BEFORE the confirmatory run):
GP-level σ* = 10 for func2C, 30 for func3C; Random-level σ† = 100 for both.

## Protocol (confirmatory)

Arms: {keep, flip} × σ ∈ {0,1,3,10,30,100} × n_cand ∈ {1000, 50} ×
{func2C, func3C}; combination dedup; 25 paired seeds (1001–1025, shared
inits); budget 80; RNG = default_rng(seed*1000 + bench_ix*100 + arm_ix);
noise rng separate per run. 1200 runs. TPE-shared-init: func2C/func3C × 25
seeds, wins computed against E3's Random arm (same seeds, same shared
inits).

## Pre-registered decision rules

Per (benchmark, pool), let W(σ) = paired keep<flip wins /25, G(σ) = mean
paired gap (flip − keep) at budget 80.

| Outcome | Criterion |
|---|---|
| **H-regime supported** | W(σ=0) ≥ 20 with p<0.05 (replicates E2), AND at the named GP-level σ*: W(σ*) ≤ 17 with p ≥ 0.05, AND G decays: G(σ†) < G(0) on both benchmarks at both pools |
| **H-regime refuted** (effect persists without guidance) | at σ*: W(σ*) ≥ 20 with p < 0.05 on both benchmarks at n_cand=50 (the pool size where E2's effect is largest) |
| **Inconclusive** | anything else — reported as such, K10 stays a hypothesis, article uses only the deflationary E3 fallback |

TPE-shared-init: K-TPE closes as "config difference explains it" if wins
vs Random ≥ 17/25 on both benchmarks (matching the thesis's direction);
otherwise the discrepancy deepens (optuna version/params next candidate)
and is reported, not resolved.

## Threats

- σ·ε with per-candidate i.i.d. noise is one guidance-degradation model;
  a *biased* surrogate (systematic error, e.g. over-smoothing) is another
  and is NOT covered — a persisting limitation to state in the article
  regardless of outcome.
- At σ→∞ selection within the pool is uniform, but the pool itself still
  concentrates half its mass near the incumbent, so even σ† arms
  hill-climb weakly; that is a property of the shared machinery, identical
  across generator variants, and does not confound the keep-vs-flip
  contrast.
- Pilot used keep-generator only for calibration; anchors are therefore
  keep-referenced. Acceptable: anchors only name *where* to test, the test
  itself is paired.
