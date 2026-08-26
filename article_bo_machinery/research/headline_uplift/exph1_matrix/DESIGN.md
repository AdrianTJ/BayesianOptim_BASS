# H1 — In-the-wild audit matrix (pre-registered design)

**Committed before any full run** (loop rule). Smoke tests of drivers and
benchmarks at tiny budgets happened before this document (tool bring-up);
no matrix cell had been run at protocol scale when this was written.

## Question

How much evaluation budget do widely-deployed BO libraries, at their
documented defaults, silently re-spend on already-evaluated categorical
combinations — beyond what unavoidable pigeonhole collisions explain — and
does the waste co-vary with each library's documented dedup machinery?

## Matrix

**Libraries (7 in-container + 1 conditional):** optuna-tpe, optuna-gp,
hyperopt-tpe, skopt-gp, ax, smac (isolated venv, subprocess), random
(baseline). HEBO joins iff its install smoke test passes (in flight at
commit time); otherwise it is a logged drop.

**Benchmarks (6):** cat_ackley_d3_L5 (K=125, solvable), cat_ackley_d5_L5
(K=3125), cat_ackley_d6_L11 (K≈1.77e6, unsolvable-in-budget, K7 regime),
pest_control (vendored COMBO, 5^25, deterministic variant — see
bo_audit/benchmarks.py docstring), func2C, func3C (mixed cat+cont).

**Budget 80 evals/run; seeds 3001–3025 (25/cell); ~1050 runs total.**
Per-run wall-clock cap 20 min: a run over cap is killed and logged as a
dropped run (never silently); a cell missing >5/25 runs is reported as
incomplete, not averaged over quietly.

## Metrics (per run, from the AuditedObjective wrapper)

- revisits (combination-level key: exact cat tuple + 6-decimal-rounded
  continuous coords), unique, best value, wall time.
- **Headline metric — excess waste:** revisits − pigeonhole(K, 80), where
  pigeonhole(K, B) = B − K·(1−(1−1/K)^B) is the expected collision count
  for uniform sampling on the K-combination space: d3_L5 → 20.7;
  d5_L5 → 1.0; d6_L11 → 0.002; pest/func2C/func3C → ≈0. Mixed-space
  pigeonhole is 0 (continuous coords make exact repeats measure-zero).
- Solve indicator: cat_ackley best ≤ 1e-9 (exact optimum 0); func2C best
  ≤ −0.206326+1e-3; func3C best ≤ −0.722140+1e-3; pest_control and
  d6_L11 report best-value distributions only (no known/solvable optimum).

## Pre-registered per-library hypotheses (from documented mechanisms)

| ID | Hypothesis | Basis |
|---|---|---|
| P1 | optuna-tpe median excess > 10 on d3_L5, and > 0 on every pure-cat benchmark incl. pest (5^25: ANY revisit is excess) | no dedup at defaults; issues #5440/#2021/#4859 |
| P2 | hyperopt-tpe median excess > 5 on d3_L5 | issue #608 (48/500 identical trials) |
| P3 | skopt-gp median |excess| ≤ 5 on d3_L5 and ≈0 revisits on pest | built-in duplicate detection with uniform-random fallback (H0) |
| P4 | smac median excess ≤ 5 on all pure-cat cells | runhistory-keyed; deterministic=True evaluates a config once |
| P5 (exploratory) | ax median excess ≤ 5 on d3_L5 | Sobol dedup + GP; no admitted-issue paper trail |
| P6 (exploratory) | optuna-gp excess > 0 on d3_L5 | no documented dedup; bring-up smoke showed 1 revisit in 15 trials |
| P7 (metric null check) | on func2C/func3C every library shows ≈0 exact revisits | continuous coords differ per proposal; the metric must not overfire on mixed spaces |

Falsification is symmetric: a hypothesis failing is reported as such (the
"wild may be clean" risk in PLAN.md), and P7 *failing* (nonzero mixed-space
exact revisits) would itself be a finding about degenerate proposal streams.

## Procedure

1. Commit this file. 2. Per-cell timing smoke (1 seed, budget 80) for the
slow libraries (ax, optuna-gp, skopt-gp, smac, hebo?) to validate the cap.
3. Full fan-out via `run_h1.py` (multiprocessing, 4 workers; JSONL append,
resumable by (lib, bench, seed) key; smac cells via venv subprocess).
4. ANALYSIS.md from results.jsonl only (no peeking mid-run into decisions).
5. Adversarial worker≠verifier review before anything touches CLAIMS.md.

## Fairness rules (unchanged from H0)

Documented defaults only; every non-default recorded in the run row
(seed, cosmetic logging/progressbar, smac's deterministic=True + temp
output dir, ax one-trial-per-ask). Framing is structural — mechanisms and
defaults, never "library L is wrong"; where a tracker admits the behavior
we cite the issue, where it doesn't we report measurements only.

## Deviations & environment constraints (declared up front)

- **JAHS-Bench-201 dropped:** requires Python <3.11; container is 3.11.
  Logged drop, not silent.
- **SMAC isolation:** smac 2.4.0 requires sklearn ≥1.6.1 but imports a
  symbol removed by our 1.9 → runs in a pinned venv (sklearn 1.7.2) via
  subprocess; counting happens inside the subprocess with the same
  bo_audit code.
- **YAHPO deferred to H1b (exploratory):** rbv2 spaces are conditional
  (e.g. `degree` exists only for polynomial kernel), so "combination"
  needs active-parameter key semantics and per-library conditional-space
  support differs (skopt: none). Data (rbv2_svm, 20 MB) is downloaded and
  the surrogate evaluates in-container; H1b will pre-register key
  semantics before any run. Named here so the cut is visible.
- **SMAC×YAHPO impossible in-container** (ConfigSpace ≥1.0 vs 0.6.1) —
  recorded for H1b scoping.
- Ax emits is_ordered=True defaults for integer choices (its documented
  default); recorded, not overridden — defaults-as-shipped is the point.
- pest_control is our deterministic variant of COMBO's stochastic
  objective (local seeded RNG); disclosed in benchmarks.py and in any
  table using it.

## Amendment 1 (post-smoke, pre-full-run; nothing beyond seed 3001 existed)

The timing smoke exposed: (a) a path bug in cell_runner.py (machinery
import; every non-pest main-env smoke run failed on import, so no data was
produced by the buggy path — fixed); (b) pest_control (25 categorical
dims) exceeds the 20-min cap for ax and skopt-gp and took 18.6 min for
optuna-gp under 4-way oversubscription. Changes, all before the full run:
per-run cap on pest_control raised to 45 min (other benchmarks stay at
20); subprocess thread pools pinned (OMP/MKL/OPENBLAS=1) so 4 workers on
4 cores don't oversubscribe; orchestrator-side wall-time fallback for smac
rows. Valid seed-3001 rows already in results.jsonl (optuna-gp pest, smac
all 6 benchmarks) are kept — they are protocol runs under the registered
config. Metrics, hypotheses, seeds, budget: unchanged.
