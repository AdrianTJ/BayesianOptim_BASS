# H2 — Machinery-controlled re-comparison (pre-registered design)

**Committed before any full run.** Question: once duplicate handling is
equalized across libraries, do the apparent method rankings change — the
paper's Z-of-W "changes conclusions" number.

## The control: memoization at the objective

`MemoizedAuditedObjective` serves the cached value for any
already-evaluated combination key at **zero objective-budget cost**; only
unique evaluations are charged. Run each library exactly as shipped
(same seeds 3001–3025, same drivers as H1) with a proposal horizon of
**400 asks**, stopping when **80 unique evaluations** are spent (or the
horizon is exhausted — reported, never hidden).

Why this is the right control: our objectives are deterministic, so a
re-evaluated duplicate returns the identical value the cache would serve.
The sampler's state trajectory is therefore **bit-identical to
as-shipped** for the same seed — memoization changes only what the budget
is charged for. H2 = "equal unique-evaluation budget" vs H1's "equal
proposal budget". No counterfactual sampler behavior is introduced; this
is exactly the cache a careful practitioner deploys, applied uniformly.

If a library cannot produce 80 unique proposals within 400 asks, the run
reports unique-at-horizon — itself a machinery measurement ("even with
free duplicates, the generator cannot cover the budget").

## Matrix

Libraries re-run under memoization: **optuna-tpe, optuna-gp,
hyperopt-tpe** (the three with H1 waste). Benchmarks: cat_ackley_d3_L5,
cat_ackley_d5_L5, cat_ackley_d6_L11, pest_control (where H1 waste lived;
pest only for the two TPEs — optuna-gp had 0 pest revisits, its H1 rows
carry over, noted in tables). Seeds 3001–3025. **275 runs.**
As-shipped comparators: the H1 rows (same seeds — paired comparison).
ax/smac/skopt-gp/random need no re-run: their H1 proposals were
(essentially) duplicate-free, so memoization is a no-op for them; their
H1 rows enter the H2 ranking table unchanged (stated in every table).

## Pre-registered metrics & hypotheses

Ranking metric, fixed now: libraries ordered by **median final best**
(tie-break: solve count where a solve threshold exists, per H1 DESIGN).
W = 4 benchmarks; **Z = number of benchmarks on which the induced
ranking of the 6 audited libraries differs between as-shipped (H1) and
budget-equalized (H2)** — any pairwise order flip counts as a change.
Secondary, same data: change in solve counts and in median best per
re-run library.

- **Q1:** Z ≥ 1 of 4 — equalizing machinery changes at least one
  benchmark's ranking. (The paper's claim-shape number; if Z = 0 the
  honest headline is "defaults' waste does not change conclusions at
  this budget" — named now, reported either way.)
- **Q2:** each re-run library weakly improves median best on every
  benchmark (a memoized trajectory is a superset of its paired as-shipped
  trajectory, so doing *worse* is impossible absent a harness bug — any
  violation halts analysis for investigation). Strict improvement in
  median best is predicted exactly where H1 median excess was ≥ 10 AND
  the H1 median best was not already the exact optimum: optuna-tpe on
  d3_L5*, d5_L5, d6_L11, pest; optuna-gp on none (its two ≥10-excess
  cells, d3/d5, already sit at the exact optimum — prediction: they stay
  there); hyperopt-tpe on d3_L5*, d5_L5. (*d3 cells marked * have H1
  median best at the exact optimum too — the strict prediction there
  moves to the solve count instead: optuna-tpe 21/25 and hyperopt 21/25
  must rise.)
- **Q3 (gate G5, promised in H1 REVIEW):** scripted duplicate injection
  on a mixed cat+float space: 10 scripted calls containing exactly 3
  true duplicates (identical floats), 2 near-misses at 1e-3 (beyond
  6-decimal rounding: must NOT count) and 1 within-rounding pair
  (differs at 1e-9: MUST count) → expected revisits = 4, unique = 6.
  G5 must pass before the full H2 run is launched.

## Procedure

1. Commit this DESIGN + memo wrapper + G5 script + analyze_h2.py (fixed
   analysis, written before data). 2. Run G5; gate. 3. Smoke (1 seed ×
   optuna-gp × d3/d6 for horizon-time check). 4. Full 275 runs. 5.
   ANALYSIS.md from results only; adversarial worker≠verifier review;
   only then CLAIMS.md.

## Fairness & disclosures

- Drivers identical to H1 except: n_trials/max_evals = 400 horizon and
  the memoized objective; stopping via an AuditStop exception caught in
  the H2 driver wrappers (optuna additionally via study.stop()).
  Non-defaults recorded per run as in H1.
- optuna-gp horizon risk: 400 GP-fitted asks ≈ 5× H1 wall per run —
  capped at 45 min/run (as pest in H1); cap-outs reported.
- The determinism-equivalence argument above holds only for
  deterministic objectives; all four H2 benchmarks are deterministic
  (pest is our disclosed determinized variant).
