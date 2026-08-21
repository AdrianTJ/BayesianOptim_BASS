# H2 — Analysis (machinery-controlled re-comparison)

*(Rewritten after adversarial review REFUTED the first draft: the
pre-committed analyze_h2.py omitted the solve-count tie-break that
DESIGN.md itself registers, under-counting Z as 2 and inflating d5's flip
list. Amendment 2 fixes ranking_pairs() toward the registered definition;
the corrected reading follows. The review also verified every cell median
and the full per-row weak-improvement property independently.)*

**Date:** 2026-08-21 · **Protocol:** DESIGN.md + Amendment 2 (above); one
pre-run path-bug fix in h2_cell_runner.py caught by the smoke (third
occurrence of the parents[2]/parents[1] class in this loop's tooling).
Raw: `results.jsonl` (275/275), `h2_agg.md` (regenerated). G5 mixed-space
injection gate: **PASS** (revisits 4/4, unique 6/6; independently re-run
by the reviewer).

## The Z-of-W number

**Z = 3 of W = 4 benchmarks change their library ranking** once duplicate
handling is equalized (memoized budget = 80 unique evaluations, same
seeds; the registered metric is median final best with solve-count
tie-break where a threshold exists):

- **cat_ackley_d3_L5 — CHANGED (7 pairwise flips), via the tie-break.**
  All six libraries share the exact-optimum median in both conditions,
  but solve counts move: optuna-tpe rises 21/25 → 25/25, leaving the
  bottom tier and passing hyperopt-tpe and smac; hyperopt-tpe rises to
  22/25, drawing level with smac. The first draft called this benchmark
  "near-saturated, unchanged" — that reading came from the metric bug.
- **cat_ackley_d5_L5 — CHANGED (1 genuine flip: ax ↔ optuna-tpe).**
  optuna-tpe converts its refunded ~29 evaluations into the exact optimum
  (median 16.18 → 4.4e-16, solves 7/25 → 22/25), reversing its order
  against ax (18/25) and closing most of the gap to the GP tier —
  though not all of it (22/25 vs 25/25). Under the registered tie-break
  it already stood above hyperopt/smac in H1, so the first draft's
  "5 flips" overstated the movement.
- **cat_ackley_d6_L11 — CHANGED (1 flip).** hyperopt-tpe (17.71→17.15)
  now beats optuna-tpe (17.71→17.60).
- **pest_control — unchanged.** Waste there (16/80, 7/80) is real but
  smaller than the between-library quality differences.

A benchmark consumer reading the as-shipped tables would mis-rank
libraries on 3 of these 4 tasks. The distortion is concentrated on one
library: optuna-tpe's apparent weakness on small categorical spaces
(21/25 and 7/25 solves) is mostly its duplicate machinery, not its model
— with duplicates refunded it solves 25/25 and 22/25.

## Hypotheses, by the letter

- **Q1 (Z ≥ 1): PASS** — Z = 3 of 4 (corrected upward by review).
- **Q2: weak clause holds at full per-row granularity** — the reviewer
  verified all 275 paired runs individually: H2 best ≤ H1 best + 1e-12
  with zero violations (stronger than the pre-committed per-cell check).
  7 of 8 strict sub-clauses pass (optuna-tpe strict-improves d5/d6/pest,
  its d3 solves rise 21→25; hyperopt d3 solves rise 21→22; optuna-gp
  stays at the exact optimum on d3/d5). **As a composite, Q2 FAILS by
  the letter:** hyperopt-tpe's predicted strict improvement on d5_L5 did
  not materialize (median 16.18→16.18, solves 3→3). Reading: freeing
  hyperopt's 14 wasted evals does not help because its TPE does not find
  better combinations with them — waste is necessary but not sufficient
  for a ranking artifact. Reported as FAIL, not reframed.
- **Q3 (G5): PASS.**

## New machinery finding (feeds H1-MASK)

optuna-gp on d3_L5 exhausts the **entire 400-ask horizon producing a
median of 30 unique configurations** (H1: 27 unique at 80 asks — 5× the
proposals buys ~3 more unique configs), and 55/400 on d5_L5, while on the
huge d6 space it needs only ~85 asks for 80 unique. GPSampler's proposal
generator *saturates* on small categorical spaces: duplicates are its
steady state, not occasional collisions. More budget cannot fix it; only
machinery change can.

## Practical readings

- Equal-proposal budgets (what benchmark tables implicitly charge) and
  equal-unique-evaluation budgets (what expensive objectives actually
  cost) disagree about rankings on 3 of 4 tasks here.
- The refund is heterogeneous: optuna-tpe converts refunded budget into
  solves; hyperopt-tpe mostly does not. An audit must measure both the
  waste and what the waste displaces.

## Threats & limitations

- Memoization equivalence requires deterministic objectives (all four
  are; pest is the disclosed determinized variant). The bit-identical-
  trajectory claim is not directly verifiable from stored summaries; the
  reviewer confirmed the checkable consequences (per-row weak
  improvement; H2 proposal counts exceeding 80 exactly when H1 had
  revisits) hold on all rows.
- Carried-over rows: ax and smac have 0 revisits in all 100 H1 rows and
  optuna-gp 0 in all 25 pest rows (reviewer-verified row-by-row), so
  carrying them is exact. **skopt-gp's carried d3_L5 row is NOT
  duplicate-free** (H1 median 20/80 revisits → ~60 median unique evals,
  not 80): DESIGN's "essentially duplicate-free" premise does not
  literally hold for that one cell. It cannot change the outcome there —
  skopt-gp already solves 25/25 at ~60 unique — but the H2 d3 column
  compares re-run libraries at up to 80 unique against skopt-gp at ~60;
  disclosed here (review MAJOR-3).
- Ranking metric fixed in DESIGN (median best + solve-count tie-break,
  1e-9 tolerance); Z under other metrics (means, solve-rate-primary) was
  not evaluated.
- d3_L5 re-runs hit the 400-ask horizon at 46/30/67 median unique
  (optuna-tpe/optuna-gp/hyperopt) — d3's H2 numbers are horizon-limited;
  the d3 ranking change rests on solve counts achieved within those
  unique budgets, which can only understate the equalized performance.
- Same benchmark-structure caveats as H1 (cat-ackley additivity).

## Ledger impact (pending: entered after this revision's review sign-off)

**H2-ZOFW** (Z=3/4, review-corrected upward), **H2-SAT** (GPSampler
generator saturation), **H2-REFUND** (heterogeneous refund; Q2 composite
FAIL on record).

---

## Auto-generated aggregate (analyze_h2.py, post-Amendment-2)

# H2 aggregate


## cat_ackley_d3_L5 — ranking CHANGED

| library | H1 as-shipped med best (solve) | H2 equalized med best (solve) | n H2 | uniq/prop (H2 med) |
|---|---|---|---|---|
| optuna-tpe | 4.441e-16 (21) | 4.441e-16 (25) | 25 | 46/400 |
| hyperopt-tpe | 4.441e-16 (21) | 4.441e-16 (22) | 25 | 67/400 |
| optuna-gp | 4.441e-16 (25) | 4.441e-16 (25) | 25 | 30/400 |
| skopt-gp | 4.441e-16 (25) | 4.441e-16 (25) | 0 | carried |
| ax | 4.441e-16 (25) | 4.441e-16 (25) | 0 | carried |
| smac | 4.441e-16 (22) | 4.441e-16 (22) | 0 | carried |

Order flips: [('ax', 'optuna-tpe'), ('optuna-gp', 'optuna-tpe'), ('optuna-tpe', 'hyperopt-tpe'), ('optuna-tpe', 'smac'), ('skopt-gp', 'optuna-tpe'), ('smac', 'hyperopt-tpe'), ('smac', 'optuna-tpe')]

## cat_ackley_d5_L5 — ranking CHANGED

| library | H1 as-shipped med best (solve) | H2 equalized med best (solve) | n H2 | uniq/prop (H2 med) |
|---|---|---|---|---|
| optuna-tpe | 16.18 (7) | 4.441e-16 (22) | 25 | 80/209 |
| hyperopt-tpe | 16.18 (3) | 16.18 (3) | 25 | 80/98 |
| optuna-gp | 4.441e-16 (25) | 4.441e-16 (25) | 25 | 55/400 |
| skopt-gp | 4.441e-16 (25) | 4.441e-16 (25) | 0 | carried |
| ax | 4.441e-16 (18) | 4.441e-16 (18) | 0 | carried |
| smac | 16.18 (4) | 16.18 (4) | 0 | carried |

Order flips: [('ax', 'optuna-tpe'), ('optuna-tpe', 'ax')]

## cat_ackley_d6_L11 — ranking CHANGED

| library | H1 as-shipped med best (solve) | H2 equalized med best (solve) | n H2 | uniq/prop (H2 med) |
|---|---|---|---|---|
| optuna-tpe | 17.71 (n/a) | 17.6 (n/a) | 25 | 80/107 |
| hyperopt-tpe | 17.71 (n/a) | 17.15 (n/a) | 25 | 80/86 |
| optuna-gp | 4.441e-16 (n/a) | 4.441e-16 (n/a) | 25 | 80/85 |
| skopt-gp | 13.77 (n/a) | 13.77 (n/a) | 0 | carried |
| ax | 15.95 (n/a) | 15.95 (n/a) | 0 | carried |
| smac | 15.95 (n/a) | 15.95 (n/a) | 0 | carried |

Order flips: [('hyperopt-tpe', 'optuna-tpe')]

## pest_control — ranking unchanged

| library | H1 as-shipped med best (solve) | H2 equalized med best (solve) | n H2 | uniq/prop (H2 med) |
|---|---|---|---|---|
| optuna-tpe | 15.47 (n/a) | 15.3 (n/a) | 25 | 80/107 |
| hyperopt-tpe | 15.64 (n/a) | 15.64 (n/a) | 25 | 80/87 |
| optuna-gp | 13.49 (n/a) | 13.49 (n/a) | 0 | carried |
| skopt-gp | 16 (n/a) | 16 (n/a) | 0 | carried |
| ax | 14.08 (n/a) | 14.08 (n/a) | 0 | carried |
| smac | 14.93 (n/a) | 14.93 (n/a) | 0 | carried |

## Z-of-W

**Z = 3 of W = 4** benchmarks change ranking once budgets are equalized (pairwise strict-order flips, tie tol 1e-09).

- **Q1** (Z ≥ 1): PASS

- **Q2** weak-improvement violations (harness-bug tripwire): none
  - optuna-tpe@cat_ackley_d5_L5: 16.18 -> 4.441e-16 STRICT-IMPROVED
  - optuna-tpe@cat_ackley_d6_L11: 17.71 -> 17.6 STRICT-IMPROVED
  - optuna-tpe@pest_control: 15.47 -> 15.3 STRICT-IMPROVED
  - hyperopt-tpe@cat_ackley_d5_L5: 16.18 -> 16.18 NOT strict
  - optuna-tpe@cat_ackley_d3_L5 solves: 21/25 -> 25/25 ROSE
  - hyperopt-tpe@cat_ackley_d3_L5 solves: 21/25 -> 22/25 ROSE
  - optuna-gp@cat_ackley_d3_L5: stays at exact optimum: YES
  - optuna-gp@cat_ackley_d5_L5: stays at exact optimum: YES

(distinct failed run attempts: 3)
