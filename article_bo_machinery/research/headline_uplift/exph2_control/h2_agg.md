# H2 aggregate


## cat_ackley_d3_L5 — ranking unchanged

| library | H1 as-shipped med best (solve) | H2 equalized med best (solve) | n H2 | uniq/prop (H2 med) |
|---|---|---|---|---|
| optuna-tpe | 4.441e-16 (21) | 4.441e-16 (25) | 25 | 46/400 |
| hyperopt-tpe | 4.441e-16 (21) | 4.441e-16 (22) | 25 | 67/400 |
| optuna-gp | 4.441e-16 (25) | 4.441e-16 (25) | 25 | 30/400 |
| skopt-gp | 4.441e-16 (25) | 4.441e-16 (25) | 0 | carried |
| ax | 4.441e-16 (25) | 4.441e-16 (25) | 0 | carried |
| smac | 4.441e-16 (22) | 4.441e-16 (22) | 0 | carried |

## cat_ackley_d5_L5 — ranking CHANGED

| library | H1 as-shipped med best (solve) | H2 equalized med best (solve) | n H2 | uniq/prop (H2 med) |
|---|---|---|---|---|
| optuna-tpe | 16.18 (7) | 4.441e-16 (22) | 25 | 80/209 |
| hyperopt-tpe | 16.18 (3) | 16.18 (3) | 25 | 80/98 |
| optuna-gp | 4.441e-16 (25) | 4.441e-16 (25) | 25 | 55/400 |
| skopt-gp | 4.441e-16 (25) | 4.441e-16 (25) | 0 | carried |
| ax | 4.441e-16 (18) | 4.441e-16 (18) | 0 | carried |
| smac | 16.18 (4) | 16.18 (4) | 0 | carried |

Order flips: [('ax', 'optuna-tpe'), ('optuna-gp', 'optuna-tpe'), ('optuna-tpe', 'hyperopt-tpe'), ('optuna-tpe', 'smac'), ('skopt-gp', 'optuna-tpe')]

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

**Z = 2 of W = 4** benchmarks change ranking once budgets are equalized (pairwise strict-order flips, tie tol 1e-09).

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
