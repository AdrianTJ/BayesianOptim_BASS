# H1 aggregate (1050 runs)


## cat_ackley_d3_L5 (pigeonhole ≈ 20.7/80)

| library | n | revisits med (mean) | excess med | best med | solved |
|---|---|---|---|---|---|
| random | 25 | 22 (21.7) | +1.3 | 4.441e-16 | 14/25 |
| optuna-tpe | 25 | 45 (45.2) | +24.3 | 4.441e-16 | 21/25 |
| hyperopt-tpe | 25 | 33 (33.1) | +12.3 | 4.441e-16 | 21/25 |
| optuna-gp | 25 | 53 (53.0) | +32.3 | 4.441e-16 | 25/25 |
| skopt-gp | 25 | 20 (19.8) | -0.7 | 4.441e-16 | 25/25 |
| ax | 25 | 0 (0.0) | -20.7 | 4.441e-16 | 25/25 |
| smac | 25 | 0 (0.0) | -20.7 | 4.441e-16 | 22/25 |

## cat_ackley_d5_L5 (pigeonhole ≈ 1.0/80)

| library | n | revisits med (mean) | excess med | best med | solved |
|---|---|---|---|---|---|
| random | 25 | 1 (1.4) | -0.0 | 18.85 | 3/25 |
| optuna-tpe | 25 | 29 (29.6) | +28.0 | 16.18 | 7/25 |
| hyperopt-tpe | 25 | 14 (14.3) | +13.0 | 16.18 | 3/25 |
| optuna-gp | 25 | 38 (38.0) | +37.0 | 4.441e-16 | 25/25 |
| skopt-gp | 25 | 1 (0.9) | -0.0 | 4.441e-16 | 25/25 |
| ax | 25 | 0 (0.0) | -1.0 | 4.441e-16 | 18/25 |
| smac | 25 | 0 (0.0) | -1.0 | 16.18 | 4/25 |

## cat_ackley_d6_L11 (pigeonhole ≈ 0.0/80)

| library | n | revisits med (mean) | excess med | best med | solved |
|---|---|---|---|---|---|
| random | 25 | 0 (0.0) | -0.0 | 18.79 | n/a |
| optuna-tpe | 25 | 18 (18.8) | +18.0 | 17.71 | n/a |
| hyperopt-tpe | 25 | 6 (6.2) | +6.0 | 17.71 | n/a |
| optuna-gp | 25 | 1 (7.5) | +1.0 | 4.441e-16 | n/a |
| skopt-gp | 25 | 0 (0.0) | -0.0 | 13.77 | n/a |
| ax | 25 | 0 (0.0) | -0.0 | 15.95 | n/a |
| smac | 25 | 0 (0.0) | -0.0 | 15.95 | n/a |

## pest_control (pigeonhole ≈ 0.0/80)

| library | n | revisits med (mean) | excess med | best med | solved |
|---|---|---|---|---|---|
| random | 25 | 0 (0.0) | -0.0 | 16.25 | n/a |
| optuna-tpe | 25 | 16 (16.0) | +16.0 | 15.47 | n/a |
| hyperopt-tpe | 25 | 7 (6.8) | +7.0 | 15.64 | n/a |
| optuna-gp | 25 | 0 (0.0) | -0.0 | 13.49 | n/a |
| skopt-gp | 25 | 0 (0.0) | -0.0 | 16 | n/a |
| ax | 25 | 0 (0.0) | -0.0 | 14.08 | n/a |
| smac | 25 | 0 (0.0) | -0.0 | 14.93 | n/a |

## func2C (pigeonhole ≈ 0.0/80)

| library | n | revisits med (mean) | excess med | best med | solved |
|---|---|---|---|---|---|
| random | 25 | 0 (0.0) | +0.0 | 0.004216 | 1/25 |
| optuna-tpe | 25 | 0 (0.0) | +0.0 | -0.03358 | 0/25 |
| hyperopt-tpe | 25 | 0 (0.0) | +0.0 | -0.00133 | 0/25 |
| optuna-gp | 25 | 0 (0.0) | +0.0 | -0.1781 | 1/25 |
| skopt-gp | 25 | 0 (0.0) | +0.0 | -0.01938 | 7/25 |
| ax | 25 | 0 (0.0) | +0.0 | -0.1805 | 3/25 |
| smac | 25 | 0 (0.0) | +0.0 | -1.39e-05 | 0/25 |

## func3C (pigeonhole ≈ 0.0/80)

| library | n | revisits med (mean) | excess med | best med | solved |
|---|---|---|---|---|---|
| random | 25 | 0 (0.0) | +0.0 | 0.007182 | 0/25 |
| optuna-tpe | 25 | 0 (0.0) | +0.0 | -0.3189 | 1/25 |
| hyperopt-tpe | 25 | 0 (0.0) | +0.0 | -0.1727 | 0/25 |
| optuna-gp | 25 | 0 (0.0) | +0.0 | -0.4402 | 0/25 |
| skopt-gp | 25 | 0 (0.0) | +0.0 | 0.004768 | 1/25 |
| ax | 25 | 0 (0.0) | +0.0 | -0.7009 | 1/25 |
| smac | 25 | 0 (0.0) | +0.0 | -0.105 | 0/25 |

## Pre-registered hypotheses, evaluated by the letter

- **P1** optuna-tpe: d3_L5 excess 24.25786780126146 (> 10?) ; excess > 0 on all pure-cat: True → PASS
- **P2** hyperopt-tpe d3_L5 excess 12.257867801261462 (> 5?) → PASS
- **P3** skopt-gp |excess| 0.7421321987385383 (≤ 5?) and pest revisits 0.0 (≈0?) → PASS  [≈0 operationalized as median ≤ 1]
- **P4** smac excess ≤ 5 on all pure-cat: violations none → PASS
- **P5** (expl.) ax d3_L5 excess -20.74213219873854 (≤ 5?) → PASS
- **P6** (expl.) optuna-gp d3_L5 excess 32.25786780126146 (> 0?) → PASS
- **P7** mixed-space ≈0 exact revisits for every library (median ≤ 1): violations none → PASS

## Headline candidates (X of N libraries, ≥Y% waste)

- excess ≥ 10% of budget on ≥1 pure-cat benchmark: 3/6 (optuna-tpe, hyperopt-tpe, optuna-gp)
- excess ≥ 25% of budget on ≥1 pure-cat benchmark: 2/6 (optuna-tpe, optuna-gp)
- excess ≥ 40% of budget on ≥1 pure-cat benchmark: 1/6 (optuna-gp)

(distinct failed run attempts in failures.log: 25, all superseded by valid re-runs; cells with n<20 are flagged in the tables above)
