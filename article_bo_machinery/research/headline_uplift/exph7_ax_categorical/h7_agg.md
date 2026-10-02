# H7 aggregation

| benchmark | n | revisits med | excess med | solves nominal | solves ordinal (H1) | best med nominal | best med ordinal | nominal better/worse/equal | Wilcoxon p |
|---|---|---|---|---|---|---|---|---|---|
| cat_ackley_d3_L5 | 25 | 0 | -20.74 | 25/25 | 25/25 | 4.441e-16 | 4.441e-16 | 0/0/25 | n/a |
| cat_ackley_d5_L5 | 25 | 0 | -1.00 | 10/25 | 18/25 | 16.18 | 4.441e-16 | 3/12/10 | 0.0202 |
| cat_ackley_d6_L11 | 25 | 0 | -0.00 | 0/25 | 2/25 | 17.15 | 15.95 | 7/18/0 | 0.00108 |
| pest_control | 25 | 0 | -0.00 | n/a | n/a | 14.08 | 14.08 | 9/2/14 | 0.00661 |
| func2C | 25 | 0 | 0.00 | n/a | n/a | -0.2057 | -0.1805 | 17/8/0 | 0.339 |
| func3C | 25 | 0 | 0.00 | n/a | n/a | 0.009076 | -0.7009 | 7/18/0 | 0.000631 |

## X1 (0 revisits in all 150 runs)

runs: 150, total revisits: 0 -> **PASS**

## X2 (d5 solves < 18/25 ordinal)

nominal 10/25 vs committed ordinal 18/25 -> **PASS**

## X3 (H2 headline pair on cat_ackley_d5_L5, descriptive)

- optuna-tpe memoized (H2): median best 4.441e-16, solves 22/25; nominal Ax median best 16.18, solves 10/25 -> optuna-tpe ahead
- optuna-tpe as shipped (H1): median best 16.18, solves 7/25; nominal Ax median best 16.18, solves 10/25 -> nominal Ax ahead
