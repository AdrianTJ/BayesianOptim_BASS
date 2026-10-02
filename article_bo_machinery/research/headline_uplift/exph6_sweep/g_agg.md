# G-sweep aggregate (16275 runs)

## Median e(80) per arm x class (per-class median of per-benchmark medians)

| arm | A | B | C | D | E | F |
|---|---|---|---|---|---|---|
| random | -0.000 | -0.004 | -0.000 | -0.000 | +0.000 | +0.000 |
| optuna-tpe | +0.303 | +0.294 | +0.462 | +0.337 | +0.000 | +0.000 |
| optuna-tpe-3.6 | +0.321 | +0.307 | +0.462 | +0.337 | +0.000 | +0.000 |
| hyperopt-tpe | +0.159 | +0.119 | +0.244 | +0.156 | +0.000 | +0.000 |
| smac | -0.013 | -0.006 | -0.000 | -0.000 | — | +0.000 |
| optuna-gp | +0.403 | +0.432 | +0.012 | +0.006 | +0.003 | +0.000 |
| skopt-gp | -0.009 | -0.006 | -0.000 | -0.000 | +0.000 | +0.000 |
| ax | -0.013 | -0.006 | -0.000 | -0.000 | +0.000 | +0.000 |

## Median e(B) per arm x benchmark x budget

| benchmark | arm | B20 | B40 | B80 | B160 | solves@80 | med best@80 |
|---|---|---|---|---|---|---|---|
| cat_ackley_d3_L5 | random | +0.028 | -0.016 | +0.003 | +0.003 | 12 | 18.18 |
| cat_ackley_d3_L5 | optuna-tpe | +0.178 | +0.234 | +0.303 | +0.303 | 24 | 4.441e-16 |
| cat_ackley_d3_L5 | optuna-tpe-3.6 | +0.178 | +0.259 | +0.303 | +0.315 | 24 | 4.441e-16 |
| cat_ackley_d3_L5 | hyperopt-tpe | -0.022 | +0.134 | +0.166 | +0.203 | 22 | 4.441e-16 |
| cat_ackley_d3_L5 | smac | -0.072 | -0.141 | -0.259 | -0.435 | 24 | 4.441e-16 |
| cat_ackley_d3_L5 | optuna-gp | +0.028 | +0.259 | +0.403 | — | 25 | 4.441e-16 |
| cat_ackley_d3_L5 | skopt-gp | -0.072 | -0.041 | -0.009 | — | 25 | 4.441e-16 |
| cat_ackley_d3_L5 | ax | -0.072 | -0.141 | -0.259 | — | 25 | 4.441e-16 |
| cat_ackley_d5_L5 | random | -0.003 | -0.006 | -0.000 | -0.000 | 1 | 18.85 |
| cat_ackley_d5_L5 | optuna-tpe | +0.147 | +0.244 | +0.362 | +0.519 | 8 | 16.18 |
| cat_ackley_d5_L5 | optuna-tpe-3.6 | +0.147 | +0.269 | +0.362 | +0.525 | 10 | 16.18 |
| cat_ackley_d5_L5 | hyperopt-tpe | -0.003 | +0.144 | +0.187 | +0.244 | 3 | 16.18 |
| cat_ackley_d5_L5 | smac | -0.003 | -0.006 | -0.013 | -0.025 | 1 | 16.18 |
| cat_ackley_d5_L5 | optuna-gp | +0.047 | +0.194 | +0.462 | +0.669 | 25 | 4.441e-16 |
| cat_ackley_d5_L5 | skopt-gp | -0.003 | -0.006 | -0.013 | -0.006 | 25 | 4.441e-16 |
| cat_ackley_d5_L5 | ax | -0.003 | -0.006 | -0.013 | -0.025 | 16 | 4.441e-16 |
| cat_ackley_d6_L11 | random | -0.000 | -0.000 | -0.000 | -0.000 | n/a | 19.04 |
| cat_ackley_d6_L11 | optuna-tpe | +0.100 | +0.150 | +0.237 | +0.319 | n/a | 16.87 |
| cat_ackley_d6_L11 | optuna-tpe-3.6 | +0.100 | +0.175 | +0.237 | +0.337 | n/a | 16.87 |
| cat_ackley_d6_L11 | hyperopt-tpe | -0.000 | +0.025 | +0.075 | +0.100 | n/a | 17.71 |
| cat_ackley_d6_L11 | smac | -0.000 | -0.000 | -0.000 | -0.000 | n/a | 16.87 |
| cat_ackley_d6_L11 | optuna-gp | +0.050 | +0.025 | +0.075 | — | n/a | 4.441e-16 |
| cat_ackley_d6_L11 | skopt-gp | -0.000 | -0.000 | -0.000 | — | n/a | 13.77 |
| cat_ackley_d6_L11 | ax | -0.000 | -0.000 | -0.000 | — | n/a | 17.15 |
| catf_rastrigin_d4L7 | random | -0.004 | -0.008 | -0.004 | -0.001 | 0 | 28.92 |
| catf_rastrigin_d4L7 | optuna-tpe | +0.096 | +0.242 | +0.346 | +0.474 | 6 | 15.6 |
| catf_rastrigin_d4L7 | optuna-tpe-3.6 | +0.096 | +0.242 | +0.321 | +0.468 | 8 | 15.6 |
| catf_rastrigin_d4L7 | hyperopt-tpe | -0.004 | +0.092 | +0.159 | +0.199 | 8 | 15.6 |
| catf_rastrigin_d4L7 | smac | -0.004 | -0.008 | -0.016 | -0.032 | 6 | 15.6 |
| catf_rastrigin_d4L7 | optuna-gp | +0.046 | +0.192 | +0.584 | — | 25 | 0 |
| catf_rastrigin_d4L7 | skopt-gp | -0.004 | -0.008 | -0.016 | — | 25 | 0 |
| catf_rastrigin_d4L7 | ax | -0.004 | -0.008 | -0.016 | — | 23 | 0 |
| catf_griewank_d5L7 | random | -0.001 | -0.001 | -0.002 | -0.005 | 0 | 40.97 |
| catf_griewank_d5L7 | optuna-tpe | +0.099 | +0.199 | +0.298 | +0.458 | 2 | 20.92 |
| catf_griewank_d5L7 | optuna-tpe-3.6 | +0.099 | +0.199 | +0.335 | +0.445 | 0 | 21.07 |
| catf_griewank_d5L7 | hyperopt-tpe | -0.001 | +0.074 | +0.135 | +0.177 | 1 | 21.09 |
| catf_griewank_d5L7 | smac | -0.001 | -0.001 | -0.002 | -0.005 | 1 | 21.35 |
| catf_griewank_d5L7 | optuna-gp | -0.001 | -0.001 | +0.110 | — | 25 | 0 |
| catf_griewank_d5L7 | skopt-gp | -0.001 | -0.001 | -0.002 | — | 25 | 0 |
| catf_griewank_d5L7 | ax | -0.001 | -0.001 | -0.002 | — | 25 | 0 |
| catf_rosen_d4L7 | random | -0.004 | -0.008 | -0.004 | -0.001 | 1 | 33.5 |
| catf_rosen_d4L7 | optuna-tpe | +0.096 | +0.267 | +0.359 | +0.486 | 0 | 28 |
| catf_rosen_d4L7 | optuna-tpe-3.6 | +0.146 | +0.242 | +0.359 | +0.486 | 1 | 21.5 |
| catf_rosen_d4L7 | hyperopt-tpe | -0.004 | +0.092 | +0.146 | +0.236 | 1 | 21.5 |
| catf_rosen_d4L7 | smac | -0.004 | -0.008 | -0.016 | -0.032 | 4 | 16 |
| catf_rosen_d4L7 | optuna-gp | -0.004 | -0.008 | -0.016 | -0.026 | 9 | 3 |
| catf_rosen_d4L7 | skopt-gp | -0.004 | -0.008 | -0.016 | -0.020 | 6 | 3 |
| catf_rosen_d4L7 | ax | -0.004 | -0.008 | -0.016 | -0.032 | 2 | 4 |
| catf_michal_d5L9 | random | -0.000 | -0.000 | -0.001 | -0.001 | 0 | -1.827 |
| catf_michal_d5L9 | optuna-tpe | +0.050 | +0.175 | +0.224 | +0.361 | 0 | -2.307 |
| catf_michal_d5L9 | optuna-tpe-3.6 | +0.100 | +0.200 | +0.262 | +0.411 | 1 | -2.198 |
| catf_michal_d5L9 | hyperopt-tpe | -0.000 | +0.075 | +0.099 | +0.111 | 0 | -2.214 |
| catf_michal_d5L9 | smac | -0.000 | -0.000 | -0.001 | -0.001 | 5 | -2.41 |
| catf_michal_d5L9 | optuna-gp | -0.000 | +0.025 | +0.437 | +0.705 | 25 | -2.868 |
| catf_michal_d5L9 | skopt-gp | -0.000 | -0.000 | -0.001 | -0.001 | 25 | -2.868 |
| catf_michal_d5L9 | ax | -0.000 | -0.000 | -0.001 | -0.001 | 25 | -2.868 |
| catf_schwefel_d4L9 | random | -0.001 | -0.003 | -0.006 | +0.000 | 1 | 364.6 |
| catf_schwefel_d4L9 | optuna-tpe | +0.099 | +0.197 | +0.294 | +0.419 | 4 | 238.4 |
| catf_schwefel_d4L9 | optuna-tpe-3.6 | +0.099 | +0.172 | +0.307 | +0.425 | 6 | 238.4 |
| catf_schwefel_d4L9 | hyperopt-tpe | -0.001 | +0.072 | +0.119 | +0.157 | 3 | 238.4 |
| catf_schwefel_d4L9 | smac | -0.001 | -0.003 | -0.006 | -0.012 | 2 | 238.4 |
| catf_schwefel_d4L9 | optuna-gp | +0.049 | +0.022 | +0.432 | — | 25 | 5.091e-05 |
| catf_schwefel_d4L9 | skopt-gp | -0.001 | -0.003 | -0.006 | — | 25 | 5.091e-05 |
| catf_schwefel_d4L9 | ax | -0.001 | -0.003 | -0.006 | — | 19 | 5.091e-05 |
| nk_n20k2 | random | -0.000 | -0.000 | -0.000 | -0.000 | 0 | -0.6271 |
| nk_n20k2 | optuna-tpe | +0.250 | +0.400 | +0.462 | +0.606 | 0 | -0.6288 |
| nk_n20k2 | optuna-tpe-3.6 | +0.250 | +0.425 | +0.487 | +0.600 | 0 | -0.6223 |
| nk_n20k2 | hyperopt-tpe | -0.000 | +0.150 | +0.237 | +0.375 | 1 | -0.6508 |
| nk_n20k2 | smac | -0.000 | -0.000 | -0.000 | -0.000 | 0 | -0.6676 |
| nk_n20k2 | optuna-gp | -0.000 | -0.000 | +0.100 | +0.406 | 6 | -0.6918 |
| nk_n20k2 | skopt-gp | -0.000 | -0.000 | -0.000 | -0.000 | 0 | -0.6711 |
| nk_n20k2 | ax | -0.000 | -0.000 | -0.000 | -0.000 | 2 | -0.6916 |
| nk_n20k8 | random | -0.000 | -0.000 | -0.000 | -0.000 | 0 | -0.651 |
| nk_n20k8 | optuna-tpe | +0.100 | +0.325 | +0.462 | +0.606 | 0 | -0.6565 |
| nk_n20k8 | optuna-tpe-3.6 | +0.050 | +0.325 | +0.450 | +0.600 | 0 | -0.6565 |
| nk_n20k8 | hyperopt-tpe | -0.000 | +0.150 | +0.275 | +0.387 | 0 | -0.6683 |
| nk_n20k8 | smac | -0.000 | -0.000 | -0.000 | -0.000 | 0 | -0.6596 |
| nk_n20k8 | optuna-gp | -0.000 | +0.025 | +0.012 | — | 0 | -0.7135 |
| nk_n20k8 | skopt-gp | -0.000 | -0.000 | -0.000 | — | 0 | -0.6751 |
| nk_n20k8 | ax | -0.000 | -0.000 | -0.000 | — | 0 | -0.7204 |
| maxcut_n20 | random | -0.000 | -0.000 | -0.000 | -0.000 | 0 | -30.19 |
| maxcut_n20 | optuna-tpe | +0.100 | +0.375 | +0.475 | +0.606 | 0 | -30.77 |
| maxcut_n20 | optuna-tpe-3.6 | +0.100 | +0.400 | +0.475 | +0.612 | 0 | -30.64 |
| maxcut_n20 | hyperopt-tpe | -0.000 | +0.175 | +0.250 | +0.369 | 0 | -31.5 |
| maxcut_n20 | smac | -0.000 | -0.000 | -0.000 | -0.000 | 0 | -31.78 |
| maxcut_n20 | optuna-gp | -0.000 | -0.000 | +0.012 | — | 6 | -34.27 |
| maxcut_n20 | skopt-gp | -0.000 | -0.000 | -0.000 | — | 0 | -33.28 |
| maxcut_n20 | ax | -0.000 | -0.000 | -0.000 | — | 6 | -34.37 |
| labs_n25 | random | -0.000 | -0.000 | -0.000 | -0.000 | n/a | 0.1984 |
| labs_n25 | optuna-tpe | +0.050 | +0.275 | +0.400 | +0.537 | n/a | 0.1728 |
| labs_n25 | optuna-tpe-3.6 | -0.000 | +0.300 | +0.412 | +0.562 | n/a | 0.192 |
| labs_n25 | hyperopt-tpe | -0.000 | +0.025 | +0.162 | +0.294 | n/a | 0.192 |
| labs_n25 | smac | -0.000 | -0.000 | -0.000 | -0.000 | n/a | 0.1792 |
| labs_n25 | optuna-gp | -0.000 | -0.000 | -0.000 | — | n/a | 0.1536 |
| labs_n25 | skopt-gp | -0.000 | -0.000 | -0.000 | — | n/a | 0.1984 |
| labs_n25 | ax | -0.000 | -0.000 | -0.000 | — | n/a | 0.1472 |
| pest_control | random | +0.000 | +0.000 | -0.000 | -0.000 | n/a | 16.3 |
| pest_control | optuna-tpe | +0.000 | +0.125 | +0.200 | +0.287 | n/a | 15.56 |
| pest_control | optuna-tpe-3.6 | +0.000 | +0.125 | +0.200 | +0.269 | n/a | 15.52 |
| pest_control | hyperopt-tpe | +0.000 | +0.000 | +0.087 | +0.106 | n/a | 15.58 |
| pest_control | smac | +0.000 | +0.000 | -0.000 | -0.000 | n/a | 14.65 |
| pest_control | optuna-gp | +0.000 | +0.000 | -0.000 | -0.000 | n/a | 13.4 |
| pest_control | skopt-gp | +0.000 | +0.000 | -0.000 | -0.000 | n/a | 15.92 |
| pest_control | ax | +0.000 | +0.000 | -0.000 | -0.000 | n/a | 14.08 |
| contam_2p25 | random | -0.000 | -0.000 | -0.000 | -0.000 | n/a | 1.145 |
| contam_2p25 | optuna-tpe | +0.250 | +0.400 | +0.475 | +0.600 | n/a | 1.099 |
| contam_2p25 | optuna-tpe-3.6 | +0.250 | +0.425 | +0.475 | +0.587 | n/a | 1.101 |
| contam_2p25 | hyperopt-tpe | -0.000 | +0.150 | +0.225 | +0.287 | n/a | 1.076 |
| contam_2p25 | smac | -0.000 | -0.000 | -0.000 | -0.000 | n/a | 1.033 |
| contam_2p25 | optuna-gp | -0.000 | -0.000 | +0.012 | — | n/a | 0.9732 |
| contam_2p25 | skopt-gp | -0.000 | -0.000 | -0.000 | — | n/a | 1.02 |
| contam_2p25 | ax | -0.000 | -0.000 | -0.000 | — | n/a | 0.974 |
| ml_rf_digits | random | -0.002 | -0.005 | +0.003 | +0.000 | n/a | 0.02393 |
| ml_rf_digits | optuna-tpe | -0.002 | +0.070 | +0.153 | +0.357 | n/a | 0.02282 |
| ml_rf_digits | optuna-tpe-3.6 | +0.048 | +0.070 | +0.153 | +0.332 | n/a | 0.02282 |
| ml_rf_digits | hyperopt-tpe | -0.002 | +0.020 | +0.041 | +0.063 | n/a | 0.02337 |
| ml_rf_digits | optuna-gp | -0.002 | -0.005 | +0.003 | — | n/a | 0.02337 |
| ml_rf_digits | skopt-gp | -0.002 | -0.005 | -0.009 | — | n/a | 0.02337 |
| ml_rf_digits | ax | -0.002 | -0.005 | -0.009 | — | n/a | 0.02337 |
| ml_svm_digits | random | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.007791 |
| ml_svm_digits | optuna-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.007234 |
| ml_svm_digits | optuna-tpe-3.6 | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.007234 |
| ml_svm_digits | hyperopt-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.007234 |
| ml_svm_digits | optuna-gp | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.007234 |
| ml_svm_digits | skopt-gp | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.007791 |
| ml_svm_digits | ax | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.007234 |
| ml_gb_bc | random | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.03335 |
| ml_gb_bc | optuna-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.02985 |
| ml_gb_bc | optuna-tpe-3.6 | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.0316 |
| ml_gb_bc | hyperopt-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.03161 |
| ml_gb_bc | optuna-gp | +0.000 | +0.000 | +0.013 | — | n/a | 0.02985 |
| ml_gb_bc | skopt-gp | +0.000 | +0.000 | +0.000 | — | n/a | 0.02985 |
| ml_gb_bc | ax | +0.000 | +0.000 | +0.000 | — | n/a | 0.02985 |
| ml_mlp_wine | random | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.005556 |
| ml_mlp_wine | optuna-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.005556 |
| ml_mlp_wine | optuna-tpe-3.6 | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.005556 |
| ml_mlp_wine | hyperopt-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.005556 |
| ml_mlp_wine | optuna-gp | +0.000 | +0.000 | +0.000 | — | n/a | 0.005556 |
| ml_mlp_wine | skopt-gp | +0.000 | +0.000 | +0.000 | — | n/a | 0.005556 |
| ml_mlp_wine | ax | +0.000 | +0.000 | +0.000 | — | n/a | 0.005556 |
| yahpo_rpart_41138 | random | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.09574 |
| yahpo_rpart_41138 | optuna-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.09037 |
| yahpo_rpart_41138 | hyperopt-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.09138 |
| yahpo_rpart_41138 | optuna-gp | +0.000 | +0.000 | +0.013 | +0.056 | n/a | 0.08226 |
| yahpo_rpart_41138 | skopt-gp | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.08226 |
| yahpo_rpart_41138 | ax | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.08138 |
| yahpo_rpart_40981 | random | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.3152 |
| yahpo_rpart_40981 | optuna-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.2985 |
| yahpo_rpart_40981 | hyperopt-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.3046 |
| yahpo_rpart_40981 | optuna-gp | +0.000 | +0.050 | +0.450 | — | n/a | 0.29 |
| yahpo_rpart_40981 | skopt-gp | +0.000 | +0.000 | +0.000 | — | n/a | 0.29 |
| yahpo_rpart_40981 | ax | +0.000 | +0.000 | +0.000 | — | n/a | 0.29 |
| yahpo_ranger_1489 | random | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.2332 |
| yahpo_ranger_1489 | optuna-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.2167 |
| yahpo_ranger_1489 | hyperopt-tpe | +0.000 | +0.000 | +0.000 | +0.000 | n/a | 0.2231 |
| yahpo_ranger_1489 | optuna-gp | +0.000 | +0.000 | +0.000 | — | n/a | 0.1975 |
| yahpo_ranger_1489 | skopt-gp | +0.000 | +0.000 | +0.000 | — | n/a | 0.2054 |
| yahpo_ranger_1489 | ax | +0.000 | +0.000 | +0.000 | — | n/a | 0.1974 |
| func2C | random | +0.000 | +0.000 | +0.000 | +0.000 | 0 | 0.003598 |
| func2C | optuna-tpe | +0.000 | +0.000 | +0.000 | +0.000 | 3 | -0.05576 |
| func2C | optuna-tpe-3.6 | +0.000 | +0.000 | +0.000 | +0.000 | 2 | -0.1988 |
| func2C | hyperopt-tpe | +0.000 | +0.000 | +0.000 | +0.000 | 0 | -0.1007 |
| func2C | smac | +0.000 | +0.000 | +0.000 | +0.000 | 2 | 0.00139 |
| func2C | optuna-gp | +0.000 | +0.000 | +0.000 | +0.000 | 4 | -0.1951 |
| func2C | skopt-gp | +0.000 | +0.000 | +0.000 | +0.000 | 3 | 0.002013 |
| func2C | ax | +0.000 | +0.000 | +0.000 | +0.000 | 9 | -0.2028 |
| func3C | random | +0.000 | +0.000 | +0.000 | +0.000 | 0 | -0.0294 |
| func3C | optuna-tpe | +0.000 | +0.000 | +0.000 | +0.000 | 0 | -0.2799 |
| func3C | optuna-tpe-3.6 | +0.000 | +0.000 | +0.000 | +0.000 | 0 | -0.3131 |
| func3C | hyperopt-tpe | +0.000 | +0.000 | +0.000 | +0.000 | 0 | -0.1857 |
| func3C | smac | +0.000 | +0.000 | +0.000 | +0.000 | 0 | -0.2263 |
| func3C | optuna-gp | +0.000 | +0.000 | +0.000 | — | 2 | -0.5563 |
| func3C | skopt-gp | +0.000 | +0.000 | +0.000 | — | 7 | -0.459 |
| func3C | ax | +0.000 | +0.000 | +0.000 | — | 1 | 0.0419 |

## Pre-registered hypotheses (letter evaluation)

- **GH1a** optuna-tpe e(80)>0.05 per class: ['A', 'B', 'C', 'D'] (4/5) → PASS
- **GH1b** hyperopt-tpe e(80)>0.05 per class: ['A', 'B', 'C', 'D'] (4/5) → PASS
- **GH2** ax/smac zero-revisit violations: none; skopt |e|>0.07: ['skopt@cat_ackley_d3_L5/B20=-0.072'] → FAIL
- **GH3** rho(B, e)>=0 in 86/89 = 0.97 of no-dedup cells → PASS
- **GH4** optuna-tpe-3.6 classes passing: ['A', 'B', 'C', 'D'] (4/5) → PASS (class E over ml_* only, per Amendment 1)
- **GH5** real-ML benchmarks with e(80)>=0.05 per no-dedup arm: {'optuna-tpe': 1, 'optuna-tpe-3.6': 1, 'hyperopt-tpe': 0, 'optuna-gp': 1} → FAIL
- **GH6** (descriptive) Kendall tau between B=20 and B=160 arm rankings (median best; fast arms, all covered benchmarks):
    class A: median tau +nan over 5 benchmarks
    class B: median tau +0.32 over 3 benchmarks
    class C: median tau +0.16 over 4 benchmarks
    class D: median tau -0.20 over 2 benchmarks
    class E: median tau +nan over 7 benchmarks
    class F: median tau +0.10 over 2 benchmarks
- **GH7** random median revisits on float-bearing spaces: violations none → PASS; no-dedup nonzero (findings, not failures): ['optuna-gp@yahpo_rpart_41138=1', 'optuna-gp@ml_gb_bc=1', 'optuna-gp@yahpo_rpart_40981=36']

(distinct failed run attempts: 0; incomplete cells flagged as — above)
