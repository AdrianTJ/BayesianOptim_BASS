#!/usr/bin/env bash
# =============================================================================
# run_all_final.sh  --  Regenerate every final result in one pass
# =============================================================================
# Runs all six synthetic/categorical benchmarks, the TPE gamma-sensitivity
# ablation, and the Elastic Net case study with the headline protocol
# (budget=80, reps=25). Every runner writes under a single results/ root:
#
#     results/<objective>/            (run_benchmark.R)
#     results/tpe_sensitivity/<obj>/  (run_tpe_sensitivity.R)
#     results/elastic_net/            (run_elastic_net.R)
#
# Usage:
#   bash run_all_final.sh                 # full headline run
#   BUDGET=15 REPS=3 bash run_all_final.sh # fast smoke run
#
# Note: this is compute-heavy (BASS refits an MCMC every BO iteration). The
# categorical runs add the TPE baseline, which needs reticulate + an importable
# optuna; drop WITH_TPE=false below if you don't have that set up.
# =============================================================================
set -euo pipefail

# Run from this script's directory so results/ always lands in code_files/.
cd "$(dirname "$0")"

BUDGET="${BUDGET:-80}"
REPS="${REPS:-25}"
WITH_TPE="${WITH_TPE:-true}"

echo "### Protocol: budget=${BUDGET}, reps=${REPS}, with_tpe=${WITH_TPE}"

# --- Continuous benchmarks ---------------------------------------------------
Rscript run_benchmark.R --objective=branin    --d=2 --budget="$BUDGET" --reps="$REPS"
Rscript run_benchmark.R --objective=rastrigin  --d=4 --budget="$BUDGET" --reps="$REPS"
Rscript run_benchmark.R --objective=synthetic  --d=3 --budget="$BUDGET" --reps="$REPS"

# --- Categorical / mixed benchmarks (TPE is the strongest comparison here) ---
Rscript run_benchmark.R --objective=func2C            --budget="$BUDGET" --reps="$REPS" --with_tpe="$WITH_TPE"
Rscript run_benchmark.R --objective=func3C            --budget="$BUDGET" --reps="$REPS" --with_tpe="$WITH_TPE"
Rscript run_benchmark.R --objective=cat_ackley --d=6 --budget="$BUDGET" --reps="$REPS" --with_tpe="$WITH_TPE"

# --- TPE gamma-sensitivity ablation (branin + cat_ackley) --------------------
Rscript 2_tpe_sensitivity/run_tpe_sensitivity.R --budget="$BUDGET" --reps="$REPS"

# --- Elastic Net case study (its own defaults: reps=50, budget=100) ----------
Rscript 4_regression_test_case/run_elastic_net.R

echo "### Done. All results are under: $(pwd)/results"
