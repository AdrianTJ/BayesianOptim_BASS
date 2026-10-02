#!/usr/bin/env bash
# Replays the TPE cells checked in REPORT.md. V = venv bin dir with the recorded library versions.
OUT=${OUT:-$(cd "$(dirname "$0")" && pwd)/out}; mkdir -p $OUT
V=${V:-/tmp/claude-0/audit-venv/bin}; H=$(cd "$(dirname "$0")/../headline_uplift" && pwd)
cd $H
for s in $(seq 3001 3025); do
  echo "$V/python exph1_matrix/cell_runner.py optuna-tpe cat_ackley_d5_L5 80 $s >> $OUT/h1.jsonl"
  echo "$V/python exph1_matrix/cell_runner.py hyperopt-tpe cat_ackley_d5_L5 80 $s >> $OUT/h1.jsonl"
  echo "$V/python exph1_matrix/cell_runner.py optuna-gp cat_ackley_d3_L5 80 $s >> $OUT/h1.jsonl"
  echo "$V/python exph1_matrix/cell_runner.py optuna-tpe pest_control 80 $s >> $OUT/h1.jsonl"
  echo "$V/python exph2_control/h2_cell_runner.py optuna-tpe cat_ackley_d5_L5 80 $s >> $OUT/h2.jsonl"
done | OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 xargs -P 4 -I{} sh -c "{}"
echo done
