#!/bin/bash
# Re-provision the headline-program environment after a container recycle.
# Idempotent. Run from anywhere: bash tools/provision.sh
set -x
pip install --quiet hyperopt scikit-optimize ax-platform yahpo-gym 2>&1 | tail -2
SCRATCH="${SCRATCH:-/tmp/claude-0/-home-user-BayesianOptim-BASS/d8cc14fb-4c54-5880-818f-3a67a8836a1b/scratchpad}"
if [ ! -x "$SCRATCH/smac_venv/bin/python" ]; then
  uv venv "$SCRATCH/smac_venv" --python 3.11 -q
  uv pip install -q --python "$SCRATCH/smac_venv/bin/python" "smac==2.4.0" "scikit-learn>=1.6.1,<1.8" numpy
fi
"$SCRATCH/smac_venv/bin/python" -c "import smac; print('smac venv OK')"
if [ ! -x "$SCRATCH/optuna36_venv/bin/python" ]; then
  uv venv "$SCRATCH/optuna36_venv" --python 3.11 -q
  uv pip install -q --python "$SCRATCH/optuna36_venv/bin/python" "optuna==3.6.1" numpy scipy
fi
"$SCRATCH/optuna36_venv/bin/python" -c "import optuna; print('optuna36 venv OK', optuna.__version__)"
if [ ! -d "$SCRATCH/yahpo_data" ]; then
  git clone -q --filter=blob:none --sparse https://github.com/slds-lmu/yahpo_data.git "$SCRATCH/yahpo_data"
fi
(cd "$SCRATCH/yahpo_data" && git sparse-checkout set rbv2_svm rbv2_xgboost rbv2_ranger rbv2_rpart iaml_ranger iaml_xgboost 2>/dev/null; du -sh .)
python3 -c "import hyperopt, skopt, ax; print('main env OK')"
