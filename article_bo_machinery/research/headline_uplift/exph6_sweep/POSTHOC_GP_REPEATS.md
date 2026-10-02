# Post-hoc check: where Optuna's GP sampler repeats on a mixed space

Not pre-registered. Run 2026-10-02 after ANALYSIS.md and its review, to
see what descriptive finding 2 (optuna-gp exact repeats on float-bearing
spaces) consists of.

Script: `posthoc_gp_repeats.py` (run from `headline_uplift/`), optuna-gp
on ml_gb_bc, budget 80, seeds 4001–4004, logging every proposed
configuration. Environment: the claims-audit ax-venv (Optuna 4.9.0,
torch, scikit-learn 1.9.1; not the sweep's pinned 1.9.0, so objective
values may differ slightly from the sweep's rows).

Output:

```
{"seed": 4001, "revisits": 0, "repeated": []}
{"seed": 4002, "revisits": 2, "repeated": [["max_features=1.0|logLR=0.0|subsample=1.0|max_depth=1", 3, "first@76"]]}
{"seed": 4003, "revisits": 3, "repeated": [["max_features=log2|logLR=0.0|subsample=1.0|max_depth=1", 2, "first@11"], ["max_features=1.0|logLR=0.0|subsample=1.0|max_depth=1", 3, "first@18"]]}
{"seed": 4004, "revisits": 1, "repeated": [["max_features=1.0|logLR=0.0|subsample=1.0|max_depth=1", 2, "first@43"]]}
```

Every repeated configuration has both continuous coordinates at their
upper bounds (logLR = 0.0, subsample = 1.0) and the integer at its lower
bound: the repeats are box corners, consistent with the acquisition
optimizer clipping to the bounds. The YAHPO cases (yahpo_rpart_40981,
yahpo_rpart_41138) were not traced; YAHPO does not run in this
environment.
