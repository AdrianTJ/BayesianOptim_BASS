# G-sweep Amendment 2 (before the GP wave; no GP-arm matrix cell has run)

DESIGN.md is frozen and is not edited; this file amends it. Written
2026-10-02, before any GP-wave run beyond the three timing-smoke rows
already in `results.jsonl` (see STATUS.md).

## 1. Ax runs with unordered categoricals

The claims audit (`../../claims_audit/REPORT.md`, A5) and H7
(`../exph7_ax_categorical/`) found that the research copy of the Ax
driver (`../bo_audit/drivers.py`) leaves Ax 1.3.1's default in place:
integer choice parameters become **ordered**. Most G-sweep benchmarks
encode categories as integers, so the ax arm would treat them as a scale
while every other arm treats them as nominal.

`g_cell_runner.py` now routes the ax arm to the released package's driver
(`bo-audit/bo_audit/drivers.py`), which passes `is_ordered=False` on every
categorical. Each result row records this in `non_defaults`
(`"... is_ordered=False on categoricals"`), so rows are self-labelling.
The research copy stays as the record of what H1 ran.

The one pre-existing ax row (smoke cell `ml_rf_digits`/B40/seed 4001) uses
string-valued categories, which Ax already treats as unordered, so it is
consistent with this amendment and is kept.

## 2. Environment recorded per wave

Each machine that runs part of the wave writes `env_<host>.txt`
(`pip freeze` of the interpreter that runs the main-env arms, plus the
smac venv) before its first run. Objective identity across arms requires
`scikit-learn==1.9.0` and `numpy==2.4.6` in the main env, as in the fast
wave (DESIGN "objective-identity rule"); a machine that cannot match these
does not run class-E cells.

## 3. Where it runs

The GP wave and the smac backfill may run on a machine other than the
original container. Results append to the same `results.jsonl` schema;
keys are unique per (arm, benchmark, budget, seed), so partial waves from
different machines merge without conflict.

Metrics, budgets, seeds, arms, coverage and hypotheses: unchanged.
