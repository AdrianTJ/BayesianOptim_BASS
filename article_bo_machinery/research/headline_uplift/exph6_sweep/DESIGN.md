# G-sweep — pre-registered design (H6 generalization program)

**Committed before any protocol-scale run.** Tool bring-up smokes at tiny
budgets preceded this; no matrix cell has run. The analysis script
(analyze_g.py) will be committed before the first full wave launches
(same precedent as H1/H2).

## Question

Do the ecosystem-audit findings (H1-WASTE, H1-MASK, H2-SAT) generalize
across benchmark structure, budget, and library version — or were they
artifacts of one suite (cat-Ackley-dominated), one budget (80), and one
version snapshot?

## Benchmarks (23) in six classes

- **A additive/separable (5):** cat_ackley_d3_L5, cat_ackley_d5_L5,
  cat_ackley_d6_L11, catf_rastrigin_d4L7, catf_griewank_d5L7
- **B non-separable (3):** catf_rosen_d4L7, catf_michal_d5L9,
  catf_schwefel_d4L9
- **C epistatic binary (4):** nk_n20k2, nk_n20k8, maxcut_n20, labs_n25
- **D simulation (2):** pest_control, contam_2p25
- **E real ML (7):** ml_rf_digits, ml_svm_digits, ml_gb_bc, ml_mlp_wine,
  yahpo_rpart_41138, yahpo_rpart_40981, yahpo_ranger_1489
- **F mixed synthetic, continuity with H1 (2):** func2C, func3C
  (excluded from the 5-class generalization criterion, which uses A–E)

All instances, seeds, grids, and ground truths are already committed
(benchmarks_g.py, yahpo_adapter.py, g0_ground_truth.json). Solve
thresholds: |best − ground truth| ≤ 1e-9 where V2 recorded a ground
truth (all class A/B/C enumerable instances + cat_ackley d3/d5 + func2C/
func3C analytic); best-value distributions only elsewhere.

## Arms (8) and coverage

random, optuna-tpe (4.9.0), **optuna-tpe-3.6** (venv, version axis),
hyperopt-tpe, smac (venv), optuna-gp, skopt-gp, ax — defaults only,
seed-only non-defaults plus the recorded cosmetic settings, per-run
version stamps as before.

**Pre-registered coverage limits (never silent):** smac runs classes
A–D+F only (16 benchmarks): its venv's pinned sklearn 1.7.2 would break
the objective-identity rule on ml_* (real-ML objectives must be
numerically identical across arms — the optuna36 venv instead pins
sklearn==1.9.0 to match), and configspace ≥1.0 vs 0.6.1 makes
smac×YAHPO uninstallable in one env (H1 precedent). All other arms cover
all 23.

## Budgets

B ∈ {20, 40, 80, 160}. Fast arms (random, both optuna-tpe, hyperopt,
smac) run all four. GP-family arms (optuna-gp, skopt-gp, ax) run
{20, 40, 80} everywhere plus B=160 on the pre-named 8-benchmark subset
(one per class + both flagship real tasks): cat_ackley_d5_L5,
catf_rosen_d4L7, catf_michal_d5L9, nk_n20k2, pest_control,
ml_svm_digits, yahpo_rpart_41138, func2C.

**Seeds 4001–4025 (25/cell). Total 16,575 runs:** fast 4×23×4×25 = 9,200;
smac 16×4×25 = 1,600; GP 3×(23×3 + 8)×25 = 5,775.

Caps: 20 min/run; 45 min for GP-arm B=160 cells and the 25-dim spaces.
Cap-outs and failures logged per cell; a cell missing >5/25 is reported
incomplete. Thread pinning (OMP/MKL/OPENBLAS=1), 4 workers, resumable
JSONL per wave, one wave per class committed as it lands.

## Primary metric

**Excess-waste fraction e(B) = (revisits − pigeonhole(K, B))/B**, with
pigeonhole computed by the expm1/log1p form; pigeonhole = 0 for spaces
with any float dimension. Revisit keys: decoded combinations, 6-decimal
float rounding, active-parameter canonicalization on yahpo_ranger_1489
(key merge/split semantics gate-verified at G1).

## Pre-registered hypotheses (evaluated by the letter)

| ID | Clause |
|---|---|
| GH1 | For optuna-tpe-4.9 AND hyperopt-tpe separately: per-class median e(80) > 0.05 in ≥4 of the 5 classes A–E (per-class median = median over the class's benchmarks of per-benchmark median e(80)) |
| GH2 | ax and smac: median raw revisits = 0 in every covered cell at every budget; skopt-gp: median &#124;e(B)&#124; ≤ 0.07 in every cell |
| GH3 | Budget dependence: Spearman ρ(B, median e(B)) ≥ 0 in ≥70% of (no-dedup arm × benchmark) cells with all four budgets (no-dedup arms: both optuna-tpe, hyperopt-tpe, optuna-gp) |
| GH4 | Version robustness: optuna-tpe-3.6 satisfies GH1's clause too (per-class median e(80) > 0.05 in ≥4/5 classes) — waste is not a 4.x accident |
| GH5 | Real ML: at least one no-dedup arm has median e(80) ≥ 0.05 on ≥4 of the 7 class-E benchmarks |
| GH6 | (descriptive, no gate) Arm-ranking stability across budgets: Kendall τ between the B=20 and B=160 rankings (median best, solve tie-break where defined) per benchmark; reported per class |
| GH7 | Metric null: on every benchmark with ≥1 float dimension, the random arm's median revisits = 0 (nonzero no-dedup-arm revisits there are findings, not gate failures — H1's organic-true-positive precedent) |

Falsification is symmetric; any failed clause is reported as FAILED,
per the loop's process rule (composite criteria by the letter).

## Procedure

1. Commit this DESIGN. 2. Timing smoke: 1 seed on the slowest suspected
cells (ax×ml_rf_digits B=40; optuna-gp×nk_n20k8 B=80; skopt×labs_n25
B=80; smac×catf_michal B=80; optuna36×yahpo B=80) to validate caps.
3. Commit analyze_g.py (fixed aggregation + letter evaluation of
GH1–GH7) before wave 1. 4. Waves: G2 fast arms by class, G3 GP arms;
results committed per wave. 5. ANALYSIS.md from results only;
worker≠verifier adversarial review; only then CLAIMS.md and the G5
paper surgery.

## Deviations declared up front

- smac coverage limit and its reasons (above).
- yahpo_ranger_1489 keeps its one conditional via active-parameter keys;
  higher-conditionality scenarios (iaml_xgboost, 8 conditions) stay
  excluded from this sweep — visible scope boundary, not silent.
- Real-ML objectives are deterministic by fixed CV folds and estimator
  seeds; sklearn version pinned identical across all evaluating arms.
- Container recycles mid-sweep are expected: every wave is resumable
  from its JSONL; the recycle protocol re-validates gates (g0_gates.py)
  before resuming a wave after any recycle.
