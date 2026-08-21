# H6 — Generalization program (pre-loop scoping plan)

**Author directive (2026-08-21):** go heavier on computation and numbers.
"Single version snapshot; six libraries; one budget (80); cat-Ackley's
structure favors one-hot GPs" is not good enough. Many more functions,
real ML problems from the wild, budget variability. Results must
generalize, not be pinned to a specific suite or budget.

This plan addresses each named axis with a concrete, in-container-feasible
design. It is a scoping document; the loop's G-cycles pre-register the
binding DESIGN.md files as always.

## Axis 1 — Function breadth: ~24 benchmarks in five structure classes

The "cat-Ackley favors one-hot GPs" critique is about *structure
monoculture*. The fix is families with deliberately different structure,
so any claim must survive per-class aggregation. All synthetic families
are vendored numpy (no egress), permuted-encoded where levels could leak
ordinal structure, with seeded instances committed before runs.

| class | benchmarks | why this class |
|---|---|---|
| A. additive/separable | cat-Ackley d3L5 / d5L5 / d6L11 (continuity with H1); cat-Rastrigin d4L7; cat-Griewank d5L7 | the class GPs like — now labeled as such |
| B. non-separable | cat-Rosenbrock d4L7 (chain coupling); cat-Michalewicz d5L9; cat-Schwefel d4L9 (deceptive) | kills the additive-only story |
| C. epistatic binary | NK landscapes N=20 K∈{2,8} (seeded); Max-Cut QUBO n=20; LABS n=25 | tunable interaction order; standard combinatorial-BO fare |
| D. simulation | pest control 5^25 (existing); contamination control 2^25 (COMBO family, vendored+determinized like pest) | large spaces, rugged, non-analytic |
| E. real ML (the wild) | sklearn HPO on built-in data, mixed cat+cont spaces, timed 0.1–2 s/eval: RF/digits (max_features, criterion cats + depth/split ints), SVM-pipeline/digits (preprocessing cat: none/standardize/PCA + C, gamma floats), GB/breast-cancer (max_features cat + lr/subsample floats + depth int), MLP/wine (architecture cat + alpha/lr floats); plus YAHPO surrogate HPO (rbv2/iaml scenarios × 2–3 OpenML task instances — real fitted response surfaces of real tuning problems) | "real problems from the wild" without egress; YAHPO = published surrogates of large real HPO studies |

Mixed continuity: Func-2C/3C stay. YAHPO conditional spaces get the
pre-registered active-parameter key semantics (inactive params excluded
from the combination key) plus a scripted injection gate per scenario
before any measurement — the H1b design debt, now paid inside G-cycles.

## Axis 2 — Budget variability

Budget becomes a swept factor: **B ∈ {20, 40, 80, 160}**. The audit
metric already scales: pigeonhole(K, B) is analytic per cell, and the
headline quantity becomes the **excess-waste fraction e(B) = (revisits −
pigeonhole)/B** reported as a curve, not a number. Pre-registered
hypotheses about the curves (exact clauses fixed in G1's DESIGN):
- no-dedup samplers: e(B) non-decreasing in B on small-K spaces
  (saturation predicts growth), ≈flat on huge-K;
- dedup-equipped: e(B) ≈ 0 at every B;
- the Z-of-W ranking-change analysis recomputed at each budget — does
  "machinery changes conclusions" replicate across budgets, or is it a
  B=80 artifact? Either answer is a result.

## Axis 3 — Version snapshot

Add an **optuna 3.6 TPE arm** in its own venv (TPE defaults changed
materially across 3.x→4.x) on the full fast-arm grid. Two versions of
the most-deployed sampler turns "a snapshot" into "a longitudinal pair"
and directly tests whether the waste is a version accident. (Further
versions/libraries stay author-optional; venv recipe generalizes.)

## Arms and allocation (compute realism)

Provisioning is now scripted (`tools/provision.sh`) since the container
recycled once already; G0 re-validates gates after any recycle.

- **Fast arms** — random, optuna-tpe (4.9), optuna-tpe (3.6), hyperopt-tpe,
  smac(venv) — run the FULL grid: ~24 benchmarks × 4 budgets × 25 seeds
  × 5 arms ≈ 12,000 runs, nearly all < 5 s (smac ~ 30 s; GB-based cells
  ~ minutes at B=160).
- **GP-family arms** — optuna-gp, skopt-gp, ax — run all benchmarks at
  B ∈ {20, 40, 80} and a pre-named 8-benchmark subset at B=160 (one per
  structure class + both flagship real-ML tasks), ≈ 5,800 runs with
  per-run caps (20 min; 45 min for B=160 and the 25-dim spaces),
  thread-pinned, 4 workers. Cap-outs reported per cell, never silent.
- Estimated total: **~18,000 runs, roughly 80–120 core-hours → 2–3 days
  of background wall time**, resumable JSONL throughout, run in
  class-sized waves so partial results are analyzable and committable
  per cycle.

## Pre-registered analysis shape (binding version in G1 DESIGN)

- Primary: e(B) per (arm × benchmark × budget), aggregated **per
  structure class** (medians + sign consistency). A generalization
  claim requires the effect's sign to hold in ≥4 of 5 classes,
  pre-named — not pooled averages that a monoculture could dominate.
- Solve/quality axis reported per class with each class's structure
  caveat attached; real-ML tasks report test-CV loss distributions.
- Ranking-change Z recomputed per budget with the H2 registered metric.
- All numbers flow through an extended extract_h_numbers pipeline; the
  paper quotes only from it.

## Cycle map

- **G0** provision + vendor the new benchmark families + per-family
  validation gates (analytic optima where known, determinism, scripted
  duplicate injection incl. YAHPO active-parameter keys). Commit before
  any measurement.
- **G1** pre-register DESIGN.md (exact benchmark instances + seeds,
  hypotheses with letter-precise clauses, allocation, caps) + timing
  smoke. Commit before the sweep.
- **G2** fast-arm full sweep (background waves). **G3** GP-arm sweep.
- **G4** analysis with pre-committed script + adversarial review +
  ledger.
- **G5** paper surgery: tab:wild → per-class table + e(B) budget
  curves figure; limitations paragraph rewritten to the new scope;
  every stale number swept; full-pass review.

## Anchor-paper resolution (this turn's research)

Both anchors are real; the IDs were correct and the failure was in the
external lookup. Corroborated metadata for the author's verification:
- arXiv:2506.11831 = **"Bayesian Optimization with Inexact Acquisition:
  Is Random Grid Search Sufficient?"**, Kim, PMLR v286 (UAI 2025) —
  proves regret bounds for GP-UCB/GP-TS under inexact (grid/pool-based)
  acquisition maximization; directly relevant to Lemma A/Prop B framing.
- arXiv:2606.30228 = **"B3O: Scalable Boltzmann Batch Bayesian
  Optimization"**, Bloor et al. (June 2026) — Boltzmann/softmax sampling
  of the acquisition with UCB-rate regret; directly relevant to Prop
  C(b). (A June-2026 ID can trip index checkers.)
Both arxiv.org and proceedings.mlr.press remain egress-blocked here, so
the full-text-before-citing gate still holds; AUTHOR_TODO updated with
titles/authors so verification is a title search, not an ID lookup.
