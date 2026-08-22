# H6 generalization loop log

Append-only; one entry per cycle, newest last; "Next:" is the
fresh-context resume instruction. Claims ledger: ../../article_loop/CLAIMS.md.
Recycle protocol: at every wakeup verify checkout (fetch+reset to
origin/claude/machinery-confound-article) and environment
(../tools/provision.sh) before continuing — the container recycled three
times on 2026-08-21/22 alone; commit+push after every meaningful step.

## Cycle G0 — 2026-08-22
- **Phase:** benchmark vendoring + validation gates — complete.
- **Did:** `bo_audit/benchmarks_g.py`: 14 new benchmarks across the five
  pre-planned structure classes — categorized classics with permuted
  grids and on-grid analytic optima (rastrigin d4L7, rosenbrock d4L7,
  michalewicz d5L9, griewank d5L7, schwefel d4L9), NK landscapes N=20
  K∈{2,8} (seeded instances), weighted Max-Cut n=20, LABS n=25,
  determinized contamination control 2^25 (pest-control convention),
  and four REAL sklearn HPO tasks with mixed cat+cont/int spaces
  (RF/digits 2.1s/eval, SVM-pipeline/digits 1.1s, GB/breast-cancer
  0.6s, MLP/wine 0.2s — all deterministic via fixed folds+seeds).
  `bench_by_name` dispatches, so every runner (incl. smac/optuna36
  venvs) gets them unchanged. `g0_gates.py`: **ALL PASS** — V1
  determinism (bit-equal) for all 14; V2 exhaustive ground truth for
  all 7 enumerable pure-cat instances (analytic-0 confirmed for
  rastrigin/rosen/griewank; michalewicz −2.868472, schwefel 5.1e-5,
  maxcut −34.678388, NK −0.711123/−0.801508 recorded in
  g0_ground_truth.json as future solve thresholds); V3 per-family
  duplicate injection through the audit wrapper (true dup counted,
  1e-3 near-miss not, 1e-9 within-rounding counted on mixed spaces).
  Environment recycled again mid-cycle; recovery via the scripted
  protocol took minutes and G0's gates re-validate the instrument
  after any future recycle.
- **Deviation (visible, not silent):** the YAHPO adapter and its
  conditional-space (active-parameter) key semantics move to G1,
  alongside the DESIGN that must pre-register them — gate logic and
  pre-registration belong in the same commit. Anchor-paper PDFs were
  uploaded by the author but lost to a recycle before they could be
  read; awaiting re-upload (AUTHOR_TODO item 2 unchanged).
- **Next:** Cycle G1 = pre-registration. (1) YAHPO adapter: pick 2–3
  scenarios/instances from the downloaded data (prefer low/no
  conditionality; rbv2_rpart and iaml_* candidates), implement
  active-parameter combination keys, injection gate per scenario;
  (2) write exph6_sweep/DESIGN.md — full benchmark list (existing 6 +
  new 14 + YAHPO), budgets {20,40,80,160}, arms (fast: random,
  optuna-tpe 4.9, optuna-tpe 3.6 venv, hyperopt-tpe, smac venv; GP:
  optuna-gp, skopt-gp, ax with the B=160 8-benchmark subset named),
  seeds 4001–4025, per-class hypotheses with letter-precise clauses,
  excess-fraction e(B) as primary metric, solve thresholds from
  g0_ground_truth.json, caps and never-silent drop rules; (3) new
  optuna36-venv runner (mirror smac_runner pattern); (4) commit DESIGN
  before any timing smoke; run the timing smoke; fix caps if needed;
  (5) LOG, commit, push, re-arm. G2 (fast-arm waves) follows.

## Cycle G1 — 2026-08-22
- **Phase:** pre-registration — complete; timing smoke in flight.
- **Did:** YAHPO adapter (`bo_audit/yahpo_adapter.py`): rbv2_rpart
  instances 41138/40981 (verified 0 conditionals — clean flat mixed
  spaces) and iaml_ranger 1489 (1 conditional; active-parameter keys via
  a new optional `canonicalize` hook in core.py — inactive
  num.random.splits merges keys under splitrule≠extratrees, splits them
  under extratrees; both directions gate-verified). Max fidelity,
  logloss target, deterministic, 1–2 ms/eval. optuna36_runner.py written
  (version arm); provision.sh now pins the optuna36 venv's sklearn to
  the main env's exact 1.9.0 (objective-identity rule) and adds
  yahpo-gym; venv rebuilt and verified. **Pre-registered
  exph6_sweep/DESIGN.md and committed before any run:** 23 benchmarks
  in 6 classes, 8 arms, budgets {20,40,80,160}, seeds 4001–4025,
  16,575 runs, e(B) excess-fraction metric, hypotheses GH1–GH7 with
  letter-precise clauses, smac coverage limit (objective-identity +
  configspace conflicts) and the conditional-space boundary declared.
  g_cell_runner.py + run_g.py (wave orchestrator, per-wave resumable,
  per-cell caps, recycle-tolerant). Timing smoke (5 slowest suspected
  cells) launched in background.
- **Next:** Cycle G2 = fast-arm waves. On smoke completion: check the 5
  timings against caps (adjust caps only via a logged DESIGN amendment);
  write and commit analyze_g.py (fixed aggregation + GH1–GH7 letter
  evaluation) BEFORE launching wave 1; then launch `run_g.py fast` waves
  class by class (A→F), committing results.jsonl after each wave; then
  `run_g.py gp` waves (G3). After each recycle: provision + re-run
  g0_gates.py before resuming. When the matrix completes: ANALYSIS.md,
  adversarial review, ledger, then G5 paper surgery.
