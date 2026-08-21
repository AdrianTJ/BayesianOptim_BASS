# Headline uplift loop log

Append-only; one entry per cycle, newest last; "Next:" is the fresh-context
resume instruction. Claim ledger: new H-claims live in
../article_loop/CLAIMS.md alongside K1-K10.

## Cycle 0 — 2026-08-22
- **Phase:** H0 (instrumentation validation gate) — complete.
- **Did:** Built `bo_audit/` (AuditedObjective wrapper + drivers for
  optuna-tpe, optuna-gp, hyperopt-tpe, skopt-gp, random). Pre-registered
  DESIGN.md (G1–G4 correctness gates, D1–D4 detection gates), then ran
  run_h0.py: **H0 GATE OPEN** — counter exact vs analytic (G3 36.0 vs
  36.0; D4 20.0 vs 20.7), zero continuous false positives, and it detects
  admitted ecosystem behavior at large magnitude (optuna-tpe median 46/80
  revisits = 2.2× pigeonhole; hyperopt-tpe 35/80; skopt-gp 18.3 ≈
  pigeonhole with 597 fired dedup warnings). Adversarial review REFUTED
  the first-draft analysis (optuna-wins misreading contradicted by its own
  table — skopt-gp wins 0.000/10-10); ANALYSIS.md rewritten, core.py −0.0
  key normalization fixed, drivers.py disclosure corrected, REVIEW.md
  records all findings. Ledger: **H-VAL** added (supported, with caveats).
- **Environment notes:** SMAC3 import-broken (old build resolves private
  sklearn symbol) — pinned-version retry queued for H1, not dropped.
  Ax/BoTorch installed OK; optuna-gp driver written but deferred by design.
- **Next:** Cycle 1 = **H1** (in-the-wild audit matrix). Fresh-context
  resume: read `PLAN.md` phase H1 + `exph0_validation/{ANALYSIS,REVIEW}.md`.
  Steps: (1) retry SMAC3 with a version pin, smoke-test optuna-gp and an
  Ax/BoTorch driver; (2) install/smoke-test YAHPO Gym and JAHS-Bench-201,
  vendored COMBO pest-control fallback if egress blocks data files;
  (3) write pre-registered H1 DESIGN.md — libraries × benchmarks × ≥25
  seeds, headline metric = excess-over-pigeonhole (per H0 Observation 2),
  per-library hypotheses named before running; (4) run, analyze,
  worker≠verifier review, ledger, log, commit, push.

## Cycle 1 — 2026-08-21
- **Phase:** H1 setup + pre-registration — complete; timing smoke in flight.
- **Did:** Tooling: SMAC 2.4.0 isolated in a pinned venv (requires
  sklearn ≥1.6.1 yet imports a symbol our 1.9 removed; venv has 1.7.2) and
  driven via subprocess with in-subprocess bo-audit counting
  (`bo_audit/smac_runner.py`) — smoke-verified on categorical and mixed
  benchmarks (0 revisits at 20 trials, consistent with its runhistory
  mechanism). Ax driver written on the modern `ax.Client` API
  (smoke: 9 s/15 trials, 0 revisits); optuna-gp smoke showed **1 revisit in
  15 trials** (P6 motivation). Benchmarks: COMBO pest control vendored
  into `bo_audit/benchmarks.py` (pure numpy, determinized with a local
  seeded RNG — disclosed; original draws MC scenarios from the global RNG
  per call), determinism + value-range validated; shared `bench_by_name`
  adapter so main-env and smac-venv runs use identical benchmark code.
  **Logged drops:** JAHS-Bench-201 (requires Python <3.11), HEBO (GPy
  1.9.9 build failure in main env AND a py3.10 venv), YAHPO deferred to
  exploratory H1b (conditional spaces need active-parameter key semantics;
  rbv2_svm data downloaded, surrogate evaluates, SMAC×YAHPO impossible:
  ConfigSpace ≥1.0 vs 0.6.1). Pre-registered `exph1_matrix/DESIGN.md`
  (7 libraries × 6 benchmarks × 25 seeds, budget 80, seeds 3001–3025,
  excess-over-pigeonhole metric, hypotheses P1–P7, 20-min per-run cap,
  never-silent drop accounting) and committed it BEFORE any full run;
  `analyze_h1.py` (fixed aggregation + by-the-letter P1–P7 evaluation)
  also committed before data. Timing smoke (slow libs × 6 benchmarks,
  budget 80) launched in background.
- **Next:** Cycle 2 = H1 run + analysis. On smoke completion: check
  failures.log and wall times against the 20-min cap (ax on pest_control
  is the flagged risk), adjust nothing silently — any cap-driven cell drop
  goes in DESIGN deviations; launch `run_h1.py full` in background
  (resumable JSONL); when the matrix is complete run `analyze_h1.py`,
  write ANALYSIS.md, adversarial worker≠verifier REVIEW.md, ledger update
  (H1 claims only via review), LOG, commit, push. YAHPO/H1b only after
  the core matrix is banked.
