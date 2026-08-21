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
