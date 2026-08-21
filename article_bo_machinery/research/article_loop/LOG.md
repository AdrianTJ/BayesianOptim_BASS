# Article loop log

Append-only; one entry per cycle, newest last; "Next:" is the fresh-context
resume instruction.

## Cycle 0 — 2026-08-21
- **Phase:** setup / re-center
- **Did:** Restructured per user direction: experimentation is now the core
  of the loop (research → decide → experiment → analyze → re-center →
  repeat), writing deferred to the terminal phase (writing plan retained
  unchanged). Imported main's content needed as ground truth
  (final_results/, updated thesis TeX, diagnostics, NLP HPO, R library) as
  files — history merge with rewritten main is classifier-blocked and
  queued as a user decision. Installed the Python experiment stack (numpy,
  scipy, scikit-learn, optuna, pandas). Wrote the experiment program
  (E1–E6) and the initial claim ledger (K1–K8; K3-old marked contradicted).
- **Files:** PLAN.md, CLAIMS.md, LOG.md, ../README.md, imported content
- **Tests:** check_research.py PASS
- **Next:** Cycle 1 = E1 (harness validation), per PLAN.md experiment
  program: DESIGN.md first, Python machinery harness vs the R reference,
  reproduce the pre-fix oracle numbers, ANALYSIS.md + independent review +
  check_experiments.py, then re-center the ledger.

## Cycle 1 — 2026-08-21
- **Phase:** experiment (E1 harness validation) + analyze + re-center
- **Did:** Built the Python machinery harness (`experiments/machinery.py`)
  from the R reference; pre-registered DESIGN.md with 6 checks; ran 135
  runs (3 benchmarks × 15 seeds × 3 arms, budget 80) in 67 s. 5/6 checks
  PASS including the load-bearing 15/15 paired keep-vs-flip wins on both
  mixed benchmarks and exact-optimum hits. V2 FAIL led to a real find: the
  recorded R figure "−0.148 at budget 10" is unreproducible from the
  committed R code (tail-probability argument + exact historical generator
  test) — quarantined as K2a-fig; the article must not quote it pending an
  R re-run. Exact optima computed and analytically proven in review:
  func2C −0.206326 at (2,2), func3C −0.722140 at (2,2,1). Independent
  Sonnet review UPHELD the analysis and caught two code defects (unstable
  hash-based RNG seeding; V6 min/max) — both fixed, experiment re-run
  deterministically. Ledger updated: K1, K6 supported with measured cost;
  K2a supported with magnitude revised down at n_cand=1000 (pool-size
  dependence queued for E2); Kopt added; K2a-fig quarantined.
- **Files:** experiments/machinery.py, exp01_harness_validation/{DESIGN,
  ANALYSIS,REVIEW}.md + run_e1.py + results.csv, tools/check_experiments.py,
  CLAIMS.md
- **Tests:** check_research.py PASS; check_experiments.py PASS; E1 V-checks
  5/6 PASS (V2 = the quarantined reference number, documented)
- **User decisions (open):** title/co-authors/venue (unchanged); branch
  history reconciliation with rewritten main (unchanged).
- **Next:** Cycle 2 = E2 (oracle-ceiling matrix at final protocol): DESIGN
  first — 2 generator variants × 2 dedup levels (add encoding-level dedup
  to the harness as a loop option), 25 seeds (1001–1025), budget 80,
  benchmarks func2C/func3C/cat_ackley d3L5+d6L11, plus the pool-size axis
  n_cand ∈ {50, 200, 1000} on func2C/func3C to test K2a's magnitude
  dependence. Reuse validated machinery.py; independent review before
  ledger update.
