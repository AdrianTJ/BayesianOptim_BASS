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
- **Next:** Cycle 1 = E1 (harness validation): write
  `experiments/exp01_harness_validation/DESIGN.md` first, then implement
  the Python machinery harness (schema encoding; hybrid generator with
  keep-combo/forced-flip variants; encoding vs combination dedup) and the
  objectives (Func-2C, Func-3C, Cat-Ackley d∈{3,4,6}, L∈{5,7,11}) against
  the R reference in `code_files/R/`; reproduce the pre-fix oracle numbers
  (15/15, −0.148 plateau vs −0.2063; both arms clear pure-categorical
  pools); write ANALYSIS.md + `tools/check_experiments.py`; update ledger.
