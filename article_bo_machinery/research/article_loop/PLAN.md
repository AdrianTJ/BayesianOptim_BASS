# Article loop — research → decide → experiment → analyze → re-center

**Master plan for the machinery-confound article.** Supersedes the
writing-first sequencing of `../writing_loop/WRITING_PLAN.md` per user
direction (2026-08-21): experimentation is the core of the loop and the
article; prose writing is deferred until the claims have converged. The
writing plan's design (single-writer TeX, worker/verifier split, checks,
S1–S8 section order) is retained as the **terminal phase**, unchanged.

The project's loop principles still bind: fresh
context per cycle resuming from `LOG.md`, read-only fan-out with disjoint
briefs, worker ≠ verifier, deterministic checks, stall counter (2 no-commit
cycles → stop), no silent scope cuts. All outputs markdown (+ CSVs for raw
results), committed every cycle.

---

## The cycle

1. **Research** — what does this cycle need to know? (Literature already
   largely done via `../novelty_check/`; from here research is mostly
   *internal*: reading the R library, final_results, prior analyses.)
2. **Compile findings & decide** — update `CLAIMS.md` (the claim ledger) and
   record the cycle's decision: which claim is weakest / which experiment
   buys the most information next. Decisions are logged, never implicit.
3. **Run experiments** — per the experiment program below. Every experiment
   gets `experiments/expNN_<slug>/` with `DESIGN.md` written **before**
   running (hypothesis, protocol, seeds, budget, what result would falsify
   what), the runnable code, and raw CSVs.
4. **Analyze** — `ANALYSIS.md` in the experiment folder: numbers, tables,
   interpretation, threats to validity. Analysis is reviewed by an agent
   that did not run the experiment before it feeds the ledger.
5. **Re-center** — update `CLAIMS.md` statuses (supported / contradicted /
   untested / superseded) with evidence pointers; update the article's
   planned narrative if a claim moved; append the `LOG.md` entry with a
   "Next:" resume instruction.
6. **Repeat** — until the experiment program is exhausted and the ledger is
   stable; then, and only then, enter the writing phase
   (`../writing_loop/WRITING_PLAN.md`, S1–S8 + adversarial review), whose
   numbers all come from the ledger and experiment analyses.

## Experiment program

**Environment reality:** this container has Python 3 with
numpy/scipy/scikit-learn/optuna/pandas (installed 2026-08-21) and **no R**.
The oracle-ceiling audit is surrogate-free, so the machinery experiments run
natively here. Surrogate cells: GP (sklearn, continuous relaxation +
Garrido-Merchán-style handling), RF (SMAC-style, sklearn), TPE (optuna) run
here; **BASS cells require R** — attempt `apt-get r-base` + CRAN BASS once
in a background cycle; if that fails, queue exact `Rscript` commands for the
user's machine and mark those cells pending, never silently dropped.

Priority order (information value for the article):

| # | Experiment | Runs here? | Serves |
|---|---|---|---|
| E1 | **Harness validation.** Reimplement the candidate machinery in Python (schema encoding, hybrid generator with keep-combo vs forced-flip variants, encoding-level vs combination-level dedup) + objectives (Func-2C/3C, Cat-Ackley at 3 sizes). Validate by reproducing the R pipeline's known oracle results (15/15 keep-combo wins on Func-2C/3C pre-fix; optima −0.2063 / −0.7216; both arms clear pure-categorical pools). No new claims until this matches. | yes | everything downstream |
| E2 | **Oracle-ceiling matrix.** Full 2×2 machinery A/B (generator {restricted, permissive} × dedup {encoding, combination}) under the oracle, 25+ paired seeds, budgets 10/40/80, all benchmarks. This is the article's Section IV/V evidence at final protocol, plus the 4-cell matrix its Experiments section only promised. | yes | C1, C2a, C2b |
| E3 | **Surrogate × machinery matrix (GP, RF, TPE).** Same 4 machinery cells with real surrogates through one shared loop: sklearn GP (relaxation), sklearn RF (SMAC-style EI), optuna TPE. Paired seeds, win/tie/loss + Wilcoxon vs Random. Extends the article beyond BASS-vs-GP exactly as its Experiments section plans. | yes | C3, S6 |
| E4 | **BASS cells.** Same matrix cells for BASS(-BO) — R required. Try in-container R once; else queue for user machine (`run_all_final.sh` + diagnostics harness generalize). | attempt | C2, S4 |
| E5 | **Revisit-budget quantification.** Instrumented runs counting decoded-combination revisits under encoding-level dedup across surrogates (not just BASS): is the ~2/3 budget loss surrogate-dependent? | yes (E3 stack) | C2b |
| E6 | **Real mixed task.** One tuning task with genuine categorical choices (sklearn pipeline HPO, e.g. gradient boosting with categorical structural choices) under the shared machinery. | yes | S6 external validity |

Protocol constants (match the thesis final run unless an experiment's
DESIGN.md argues otherwise): budget 80, ≥25 paired seeds via shared initial
designs, per-seed win/tie/loss + Wilcoxon signed-rank, benchmark sizes
spanning solvable and unsolvable-in-budget instances.

**Numbers discipline:** `numbers.md` (Phase 0 of the writing plan) remains
the canonical extract of the *thesis* final results; each experiment's
ANALYSIS.md is canonical for *new* results. The article cites only these.

## Claim ledger

`CLAIMS.md` holds every claim the article intends to make, each with status
(supported / contradicted / untested / superseded), the evidence pointer,
and what would change it. The re-center step may not end while any claim
slated for the article is status *untested* without a queued experiment or
an explicit user decision to cut it.

## Testing leg (every cycle)

- `../tools/check_research.py` (research-folder integrity).
- Every experiment folder must contain DESIGN.md before results and
  ANALYSIS.md before its numbers enter the ledger (checked by
  `tools/check_experiments.py`, to be written in E1's cycle).
- Experiment code gets a smoke test (tiny budget/seeds) that runs in CI
  fashion each cycle it changes.
- Writing-phase checks (`check_article.py`) activate when writing begins.

## Known blockers / user decisions

- **Branch history:** upstream `main` was force-rewritten; this branch
  shares no ancestor with it. Merge/reset attempts are blocked by the
  session's permission classifier. Current workaround: main's content
  (final_results/, updated thesis, NLP HPO, R library) imported as files
  (commit on this branch); histories remain unrelated. User may prefer to
  reconcile locally or grant the permission; queued as a decision, not a
  blocker.
- Open user decisions from the writing plan (title, co-authors, venue)
  remain queued in `../writing_loop/WRITING_LOG.md`.
