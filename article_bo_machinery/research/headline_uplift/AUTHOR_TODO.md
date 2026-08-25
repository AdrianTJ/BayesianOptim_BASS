# Author to-do list (compiled at headline-loop close, Cycle 8)

Everything the loop could not do in-container, in rough priority order.

## Before submission

1. **DONE (2026-08-24): compiled locally.** One blocking defect found and
   fixed: `figures/fig_dedup_audit.pdf` had never been committed (built
   only in the loop's container) — regenerated from the committed E2
   results with a checked-in script (`figures/make_fig_dedup_audit.py`);
   medians reproduce the caption exactly (0 vs 78/80). One overfull table
   (tab:dedup) fixed via header shortening. Final: 11 pages, no errors,
   no undefined refs, no overfull boxes.
2. **DONE (2026-08-22): anchor papers verified and cited.** Both
   PDFs supplied by the author, full texts read, citations
   kim2025inexact + bloor2026b3o wired into Related Work and the
   appendix; record in anchor_verification.md. REMAINING from this
   item: the audit-genre positioning cites only (Recht et al.,
   bayesmark) — still verify-before-cite.
   Original item follows for the record: Verify-and-cite the anchor papers (full text, then add to
   references.bib and the appendix/Related Work). Both are REAL and the
   arXiv IDs are correct — an external ID lookup reported them missing
   on 2026-08-21, but web search corroborates both with
   title/author/venue (verify by TITLE if an ID checker balks; the
   June-2026 ID legitimately trips some indexes):
   - arXiv:2506.11831 — **"Bayesian Optimization with Inexact
     Acquisition: Is Random Grid Search Sufficient?"**, Kim, PMLR v286
     (UAI 2025). Regret bounds for GP-UCB/GP-TS under inexact
     (grid/pool-based) acquisition maximization — the hook for
     Lemma A / Prop B.
   - arXiv:2606.30228 — **"B3O: Scalable Boltzmann Batch Bayesian
     Optimization"**, Bloor et al., June 2026. Boltzmann/softmax
     acquisition sampling with UCB-rate regret — the hook for Prop C(b).
   - Audit-genre positioning cites flagged as a TODO in Related Work:
     Recht et al. (ImageNet replication), the bayesmark/BBO-challenge
     report (Turner et al. is already cited), MCBO (already cited).
   The loop's rule stands: none of these enter the text before
   full-text verification (arXiv AND proceedings.mlr.press are
   egress-blocked in-container; the corroborating search metadata is
   recorded in h6_generalize/PLAN.md).
3. **DONE (2026-08-24): author decisions applied.** Title kept as-is
   ("The Machinery Confound: Acquisition-Optimization Machinery…");
   sole author; email adrian.tame.jacobo@gmail.com; acknowledgments left
   as a marked placeholder until the author supplies text.
4. **Venue call.** With the in-the-wild audit (1,050 runs, 6 libraries),
   the Z=3/4 re-comparison, the released tool, and the theory appendix,
   the paper now has the profile of the audit-genre papers that landed at
   main venues (third-party systems, released artifact, one striking
   number). Realistic targets, in order of fit: NeurIPS Datasets &
   Benchmarks (the tool + audit is exactly their rubric; code release
   already satisfied), ICML/NeurIPS main (framed as the confound +
   protocol paper), AutoML Conf (natural home, smaller splash), TMLR
   (if speed matters over venue brand). JMLR/MLOSS note for bo-audit is
   a possible companion.

## Runs only the author can do (open cells, honestly labeled in the paper)

5. **DECLINED by author decision (2026-08-24): E4 will not be run.**
   Rationale: the audit evaluates deployed, pip-installable candidates
   (optuna and the like); BASS is the motivating research pipeline and
   case study, not a candidate. The paper's "open cell" text has been
   rewritten as a deliberate scope statement (Experiments + Discussion).
   Commands remain in `run_all_final.sh` for anyone who wants the cell.
6. Optional **H1b: YAHPO conditional-space audit** — data downloaded and
   surrogate verified working in-container; needs pre-registered
   active-parameter key semantics (design sketch in exph1_matrix
   DESIGN deviations). Adds a "realistic HPO spaces" cell to tab:wild.
7. Optional **H4 strong-surrogate-near-ceiling attempt** (more GP
   restarts / larger budget on an easy mixed instance) to try to observe
   the generator effect on a real surrogate (K5's undetected axis).

## Repository hygiene

8. **Branch reconciliation:** `main` was force-rewritten upstream at some
   point; this branch (`claude/machinery-confound-article`) carries the
   whole program. Decide merge strategy (the loop imported needed
   content via `git checkout origin/main -- <paths>` rather than
   history surgery).
9. **DONE (2026-08-24): bo-audit packaged and promoted.** Canonical home
   is now the top-level `bo-audit/` directory on `main`
   (pyproject.toml, README, 10-test suite; audited code moved verbatim).
   Verified: clean-venv pip install, tests pass, end-to-end optuna-TPE
   audit from site-packages. The paper's Data Availability section points
   there.
10. If the pest-control determinization matters to reviewers: the
    disclosure is in `bo_audit/benchmarks.py` and the paper; the
    stochastic-original comparison is a possible rebuttal experiment.

## Standing caveats the paper already carries (no action, awareness only)

- Q2's composite FAIL (hyperopt refund), the func2C fit non-transfer,
  Prop B's conjecture status, the skopt-gp d3 carryover exception, and
  the four refuted-then-corrected internal analyses are all disclosed in
  the text — reviewers may probe them; the REVIEW.md files under
  `research/` are the receipts.
