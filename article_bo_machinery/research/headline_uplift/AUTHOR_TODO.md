# Author to-do list (compiled at headline-loop close, Cycle 8)

Everything the loop could not do in-container, in rough priority order.

## Before submission

1. **Compile the paper locally** (`pdflatex main.tex` + `bibtex`; no TeX
   in the loop's container — the draft has never been compiled). Check
   Table `tab:wild` and the appendix render; the two-panel dedup figure
   (`figures/fig_dedup_audit.pdf`) is pre-built.
2. **Verify-and-cite the anchor papers** (full text, then add to
   references.bib and the appendix/Related Work):
   - Kim et al., arXiv:2506.11831 (inexact-acquisition regret) — the
     regret-decomposition framing hook for Lemma A/Prop B.
   - B3O, arXiv:2606.30228 (softmax-selection regret) — the hook for
     Prop C(b)'s softmax law.
   - Audit-genre positioning cites flagged as a TODO in Related Work:
     Recht et al. (ImageNet replication), the bayesmark/BBO-challenge
     report (Turner et al. is already cited), MCBO (already cited).
   The loop's rule: none of these enter the text before full-text
   verification (arXiv was egress-blocked in-container).
3. **Title decision.** Placeholder stands ("The Machinery Confound…").
   Loop recommendation: keep the name, consider a subtitle carrying the
   audit ("…: Auditing What Bayesian-Optimization Budgets Actually Buy").
   Author's call, with co-authors/advisor and acknowledgments (TODOs in
   main.tex).
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

5. **E4: BASS through the machinery ablations** on the author's machine
   (R + BASS; exact commands in `article_loop/experiments/` and
   `run_all_final.sh`). Would close the paper's one open cell.
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
9. **bo-audit packaging** if submitting to D&B: `bo_audit/` is
   pip-structured but has no pyproject.toml/tests-as-package; an
   afternoon of packaging + README makes it installable.
10. If the pest-control determinization matters to reviewers: the
    disclosure is in `bo_audit/benchmarks.py` and the paper; the
    stochastic-original comparison is a possible rebuttal experiment.

## Standing caveats the paper already carries (no action, awareness only)

- Q2's composite FAIL (hyperopt refund), the func2C fit non-transfer,
  Prop B's conjecture status, the skopt-gp d3 carryover exception, and
  the four refuted-then-corrected internal analyses are all disclosed in
  the text — reviewers may probe them; the REVIEW.md files under
  `research/` are the receipts.
