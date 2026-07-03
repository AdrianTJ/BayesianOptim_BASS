# The Machinery Confound (article scaffold)

Scaffold for a standalone article on the methodological finding that came out
of the thesis work: **acquisition-optimization machinery — candidate
generation and duplicate handling — can dominate surrogate comparisons in
mixed and categorical Bayesian optimization**, and a cheap diagnostic (the
*oracle-ceiling audit*) that exposes it.

Where things came from and where they lead:

- **Evidence**: `code_files/3_categorical_diagnostics/` in this repository —
  the oracle A/B experiments, revisit counting, and surrogate fit-quality
  checks. Numbers currently quoted in the scaffold are the preliminary runs
  (10–15 seeds, budgets 60–80) recorded in that folder's README; the full
  25-seed protocol (`run_all_final.sh`) supersedes them.
- **Templating**: the AIP REVTeX 4.1 document class and preamble, copied from
  `class_presentation/ReporteFinal/`'s article. The class itself comes from
  the TeX distribution (any full TeX Live / MacTeX; on minimal installs,
  `texlive-publishers`) rather than being vendored — the copy in
  `ReporteFinal/` is incomplete as a standalone distribution.
- **Status**: section-level structure and the argument are in place; every
  `% TODO` block marks either full-protocol numbers to insert or new
  experiments to run (see the Experiments section for the planned
  surrogate × machinery matrix).

## Build

```bash
cd article_bo_machinery
pdflatex main && bibtex main && pdflatex main && pdflatex main
```

## Relationship to the thesis

The thesis (`written_files/tesis_escrito/`) claims this finding briefly in its
Conclusions and defers the systematic treatment here. Keep the two consistent:
the thesis states the finding and cites the machinery precedents (SMAC,
Casmopolitan, Garrido-Merchán & Hernández-Lobato); this article generalizes
the audit, expands the experimental matrix beyond BASS-vs-GP, and proposes the
machinery-controlled comparison protocol.
