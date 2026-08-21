# Writing loop log

Append-only record; one entry per cycle, newest last. Each entry's "Next:"
is the resume instruction for a fresh-context cycle. "User decisions"
accumulate items only the author can settle.

## Cycle 0 — 2026-08-21
- **Did:** Plan created (`WRITING_PLAN.md`) applying the Q1 loop-engineering
  findings: single-writer prose, read-only fan-out, worker/verifier split,
  deterministic checks, stall counter. Loop-engineering track paused at its
  Cycle 1.
- **Files:** WRITING_PLAN.md, WRITING_LOG.md, ../README.md,
  ../loop_engineering/LOG.md
- **TODO count (main.tex):** 14 (baseline, to ratchet down)
- **Tests:** check_research.py PASS (check_article.py not yet written)
- **User decisions (open):** final title (scaffold lists 3 candidates);
  co-authors/advisor line; acknowledgments; target venue.
- **Next:** Cycle 1 = Phase 0: write `tools/extract_numbers.py`, generate
  `numbers.md` from `final_results/` + the diagnostics README, and write
  `tools/check_article.py`; run both; commit. No TeX edits until numbers.md
  exists.

## Cycle 1 — 2026-08-22 (Phase 0, executed inside article-loop Cycle 5)
- **Did:** `tools/extract_numbers.py` written; `numbers.md` generated
  deterministically from `final_results/` (all nine benchmarks + elastic
  net; leaderboards, paired W/T/L + Wilcoxon tables) plus the fixed
  reference values from the article loop (exact optima; the refuted
  −0.148 figure listed as forbidden). `check_article.py` still to be
  written when TeX editing begins.
- **Files:** tools/extract_numbers.py, numbers.md
- **TODO count (main.tex):** 14 (unchanged — no TeX edits yet, by design)
- **Tests:** check_research.py PASS
- **Next (writing loop):** remains gated on the experiment program
  finishing (article-loop LOG is authoritative). When writing begins:
  write `tools/check_article.py` first, then S1 per WRITING_PLAN.

## Cycle 2 — 2026-08-22 (S1: Setup section)
- **Did:** `tools/check_article.py` written (citation integrity, TODO
  ratchet with persisted high-water mark, ref/label and environment
  balance, forbidden-number list seeded with the refuted −0.148) — its
  first run immediately caught the refuted figure still in the scaffold's
  Section IV comments, which were replaced with the E2 final-protocol
  evidence. S1 written (single-writer): four paragraphs — encoding/schema,
  the loop, the generator and its one degree of freedom,
  implementations — replacing the comment outline; section retitled "One
  Loop, Shared Machinery". Independent review REFUTED the first draft's
  cross-validation paragraph as overscoped (the R↔Python side-by-side
  covers only func2C permissive + a reconstructed historical flip, not
  the paper's restricted variant or other benchmarks); rewritten with
  accurate scope ("Two implementations, cross-checked where they
  overlap"), and the EI parameter-freeness claim narrowed to "no
  acquisition-side exploration weight". All other technical statements
  verified against the R and Python code line-by-line by the reviewer.
- **Files:** ../../main.tex (S1 + Section IV comment block),
  tools/check_article.py, tools/article_state.json
- **TODO count (main.tex):** 14 (S1's section TODO resolved; one optional
  figure TODO added; net 0 — ratchet holds)
- **Tests:** check_article.py PASS; check_research.py PASS
- **Next (writing loop):** S2 (The Oracle-Ceiling Audit): definition
  environment, the E2 table (from exp02 ANALYSIS, canonical), gap-vs-pool
  figure decision deferred to full-pass; then S3 (Duplicate Leakage) from
  E2 H3/H4 + E3 H2. Same single-writer + review discipline.

## Cycle 3 — 2026-08-22 (S2 + S3: oracle audit and duplicate leakage)
- **Did:** S2 written: Definition environment (amsthm added to preamble),
  properties/cost paragraph, the E2 generator-ceiling table (tab:oracle),
  and the cautionary-note paragraph about the dropped figure. S3 written:
  mechanism paragraph (encoding-space uncertainty), ceiling-vs-below-it
  paragraph, and the dedup-cost table (tab:dedup, from E3). Review
  REFUTED two load-bearing overclaims, both fixed: (1) Definition 1's
  unqualified "upper-bounds any surrogate's trajectory" is not a theorem
  because pools are history-dependent — replaced by an explicit remark
  (exact per pool; empirical-only at trajectory level; "diagnostic
  yardstick, not a proved envelope"); (2) "traced to small-pool
  territory" softened to "consistent with", matching the sources'
  hypothesis-only status. Minors fixed: K5 forward-pointer added (real
  weak surrogates showed no generator effect — Sec. VI ref), TPE figure
  marked configuration-conditional with mean/median labels, table header
  clarified, Definition cross-referenced. All table numbers verified
  digit-for-digit by the reviewer against E1/E2/E3 canonical analyses.
- **Files:** ../../main.tex (S2, S3, preamble, labels)
- **TODO count (main.tex):** 13 (ratchet down from 14)
- **Tests:** check_article.py PASS; check_research.py PASS
- **Next (writing loop):** S4 (Consequences for Surrogate Comparisons,
  sec:conseq): the recast punchline per K3/K5 — machinery repair changes
  what the comparison measures; BASS's mixed-space failure localizes to
  the surrogate (GP/TPE win through the identical loop, thesis numbers
  from numbers.md); the E3 generator-null for weak surrogates; the
  E8-supported decay claim K10 WITH its mandatory disclosures (E7
  footnote + sequential-design note). Then S5 (protocol checklist).

## Cycle 4 — 2026-08-22 (S4: Consequences)
- **Did:** S4 written: the recast punchline (repair changed what the
  comparison measures, not who wins — tab:thesis with the final-protocol
  W/T/L and Wilcoxon values; BASS's mixed failure localizes to the
  surrogate with GP/TPE winning through the identical loop), the
  categorical capability paragraph (exact optimum every seed; tie
  structure vs win counts), the generator-axis-for-real-surrogates
  paragraph with the K10 decay claim and its mandatory dual-disclosure
  footnote (E7 composite + sequential design + non-monotone residual),
  and the qualitative surrogate-attributability close. Review UPHELD —
  all table values and roundings exact, K10 disclosures judged fully
  satisfied, narrative consistent with the thesis Conclusions — with two
  clarity minors fixed: the dedup-GP cross-implementation linkage now
  named explicitly (Python GP-EI, not thesis GP-BO), and the companion
  diagnostics pointer made concrete.
- **Files:** ../../main.tex (S4)
- **TODO count (main.tex):** 12 (ratchet down from 13)
- **Tests:** check_article.py PASS; check_research.py PASS
- **Next (writing loop):** S5 (A Protocol for Machinery-Controlled
  Comparisons): the boxed checklist with rationale-and-cost per item,
  the machinery-disclosure table template, MCBO differentiation per the
  novelty check's must-cite notes. Then S6 (Experiments: report the
  completed matrices; queue the BASS/E4 cells for the user machine with
  exact commands, no silent scope cuts).

## Cycle 5 — 2026-08-22 (S5 + S6: protocol and experiments)
- **Did:** S5 written: five-item checklist with rationale-and-cost per
  item, the machinery-disclosure convention, and tab:disclosure filled in
  for this pipeline. S6 written: protocol summary, the completed
  experimental matrix with run counts (700/750/2450 + thesis run),
  pre-registration statement, and the open-BASS-cell paragraph (honest,
  no extrapolation). Review REFUTED one blocker, fixed: the opening
  sentence had extended the budget-80/25-seed protocol over the two real
  tasks, which ran at their own scales (Elastic Net 100/50; nlp_hpo
  60/25 — the repo README flags both) — restructured to state the scales
  explicitly. Minors fixed: H4's failure now cross-referenced to Sec V
  as well as VI; the pre-registration claim qualified (git-verifiable
  for the later experiments; the first two entered the record with their
  results); checklist item 4's "hid both" softened to hid-one/
  understated-other. Everything else upheld: run counts, disclosure
  table rows, citations, 11^6 figure, checklist rationales consistent
  with the corrected Definition framing.
- **Files:** ../../main.tex (S5, S6)
- **TODO count (main.tex):** 10 (ratchet down from 12)
- **Tests:** check_article.py PASS; check_research.py PASS
- **Next (writing loop):** S7 (Introduction sharpen + Related Work
  complete): fold in the novelty check's must-cite list and
  differentiation notes (../novelty_check/REPORT.md) — MCBO prominently,
  Tripp & Hernández-Lobato, Daulton PR, Luong, Wilson, EvoGO,
  benchmarking-rigor cluster; extend references.bib (subagent drafts
  BibTeX from the verified sources, check_article gates the keys);
  resolve the intro's TODO paragraphs and the related-work TODO. Then
  S8 (Discussion/Limitations; abstract headline numbers; title
  recommendation for the user; acknowledgments/data availability).
