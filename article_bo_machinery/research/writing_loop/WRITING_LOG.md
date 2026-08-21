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
