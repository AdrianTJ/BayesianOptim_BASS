# Research log for the machinery-confound article

Working folder for background research supporting `article_bo_machinery/`.
Every research task gets its own subfolder with a report and the raw
material it was synthesized from, and is committed to this branch so the
trail is tracked in git.

| Folder | Question | Status |
|---|---|---|
| `novelty_check/` | Is the article's contribution actually new? Prior-work sweep over the oracle-ceiling audit, machinery-confound demonstrations, and machinery-controlled comparison protocols. | done — see `novelty_check/REPORT.md`: novel as a combination; five near-misses verified and downgraded; must-cite list produced |
| `article_loop/` | **The master loop**: research → compile findings & decide → run experiments → analyze → re-center claims → repeat, with writing as the terminal phase. Plan in `article_loop/PLAN.md`, claim ledger in `article_loop/CLAIMS.md`, record in `article_loop/LOG.md`, experiments in `article_loop/experiments/`. | done — experiment program complete (E4/BASS cells queued for the author's machine); loop stopped at its normal end |
| `headline_uplift/` | What would make the paper a venue headline? Three-stream research (success anatomy of headline benchmarking papers; in-the-wild audit feasibility across 9 deployed BO libraries; theory upgrades) synthesized into the second loop's plan: audit the wild, release `bo-audit`, add the theory appendix. | planned — awaiting author go |
| `writing_loop/` | The writing phase of the article loop (single-writer prose, worker/verifier split, S1–S8 order, deterministic checks). Activates after the experiment program converges. Plan in `writing_loop/WRITING_PLAN.md`, record in `writing_loop/WRITING_LOG.md`. | done — full draft, Phase-2 three-lens review clean; remaining TODOs are author decisions |

Method note: searches, source-fetching, and claim assessment are fanned out
to subagents; findings are then reviewed and synthesized by the supervising
session before being written up here. Reports cite the sources they rest on;
raw per-source assessments are kept alongside each report.
