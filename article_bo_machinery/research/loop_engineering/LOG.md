# Loop log

Append-only record of loop cycles. One entry per cycle, newest last, using
the template in `PLAN.md`. Cycle 0 is the setup itself.

## Cycle 0 — 2026-08-21
- **Question:** — (setup)
- **Did:** Created the loop plan (`PLAN.md`), this log, and the testing-leg
  checker (`tools/check_research.py`); registered the folder in
  `research/README.md`; started the self-paced loop.
- **Files:** PLAN.md, LOG.md, tools/check_research.py, ../README.md
- **Tests:** check_research.py PASS
- **Next:** Cycle 1 takes Q1 (loop architectures).

## Cycle 1 — 2026-08-21
- **Question:** Q1 (loop architectures)
- **Did:** Two Sonnet 5 research agents (architecture families; stop
  conditions and pacing — ~128k tokens, 53 tool calls), reviewed and
  synthesized by Fable 5 into `notes/01_loop_architectures.md`, together
  with an internal case study of the novelty-check run. Practice
  improvement applied: PLAN.md gained a stall counter (2 consecutive
  no-commit cycles stop the loop), from Magentic-One's progress-ledger
  pattern. Caveat recorded in the note: most publisher pages were
  egress-blocked, so several external claims rest on search snippets.
- **Files:** notes/01_loop_architectures.md, PLAN.md (Q1 checked, stall
  counter added), LOG.md
- **Tests:** check_research.py PASS
- **Next:** Cycle 2 takes Q2 (workflow graphs: pipeline vs. barrier
  semantics, verification motifs, barrier wall-clock cost). Primary
  internal evidence: the novelty-check script's one-barrier design and its
  journal timings; pair with one external sweep on DAG orchestration
  semantics.
