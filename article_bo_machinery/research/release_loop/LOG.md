# Release loop log

Append-only; one entry per item, newest last; "Next:" is the
fresh-context resume instruction. Plan: `PLAN.md`. Claim ledger:
`../article_loop/CLAIMS.md` (this loop adds no claims — it changes
packaging, documentation and scope language only).

Discipline reminder for any session resuming here: a Sonnet 5 subagent
makes each edit, this session verifies independently by re-running, and
committed results / DESIGN files / paper numbers are read-only.

## Item R0 — 2026-08-26
- **Phase:** pre-registration — complete.
- **Did:** Wrote `PLAN.md` before any R1–R7 change, per the loop
  precedent that a plan is committed ahead of the work it governs.
  Recorded the pre-loop verification pass that motivates the seven
  items: every H1 headline number, the Z = 3 of 4 ranking change, and
  the figure medians re-derived with independent code and matching
  exactly; two apparent discrepancies traced to the auditor's own
  errors (wrong ranking population; a naive pigeonhole formula that
  collapses at K = 5^25) and kept on the record; bit-exact replay shown
  to fail while the reported statistics hold (median revisits 16,
  median unique 64 reproduced; 3 of 25 individual runs matching).
  Restarted the working branch from current `main` after the branch
  consolidation retired its predecessor.
- **Next:** Item R1 = the bo-audit README example. Delegate to a
  Sonnet 5 subagent: replace the "Use" snippet's mixed space (whose
  continuous `C` dimension makes exact revisits near-impossible) with a
  purely categorical space, run it, and paste the measured output.
  Out of scope for that agent: every other file, and any change to
  `core.py` counting logic. Verify by running the new snippet verbatim
  in a clean venv and diffing its stdout against the README's claimed
  output; only then commit and move to R2.
