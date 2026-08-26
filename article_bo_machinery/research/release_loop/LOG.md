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

## Item R1 — 2026-08-26
- **Phase:** bo-audit README headline example — complete.
- **Did:** The "Use" example advertised `revisits: 16 / unique: 64` on a
  space carrying a continuous `C` dimension, where exact revisits are
  near-impossible; run verbatim it returned `0 / 80`. A Sonnet 5 subagent
  replaced it (README.md only, source untouched) with a purely
  categorical 5x5x5 = 125-combination space at budget 80 — the paper's
  cat-Ackley d3/L5 regime — plus a uniform-random control, with all
  documented output pasted from real measured stdout.
- **Verification refuted the first attempt (worker != verifier, working
  as intended).** The agent's first rewrite was mechanically correct —
  scope respected, output literally true and reproducible — but used a
  9-combination space at budget 80, reporting 72 revisits and
  `revisit_frac: 0.9`. The pigeonhole baseline there is 71.0, so the
  excess over unavoidable collisions was +1.0 (1.2% of budget), and
  uniform random on the same space also returns 71. That example would
  have advertised what reads as 90% waste on a space where this
  project's own metric says there is none, and where TPE is
  indistinguishable from random — a worse defect than the one being
  fixed, and precisely what a skeptical reviewer would compute first.
  Sent back with the requirement to use a regime with real headroom and
  an explicit instruction to report rather than tune the space if the
  excess came out near zero again.
- **Second attempt verified and accepted:** pigeonhole(125, 80) = 20.74
  against a measured 47 revisits (excess +26.3, 32.8% of budget) and a
  random control at 19 (excess -1.7). Every prose figure recomputed
  independently; the `(below)` cross-reference checked; scope confirmed
  as README-only; the 10-test suite still passes.
- **Gate committed:** `tools/verify_readme_example.py` extracts the
  README's own code block, runs it twice for determinism, diffs the
  documented comment against real stdout, fails when excess over
  pigeonhole is under 5% of budget, and fails when the sampler is not
  clearly above a random control. Validated against the known-bad
  version first — it passed that version's truthfulness check and
  failed its substance check, which is the behavior required. A gate
  that cannot fail is worthless.
- **Next:** Item R2 = the missing LICENSE. `README.md:179` carries a
  markdown link pointing at `LICENSE`, `bo-audit/README.md` says MIT and
  `bo-audit/pyproject.toml` sets `license = "MIT"`, but no LICENSE file
  exists anywhere in the tree, so collaborators are granted nothing.
  Delegate to a Sonnet 5 subagent: add a canonical MIT LICENSE at the
  repository root, copyright Adrian Tame Jacobo, year 2026. Out of
  scope for that agent: every other file, and any change to the license
  *identifier* in pyproject (it is already correct). Verify the text
  against the canonical MIT wording word for word, confirm the README
  link resolves to a real path, and confirm the SPDX identifier agrees;
  then commit and move to R3.
