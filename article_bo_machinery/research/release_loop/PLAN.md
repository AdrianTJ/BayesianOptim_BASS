# Release loop — from finished draft to shareable artifact

**Question this plan answers:** what must be true before the article and
`bo-audit` can be handed to outside colleagues without a caveat email.

**Author directive (2026-08-26):** the article is going to colleagues to
gauge collaboration interest. It must be defensible and reproducible on
arrival. Work slowly, one item at a time, delegate edits to Sonnet 5
subagents and verify their work independently — "the auditing and
verification step is as important, if not more, than the actual code
writing." This is an academic study making strong claims about packages
that hundreds of thousands of people depend on.

## Basis: what the pre-loop audit measured (2026-08-26)

Verification pass over the committed record, re-deriving every headline
number with independent code rather than the committed scripts:

- **The science reproduces.** H1's matrix size, per-cell medians, the
  3/6–2/6–1/6 waste headline, optuna-gp's 53/80 with ~27 unique, the
  349/350 mixed-space null, SMAC's 0-revisits-and-6-terminations, and
  the **Z = 3 of 4** ranking change all re-derived exactly. The
  `fig_dedup_audit` medians (0 vs 78/80) regenerate and match the
  caption. All three checkers PASS.
- Two apparent discrepancies were **the auditor's error, not the
  paper's**: a first Z computation returned 1 of 4 by ranking only the
  three re-run arms instead of the registered six-library population,
  and a naive pigeonhole formula collapsed at K = 5^25 where the
  committed `expm1`/`log1p` form is correct. Recorded here because the
  loop's rule is that refuted checks stay on the record.
- **Bit-exact replay fails.** Re-running pest_control × optuna-tpe × 25
  seeds from the released package reproduces the reported statistics
  exactly (median revisits 16, median unique 64) but only 3 of 25
  individual runs match. The objective is deterministic and unchanged
  since H1 and the runner path is identical; the drift is unrecorded
  transitive dependencies (numpy/scipy), which the raw results do not
  capture. The finding is robust; the run is not replayable.
- **The tool's front door is broken.** The README's flagship example
  advertises `revisits: 16 / unique: 64`; run verbatim it returns
  `0 / 80` — and must, since its example space carries a continuous
  dimension. Five of the six H1 benchmarks are unreachable from an
  installed package. There is no LICENSE anywhere despite three MIT
  claims.

## Scope: seven items, in this order

| # | Item | Why it blocks sharing |
|---|---|---|
| R1 | README example returns what it claims | First thing a colleague runs contradicts the docs and teaches the wrong mental model |
| R2 | LICENSE file | Three MIT claims, no grant; collaborators legally hold nothing |
| R3 | Article PDF builds from a clean clone | Blanket `*.pdf` ignore hides the one required figure |
| R4 | H6 sweep status recorded | 10,203 unanalyzed rows that narrow the headline read far worse discovered than disclosed |
| R5 | GH5 scope caveat in the article | A pre-registered hypothesis FAILED and the text does not say so |
| R6 | Environment freeze + honest replay note | Per-seed replay is impossible from recorded metadata |
| R7 | Package gaps (benchmarks, smac_runner, canonicalize, test path) | The released artifact cannot run the paper's own experiments |

R1–R4 are cheap and land first; R5–R7 touch the paper and the package
and carry the heavier verification burden.

## Loop mechanics — unchanged from the writing and headline loops

- **Worker ≠ verifier, always.** A Sonnet 5 subagent makes each edit;
  this session verifies independently and never accepts an agent's own
  report of success as evidence. Verification re-runs the thing.
- **Every subagent brief states what is out of scope.** Agents get one
  item, the exact files they may touch, and an explicit prohibition on
  touching anything else — above all on editing committed results,
  pre-registered DESIGN files, or any number in the paper.
- **Deterministic checks own the gate:** `check_article.py`,
  `check_research.py`, `check_experiments.py`, and the bo-audit test
  suite must pass before any commit. The TODO ratchet may not rise.
- **Numbers are re-derived, never copied.** Any figure quoted in prose
  is recomputed from raw results by this session before it is allowed
  to stand, including numbers taken from the pre-loop audit above.
- **One item at a time**, committed separately, logged per item.
- **No silent caps.** Anything deferred is named in LOG.md and in the
  close-out report.
- **Frozen artifacts stay frozen.** Committed `results.jsonl` / CSVs,
  DESIGN.md files, and the claim ledger's evidence rows are read-only
  for this loop; R5's caveat adds text, it does not restate a result.

## Risks, stated up front

- **An agent "fixes" a number.** The single most damaging failure mode
  available here: a well-meaning edit that adjusts a result to match
  prose. Mitigated by the read-only rule above, by diffing every
  agent's change against the committed tree before commit, and by
  re-running the checkers.
- **R7 changes package behavior.** Shipping the missing benchmark
  builders means moving code that produced published numbers. Any move
  must be verbatim and verified by re-running a paper cell to the
  median, not by inspection.
- **R5 overclaims from partial data.** GP arms are unrun, so the
  caveat must be scoped to the fast-arm evidence and say so.
- **No LaTeX or R in-container.** The PDF cannot be compiled here; R3
  is verified by clean-clone simulation and figure regeneration, and
  the actual `pdflatex` run stays an author task, named as such.

## Stop conditions

R1–R7 complete and independently verified, checkers PASS, working tree
clean and pushed → close-out report to the author with anything
deferred named explicitly. Stall counter and user-stop as always.
