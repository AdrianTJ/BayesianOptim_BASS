# E3 — Independent adversarial review (summary) and response

**Reviewer:** Sonnet 5 agent, read-only, did not write the code.
**Verdict: REFUTED on interpretation; computations upheld.**

What was upheld: EI formulas (GP and RF), the TPE curve alignment and
revisit-counter semantics (verified equivalent to `run_bo`'s), closure and
filtering correctness in run_e3.py, faithfulness of the H1–H4 check code to
DESIGN.md, and **every recomputed number in ANALYSIS.md (zero
mismatches)**. The reviewer also independently confirmed, from
`code_files/R/tpe.R`, that the thesis's TPE was seeded with the shared
maximin-LHS initial design via `add_trial` — substantiating the leading
candidate cause for the H4 discrepancy.

**The blocker:** the first draft reframed H1's failure as a positive
"regime" finding (surrogate-to-ceiling gap as a diagnostic, drafted as
K10). The reviewer correctly identified this as (1) *confounded* — E2's
oracle and E3's surrogates differ simultaneously in distance-to-ceiling and
in search type, so "distance is the operative variable" cannot be
distinguished from "the generator effect is oracle-specific, period"; and
(2) *HARKing* — DESIGN.md's pre-registered fallback for an H1 failure was
the deflationary "the audit's ceiling overestimates machinery sensitivity",
and the draft escalated past it into a new named contribution.

**Response (all applied):**
1. ANALYSIS.md rewritten to the pre-registered fallback framing; the
   regime idea demoted to an explicit hypothesis pair (H-regime vs
   H-oracle-specific) with the de-confounding experiment designed: a
   noise-corrupted oracle (`score = −f + σ·ε`) sweeping guidance quality
   within the identical pool-argmax search type. Next cycle runs it.
2. K10 in the ledger demoted from supported to hypothesis; K5's
   generator-axis clause reworded to "effect not detected", per fallback.
3. Minor findings fixed/disclosed: GP `random_state=0` disclosed in
   Threats; "TPE revisits init-insensitive" softened to plausible-untested;
   `combo_of`'s lexicographic key sort (latent d≥10 bug) fixed numerically
   in surrogates.py.
