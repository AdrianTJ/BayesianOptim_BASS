# E8 — Independent adversarial review (summary)

**Reviewer:** Sonnet 5 agent, read-only. **Verdict: UPHELD.**

Verified independently: seeds 1026–1050 genuinely fresh (checked against
every prior experiment's results.csv); the noise-rng benchmark-index fix
real; decision-rule code faithful to DESIGN; pairing genuinely seed-aligned
in every cell (checked programmatically, not just equal-length); **all 24
σ-cells recomputed from raw data with zero mismatches**, including the four
flagged non-anchor significant cells. Crucially, the reviewer
git-verified pre-registration: the commit containing DESIGN.md and the
decision code predates the data, results were still uncommitted at review
time, and the numeric thresholds trace back unchanged through the Cycle-4
authorization to E7's original frozen design — "the numbers were never
data-tuned."

On the multiplicity question the reviewer judged the two-step sequential
design fair and disclosed (fresh seeds + committed criteria + hard
stopping rule as real controls) but required the E7→E8 clause-selection
history be named plainly in Threats rather than reconstructed from three
files — added. Two further minors: tighter claim wording so "decays to
undetectable" cannot be read as monotone vanishing (adopted verbatim), and
an "untested" tag on the mechanistic speculation (added). The E7-footnote
requirement for any article use of K10 is recorded and will be gated at
the writing stage by check_article conventions.