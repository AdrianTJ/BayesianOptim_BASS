# E2 — Independent adversarial review (summary)

**Reviewer:** Sonnet 5 agent, read-only, did not write the code.
**Verdict: UPHELD.** Every independently recomputed number (H2 gap table,
H4 revisit median/range/mean, the exact Wilcoxon floor p=5.9604645e-08, row
counts, random references, H5 tallies) matched ANALYSIS.md with zero
transcription errors. The `dedup='encoding'` mode was verified faithful to
the pre-fix R loop by diffing commits 1ecc5a1 → 0731709; the revisits
counter was verified mode-independent and pre-append; E1 behavior confirmed
unchanged by default.

Minor findings, addressed in ANALYSIS.md after review:

1. **Cross-benchmark RNG seed reuse** — `seed*100+arm_ix` repeats across
   benchmarks. Harmless (all comparisons are within-benchmark); now
   documented. Future runs should fold a benchmark index into the seed.
2. **H2's second pre-registered clause** (flip's best@10 degrades faster
   than keep's) was shown in the table but not asserted in code. Recomputed:
   func2C keep@10 degrades 0.0170 vs flip 0.0695 (≈4×, strong); func3C
   0.1852 vs 0.1944 (≈5% margin — weak). ANALYSIS now states this
   quantitatively; the H2 headline rests on the final-gap clause.
3. **H3 edge case** — SE=0 cells auto-pass; in the data those cells also
   have diff=0 (bit-identical convergence), so unexercised; noted.
4. **"Exact optimum" precision** — keep's per-seed finals carry a ≤3×10⁻⁵
   residual at n_cand=50; "exact" holds only to the table's 4 decimals.
   Wording adjusted.
5. **H5 near-tautology disclosed** — on pure-categorical schemas the keep
   and flip code paths are functionally identical (both force ≥1 flip;
   there is no continuous coordinate to refine), so H5 is a consistency
   check on the harness, not an independent discovery. The substantive
   pure-cat evidence (oracle clears the pool) stands.
