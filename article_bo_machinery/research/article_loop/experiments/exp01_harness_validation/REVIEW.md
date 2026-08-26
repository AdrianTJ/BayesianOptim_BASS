# E1 — Independent adversarial review (summary)

**Reviewer:** Sonnet 5 agent, read-only, did not write the code (27 tool
calls, ~115k tokens). **Verdict: UPHELD.**

What the reviewer verified independently (not by re-reading our claims):

- **Semantic fidelity:** every ported function checked line-against-line vs
  `code_files/R/` — decode/canonicalize/local_scale/generator split/Hamming
  moves/dedup tol/tie-break/Random arm/objective constants all [OK].
- **V2 non-circularity:** reran the 2M-sample tail MC themselves
  (P(f<−0.148)=1.418e-3, matching ours), and independently reimplemented the
  true historical k~U{1..min(3,n_cat)} generator from `git show 1ecc5a1`
  on fresh seeds: best@10 −0.1988, best@80 −0.2054 — corroborating that the
  recorded R "−0.148 at budget 10" is unreproducible from the committed code.
- **Exact optima proven analytically:** func2C = 2×camel_min/10 =
  −0.20632569 at (2,2), uniqueness proven (rosen/beale are ≥0 everywhere);
  func3C = 7×camel_min/10 = −0.72213991 at (2,2,1), uniqueness proven.
- **Pairing and V-check arithmetic** recomputed from results.csv — matched
  to 4+ decimals.

Defects found and fixed before commit (both re-run through V-checks):

1. **[fixed]** `hash(arm_name)%7` RNG seeding was process-salted →
   non-reproducible results.csv and possible arm-seed collisions (~39% of
   runs). Replaced by an explicit arm index; results regenerated
   deterministically. Verdicts unchanged.
2. **[fixed]** V6 compared random against `min` of the oracle arms while
   claiming "both"; now `max`. Empirically passed either way.
3. **[documented]** DESIGN.md's "flip = historical" label is imprecise: the
   true pre-fix generator flipped k~U{1..min(3,n_cat)} coords jointly. The
   `flip` variant is the *restricted class* (≥1 forced flip); the exact
   historical variant was tested separately in the V2 investigation and
   behaves the same (−0.199@10 / −0.2054@80).
