# E7 — Independent adversarial review (summary) and response

**Reviewer:** Sonnet 5 agent, read-only. **Verdict: REFUTED (first draft);
corrections applied in full.**

Upheld: pairing and arm-index structure (verified — zero duplicate cells,
full grid), fresh per-iteration noise implementing the design's model,
dial decision-rule code faithful to DESIGN, TPE↔E3-Random seed matching,
injected-trial encoding correctness, and **every recomputed number (all 24
W/p cells, quoted G values, all four TPE win counts — zero mismatches)**.
The dial's composite INCONCLUSIVE verdict and the refusal to upgrade K10
were judged honest and correctly restrained; the "W-pattern consistent
with H-regime in 4/4 cells" reading was judged fair reporting of a
pre-registered sub-criterion, not a smuggled reinstatement.

Blockers found and corrected:

1. **K-TPE "closure" retracted.** DESIGN's rule was joint (≥17/25 on both
   benchmarks); func3C hit 16/25, so the pre-registered outcome is the
   fallback branch ("not resolved"). The first draft declared a qualified
   "closes" anyway — the same unauthorized post-hoc softening E3's review
   blocked. ANALYSIS and the ledger now record K-TPE as **open,
   narrowed**.
2. **G-clause count corrected:** fails in 2/4 cells (both func2C pools),
   not 3/4; the func3C "passes" are themselves noise-driven (sign flip /
   modest shrink) and are now described as such.

Minors disclosed: `n_startup_trials=0` vs tpe.R's default 10 (pro-TPE by
one trial on func2C, n0=9<10); dial noise rng omits the benchmark index
(noise streams shared across benchmarks at matching seed/arm — cosmetic,
no reported statistic relies on cross-benchmark independence).

**Process note (two consecutive REFUTED drafts):** both refutations were
interpretation-layer, both in the direction of overclaiming from
partially-met criteria, and both caught by the worker≠verifier split. The
corrective rule adopted going forward: a pre-registered composite
criterion is evaluated by its letter first, verdict stated first, and only
then may sub-criteria be discussed — and any ledger status change must
quote the criterion it satisfies.