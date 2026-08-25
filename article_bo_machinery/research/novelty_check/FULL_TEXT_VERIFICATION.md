# Full-text verification addendum: the three closest prior works

**Date:** 2026-08-24
**Closes the caveat in REPORT.md / claim K8** ("full PDFs of MCBO/Tripp/Daulton
unchecked — egress"). All three papers were read end-to-end from their arXiv
HTML renderings; per-question verdicts below. Method: three independent
read-only verification agents, one per paper, each instructed to refute the
article's novelty claim (K8) against the full text; verdicts spot-checked
against the quoted section numbers.

**Result: K8 survives full-text verification on all three axes.** All three
downgrades from the original sweep are CONFIRMED. No source performs (a)
true-objective substitution inside a pool-based BO loop as a per-machinery
ceiling, (b) candidate-generator/dedup-rule isolation via such an audit, or
(c) a machinery-controlled cross-library protocol with combination-level
dedup and paired per-seed statistics. Each does contain material the Related
Work must credit and differentiate (per-paper lists below).

---

## 1. MCBO — Dreczkowski, Grosnit & Bou Ammar, NeurIPS 2023 D&B (arXiv:2306.09803, v3)

**Threat: LOW.** Closest framework-level precedent; the "uncontrolled
primitives" thesis is prior art and must be credited as such.

- **(a) Oracle substitution: NO.** The BO loop (Alg. 1, §3) always optimizes
  the acquisition α(x|D). Closest analogues are non-oracle diagnostics:
  §5.4 correlates GP model-fit quality with final regret ("we measure the
  Pearson correlation between the quality of a GP model fit, and the quality
  of the objective value attained after 200 iterations"); Fig. 2 aggregates
  mean ranks by primitive.
- **(b) Generator/dedup isolation: NO.** It varies whole acquisition
  optimizers (LS/GA/SA/HC/MAB-GD; §5.1–5.3, App. D.1) — "we can then
  aggregate the ranks across tasks, random seeds, and the BO primitives not
  under investigation" (§5.1) — but never candidate-generation strategy or
  duplicate handling as named factors. Deduplication is never mentioned
  (full-text grep: zero hits for duplicate/revisit/dedup/memoize).
- **(c) Revisit measurement: NO.**
- **(d) Protocol: spirit-adjacent, not equivalent.** §1: "papers introducing
  a solution for a single MCBO primitive often forget to benchmark against
  baselines that use the same methods for the remaining primitives…failing
  to fully highlight the merits of their proposed solution in a controlled
  setting." Remedy is modular reimplementation inside its own framework
  (BoBuilder, §4.3), average ranks with Friedman/Wilcoxon critical
  intervals, shared initial designs ("each BO algorithm suggests the same
  set of 20 uniformly sampled points", §5.1) — not a protocol imposed on
  deployed external libraries at their defaults.
- **Differentiate:** cite as the closest framework precedent; state that it
  benchmarks its own reimplemented primitive combinations via rank
  aggregation, never substitutes f for the acquisition score, never treats
  generator vs. dedup as separate factors, and reports no duplicate metric.

## 2. Tripp & Hernández-Lobato — arXiv:2406.07709 (v2)

**Threat: LOW.** Closest rhetorical precedent for "BO underperformance is
fixable implementation issues," but on molecule design, and explicitly
limited.

- **(a) Oracle substitution: NO.** Alg. 1 (§2) keeps x_i = argmax α_i(x; p_i(f̂)).
- **(b) Factor isolation: NO — self-admitted.** §5: "We also did not perform
  an ablation study, and therefore our results do not provide insight into
  how much each component of BO influences the overall result." Audited
  factors are surrogate hyperparameters (prior width σ=1.0, §4), fingerprint
  choice (count vs. binary Morgan, §4), and GA search budget (≈1000
  proposals per molecule chosen, §4) — not candidate-generator design or
  duplicate handling.
- **(c) Duplicate measurement: NO.** No revisit counting anywhere.
- **(d) Protocol: NO.** Table 1 baseline columns are copied from other
  papers ("Taken from [Gao et al. (2022)]", App. B); no shared machinery,
  no paired-seed statistics.
- **Scope disclaimer (useful for positioning):** §5: "what this paper
  presents should best be thought of as a very limited pilot study, rather
  than a full diagnosis of potential issues in BO."
- **Differentiate:** credit as supporting the confound thesis on PMO; state
  that its three audited factors are surrogate hyperparameters and GA search
  budget, that it proposes no checklist or protocol, and that it disclaims
  component attribution.

## 3. Daulton et al. — NeurIPS 2022 (arXiv:2210.10199, v1)

**Threat: LOW.** A new acquisition-optimization machinery (probabilistic
reparameterization) with an optimizer-isolated evaluation — the strongest
"machinery matters" precursor, and prior art the article must position
against carefully.

- **(a) Oracle substitution: NO.** Theory concerns maximizer equivalence
  between the probabilistic objective and the AF (Thm. 1 "Consistent
  Maximizers", §3.2); Fig. 1 (§2) illustrates AF over-estimation of a
  rounded candidate ("only 86% of the AF value of the true maximizer") —
  a single-problem illustration, not a per-machinery ceiling through a loop.
- **(b) Factor isolation: PARTIAL (optimizer only).** Sec. 6 holds surrogate,
  kernel, and AF fixed and varies only the acquisition optimizer ("We
  compare PR against two alternative acquisition optimization strategies:
  using a continuous relaxation… and using exact discretization with
  approximate gradients"). Duplicate handling is never a factor; PR's own
  sampler (Alg. 1, line 5: "Sample z_n ~ p(Z|theta_n)") has no dedup rule.
- **(c) Revisit measurement: NO.** The only duplicate mention is the §2
  motivating anecdote about a competitor ("discretization will result in a
  design that has already been evaluated and has zero AF value") — no rates
  for any method, including PR itself.
- **(d) Protocol: NO.** Standard equal-replication benchmarks ("mean for
  each method ± 2 standard errors across 20 replications", §6); baselines
  run native pipelines, some excluded or budget-reduced where unsupported
  ("Casmopolitan and HyBO are not run on Welded Beam and Oil Sorbent…";
  "we only run 60 BO iterations on SVM due to the large wall time").
- **Differentiate:** credit as establishing that AF-optimization quality is
  a load-bearing, often-failed component and that optimizer-only
  comparisons change outcomes; state that it proposes machinery but audits
  none, measures no revisits (its own sampler included), and benchmarks
  method-vs-method without a controlled cross-library protocol. PR's
  feasibility-guaranteed generator serves its own optimizer, not a neutral
  shared harness.

---

## Consequences for the article

1. The Discussion's verification-limit disclosure has been upgraded from
   "abstract and companion-code level" to full-text verification (main.tex,
   "Open cells and verification limits").
2. Related Work already cites all three; the per-paper differentiation
   points above are covered by the existing text's framing (MCBO as the
   framework-level precedent; Tripp as the diagnosis-genre precedent;
   Daulton cited for probabilistic reparameterization). No further text
   changes required beyond the disclosure upgrade.
3. K8 status remains **supported**, now with full-text evidence. The
   remaining unchecked surface from the original sweep (the two
   medium-threat sources verified only at code level: Luong et al. 2019 and
   Audet et al. 2026) was already independently checked by the supervising
   session per REPORT.md.
