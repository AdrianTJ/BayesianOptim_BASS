# Novelty check: is the machinery-confound article actually new?

**Date:** 2026-08-21
**Question:** Does prior work already do what `article_bo_machinery/` claims as
its contributions — (C1) the oracle-ceiling audit, (C2) the demonstration that
acquisition-optimization machinery dominates surrogate comparisons in
mixed/categorical BO, (C3) the machinery-controlled comparison protocol?

**Method:** Five parallel web-search agents (angles: acquisition-optimization
importance; BO benchmarking critiques; mixed/categorical BO machinery;
oracle/perfect-surrogate baselines; duplicate-evaluation handling) surfaced 39
unique sources. The top 15 were fetched and rated for novelty threat; every
medium/high rating was adversarially re-verified by a second agent instructed
to refute it. The supervising session then reviewed all verdicts, audited the
24 unassessed sources for missed threats, and independently checked the two
that touch the article's specific mechanisms (Luong et al. 2019; Audet et al.
2026). Raw per-source assessments and verdicts: `sources.json`; the exact
fan-out script: `workflow_script.js`.

---

## Verdict

**The article's core contribution appears to be novel, but its novelty is in
the combination and the diagnostic, not in any single ingredient.** No source
was found that (a) substitutes the true objective for the acquisition score in
a pool-based BO loop to produce a per-machinery-variant performance ceiling,
(b) uses that audit to A/B candidate generators and dedup rules, or (c)
proposes a machinery-controlled protocol for cross-surrogate comparisons in
mixed/categorical spaces. Five sources were initially rated medium threats;
all five were downgraded on adversarial verification, in two cases after
direct inspection of the papers' code repositories. Every ingredient, however,
has a nearby precedent that the article **must cite and explicitly
differentiate** — most urgently MCBO, which already argues the general
"uncontrolled primitives confound comparisons" thesis at the framework level.

## The five verified near-misses (all downgraded)

**1. MCBO — Dreczkowski, Grosnit & Bou Ammar, NeurIPS 2023 D&B
([arXiv:2306.09803](https://arxiv.org/abs/2306.09803)).** *The closest prior
work; the article's biggest positioning risk.* A modular mixed/combinatorial
BO framework whose stated motivation is that "papers introducing a solution
for a single MCBO primitive often omit benchmarking against baselines that
utilize the same methods for the remaining primitives" — i.e., the
machinery-confound thesis at framework level — and which already provides
mix-and-match primitives held fixed across comparisons plus Friedman +
post-hoc Wilcoxon testing. **Why it doesn't pre-empt:** verified by grepping
the actual codebase (`huawei-noah/HEBO`, MCBO subdirectory): no oracle mode or
true-objective substitution anywhere (C1 absent); its local-search optimizer
is nominal/ordinal-only and mixed spaces route through a different optimizer,
so the categorical-flip pathology is never analyzed (C2a absent); its dedup is
generic pymoo plumbing, never framed or quantified as an encoding-vs-decoded
budget-waste confound (C2b absent); its statistics are rank-based over 47
combinations, not paired per-seed win/tie/loss with shared initial designs,
and there is no oracle gate or budget-solvability rule (C3 differs).
**Article must:** cite MCBO prominently in Related Work, credit the shared-
machinery and Wilcoxon ideas, and position C1 (the oracle-ceiling gate) plus
the two named, measured failure modes as what MCBO lacks.

**2. Tripp & Hernández-Lobato 2024, "Diagnosing and fixing common problems in
BO for molecule design" ([arXiv:2406.07709](https://arxiv.org/abs/2406.07709)).**
*Closest prior articulation of "machinery masquerading as a surrogate
result."* Shows that fixing three pipeline pitfalls — including "inadequate
acquisition function maximization" — makes a basic GP-BO best-in-class on the
PMO benchmark, implying prior rankings there were pipeline artifacts. **Why it
doesn't pre-empt:** the diagnosis is indirect (a bigger GA search improves
scores), not an oracle-substitution audit; the domain is pure combinatorial
molecular-graph search with no continuous coordinates, so neither of C2's
failure modes can even arise; a single before/after fix, no cross-surrogate
protocol. (Verified via its companion repo `AustinT/basic-mol-bo-workshop2024`.)

**3. EvoGO ([arXiv:2508.00380](https://arxiv.org/abs/2508.00380)), "Real Eval"
ablation.** *Convergent prior use of the oracle-substitution idea.* Replaces
predicted objective values with ground truth to estimate its framework's
performance ceiling. **Why it doesn't pre-empt:** the substitution happens in
the training loss of its own generative proposal model, not in the
candidate-selection step of a pool-based BO loop; continuous-only benchmarks;
a one-off self-ablation, never a general diagnostic or a machinery A/B.
(A similarly narrow ground-truth ablation appears in "Explainable Bayesian
Optimization", [arXiv:2401.13334](https://arxiv.org/abs/2401.13334).)
**Article must:** cite as independent precedent for the substitution pattern
and frame C1 as generalizing it into a machinery-isolation audit.

**4. Garrido-Merchán & Hernández-Lobato 2020
([arXiv:1805.03463](https://arxiv.org/abs/1805.03463)).** *Earliest clear
statement of the C2b mechanism:* continuous-relaxation-plus-rounding makes the
acquisition optimizer repeatedly propose points that decode to
already-evaluated designs. **Why it doesn't pre-empt:** treats it as a
modeling defect of one method, fixed by a bespoke kernel transformation;
no budget-waste quantification across surrogates, no audit, no protocol.
Already cited in the article — the differentiation paragraph should be made
explicit. **Same lineage, also must-cite:** Luong et al. 2019, *Discrete-BO*
([Springer](https://link.springer.com/chapter/10.1007/978-3-030-35288-2_38)),
which names the same stuck-repeating-observations failure and fixes it by
manipulating acquisition exploration and GP length-scale — again a
single-method fix, not a protocol (independently verified this sweep).

**5. Daulton et al. 2022, Probabilistic Reparameterization
([arXiv:2210.10199](https://arxiv.org/abs/2210.10199)).** *Closest prior
empirical evidence for the spirit of C2a:* diagnoses relaxation-then-round
acquisition optimization as a biased, degenerate approximation that caps
performance regardless of the acquisition function on top. **Why it doesn't
pre-empt:** verified via `facebookresearch/bo_pr` source — the surrogate is a
GP throughout (never a cross-surrogate comparison); the mechanism is rounding
bias in gradient-based optimization, not a local-search neighborhood
restriction; the remedy is a new optimizer, not a diagnostic; the only dedup
logic is a within-batch uniqueness constraint. **Article must:** cite as key
C2a-adjacent precedent.

## Related work the article should add (currently missing from references.bib)

Beyond the five above, the sweep surfaced related work strengthening the
paper's framing, none of it threatening:

- **Acquisition-optimizer importance (continuous):** Wilson et al. 2018,
  "Maximizing acquisition functions" ([arXiv:1805.10196](https://arxiv.org/abs/1805.10196));
  "Unleashing the Potential of Acquisition Functions in High-Dimensional BO"
  ([arXiv:2302.08298](https://arxiv.org/abs/2302.08298)); "An Empirical Study
  of BO: Acquisition Versus Partition" ([JMLR 2021](https://www.jmlr.org/papers/v22/18-220.html)).
- **Benchmarking rigor:** Turner et al. 2021 ([arXiv:2104.10201](https://arxiv.org/abs/2104.10201));
  HPOBench ([arXiv:2109.06716](https://arxiv.org/abs/2109.06716));
  carps ([arXiv:2506.06143](https://arxiv.org/abs/2506.06143));
  the discrete-sequences BO survey/benchmark ([arXiv:2406.04739](https://arxiv.org/abs/2406.04739)),
  which standardizes initial designs across methods;
  "Pitfalls and Best Practices in Algorithm Configuration" ([arXiv:1705.06058](https://arxiv.org/abs/1705.06058)).
- **Candidate-generator design for categoricals (C2a lineage):** CatMADS_GP /
  surrogate-based categorical neighborhoods, Audet et al. 2026
  ([arXiv:2603.27839](https://arxiv.org/abs/2603.27839)) — non-BO (MADS), but
  designs exactly the kind of categorical neighborhood structure whose absence
  C2a documents; Adaptive Local BO over discrete variables
  ([arXiv:2012.03501](https://arxiv.org/abs/2012.03501)); Bounce
  ([arXiv:2307.00618](https://arxiv.org/abs/2307.00618)); BODi
  ([arXiv:2303.01774](https://arxiv.org/abs/2303.01774)).

## Recommended positioning (one paragraph the article needs)

The article should state its novelty as: *prior work has separately argued
that BO benchmarking needs controlled primitives (MCBO), that pipeline defects
masquerade as model results (Tripp & Hernández-Lobato), that rounding-based
candidate generation wastes budget on duplicates (Garrido-Merchán; Luong;
Daulton), and that ground-truth substitution can estimate a ceiling (EvoGO's
ablation). What no prior work does is combine these into a surrogate-free,
per-machinery-variant oracle-ceiling audit for pool-based mixed/categorical BO,
demonstrate with it two named, quantified failure modes that flip surrogate
rankings, and derive a machinery-controlled comparison protocol.* The "TODO:
verify none performs an oracle-style audit" comment in the Related Work
section can now be resolved affirmatively — none of the 39 sources swept does.

## Caveats

- This environment's egress policy blocks arxiv.org, OpenReview, Semantic
  Scholar, and proceedings sites, so most assessments rest on abstracts,
  GitHub companion repositories, and consistent search-index summaries rather
  than full texts. The MCBO and Daulton verdicts are the strongest (direct
  code inspection); the Tripp and EvoGO verdicts rest on repos + abstracts.
  ~~Spot-check the full PDFs of MCBO, Tripp & Hernández-Lobato, and Daulton
  et al. before submission.~~ **DONE 2026-08-24:** all three read end-to-end
  from arXiv HTML; all downgrades confirmed; see
  `FULL_TEXT_VERIFICATION.md` in this directory.
- The sweep is a 39-source web pass, not an exhaustive systematic review;
  workshop papers and very recent preprints (2026) are the likeliest blind
  spot. Re-run closer to submission.
- 24 lower-priority sources were not agent-assessed; all were reviewed at
  title/relevance level by the supervising session and the two
  mechanism-adjacent ones were checked independently (Luong 2019; Audet 2026).
