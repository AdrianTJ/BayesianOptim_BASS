# Anchor-paper full-text verification (gate satisfied 2026-08-22)

Author re-uploaded both PDFs after the first upload was lost to a
container recycle; copies are secured in-repo under `anchors/` (force-
added past the `*.pdf` gitignore). Full texts read via pdftotext; the
PLAN's hard gate ("no appendix citation without a full-text check") is
now satisfied for both. Citations added: `kim2025inexact`,
`bloor2026b3o` (Related Work "Acquisition optimization as a first-class
concern" + Appendix Lemma A remark + Prop C(b)).

## arXiv:2506.11831 — VERIFIED

**"Bayesian Optimization with Inexact Acquisition: Is Random Grid Search
Sufficient?"** — Hwanwoo Kim (Duke), Chong Liu (SUNY Albany), Yuxin Chen
(U Chicago). UAI 2025, PMLR v286. arXiv v1 13 Jun 2025.

Read confirms: first theoretical study of inexact acquisition-function
maximization in BO; defines an inaccuracy measure for acquisition
solutions; cumulative-regret bounds for GP-UCB and GP-TS under imperfect
maximizers; headline result — random grid search with linearly growing
grids (|X_t| = Θ(t)) achieves sublinear regret, relaxing prior t^{2d}
grid requirements; extra regret term Õ(T^{(d−1)/d}).

Relevance as cited: their random grids are **exogenous pool sequences**
in exactly Lemma A's sense (pool drawn independently of selection
history), so our per-realization ceiling complements their asymptotic
regret rates; their inexactness formalism is the regret-side counterpart
of our machinery-ceiling measurement. No claim in our paper depends on
their theorem statements beyond this positioning.

## arXiv:2606.30228 — VERIFIED

**"B3O: Scalable Boltzmann Batch Bayesian Optimization"** — Maximilian
Bloor (Imperial College London), Liyuan Xu, Hrvoje Stojic, Victor
Picheny (Secondmind). arXiv v1 29 Jun 2026.

Read confirms: batch generation reframed as sampling from the Boltzmann
density exp(λ_t α_t(x)) of the acquisition; inverse temperature λ
interpolates uniform (λ→0) ↔ acquisition argmax (λ→∞); theorem —
finite-time cumulative regret for single-query Boltzmann sampling under
a UCB acquisition recovers GP-UCB rates up to a negligible additive
term (Laplace-style 1/√t gap bound under a λ_t schedule); sampler-,
surrogate-, acquisition-agnostic; experiments include a mixed
continuous–discrete configuration task.

Relevance as cited: their fixed-temperature Boltzmann law over a finite
candidate set **is** Prop C(b)'s Gumbel/softmax selection law with
λ = 1/σ; the guidance dial measures on our pools the per-step
selection-quality cost their analysis bounds on the regret side. Our
Prop C(a) (Gaussian case) is not in their paper; no dependency.

## Notes

- The author's earlier external ID lookup reported both IDs missing;
  both are in fact valid (the June-2026 ID plausibly trips index
  checkers). Recorded in AUTHOR_TODO item 2, now closable.
- THEORY.md's header note ("anchors excluded, queued for author") is
  superseded by this verification; the appendix now cites both.
