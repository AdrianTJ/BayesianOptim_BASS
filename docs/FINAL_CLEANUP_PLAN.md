# Final cleanup and editing plan — v2

Updated 2026-07-15 on branch `written-corrections`, after the mechanical pass
(7ba7a96), the citation pass (a9d7932), and the stale-file sweep. Supersedes
the v1 checklist (its surviving items are folded into Parts C–E).

**How this file works.** Part A is your pen: every item that needs real
writing or a decision, with Martin's note verbatim and a `DIRECTION:` block.
Write into those blocks — prose, bullets, or just `keep as is` / `skip` — and
hand the file back; I turn whatever you write into LaTeX (wrapped in
`\green{}`) and update the tracker. Part B is new citations awaiting your
validation. Parts C–E are production tasks, repo hygiene, and verification —
none of them need your prose.

IDs (`C###`, `T###`, `N##`, `W##`, `D#`) match `written_files/corrections.json`,
which carries the full status ledger.

Progress at a glance: **227 markup items → 158 resolved, 12 decisions below,
~44 writing items grouped into 26 work packages below, 2 figure checks.**

---

## Part A — Your pen: decisions and real writing

### A1. Decisions (quick — tick or one-liner)

**D1 (C008)** `GP.tex:26` — Martin on the Billingsley footnote: *"Needed?"*
The footnote is currently kept.
> DIRECTION: keep / drop:

**D2 (C047)** `GP.tex:345` — Martin on §1.4.1 "Preamble to GPR: A Rehash on
Regression Analysis": *"Better Before?"* — i.e. should the regression rehash
come before the new marginal-likelihood material (or even before §1.4)?
Current order: §1.4 opener → minimization development → preamble → theory.
> DIRECTION: keep order / move where:

**D3 (C053, C054, C055)** `GP.tex:370-372` — three related notation calls in
one block: (a) Martin: *"ℙ or density? f continuous or discrete"* — the block
still writes `\mathbb{P}(f|X) = N(f|μ,k)` while Chapter 2 now uses lowercase
`p(·)` densities; (b) Martin suggested conditioning on `D` rather than `X`;
(c) *"later becomes X"* — `D_train` is defined then immediately called `X`.
My default proposal: switch the block to `p(\boldsymbol{f} \mid \boldsymbol{X})`
density notation (matching Ch. 2), keep conditioning on `X`, and add one
sentence noting `X` collects the inputs of `D_train`.
> DIRECTION: accept default / other:

**D4 (C048, C056, C057)** `GP.tex` notation audit — Martin: *"Bold always or
never"*, *"Put Bolds Correctly"*, plus `X_*` used two lines before it is
defined. Needs one convention (e.g. bold for vectors/matrices, roman for
scalars) applied through §1.4.2. I can execute the audit mechanically once
you confirm the convention.
> DIRECTION: convention = bold vectors+matrices? yes / other:

**D5 (C124, C125, C126, C127)** `Surrogate_Models.tex:14-18` — Martin marked
the interpretability, interpolation, and ecosystem paragraphs with the same
asterisk: *"Here or in the above GP Section in Ch 2?"* and on the section:
*"this is very much about GP in general... Here should go a focused
discussion as Surrogate models."* Options: (i) move the three paragraphs to
Chapter 1 and leave a summary sentence; (ii) keep them here but rewrite each
opening to be surrogate-specific; (iii) keep as is.
> DIRECTION:

**D6 (C220)** `SMBO.tex:3` — the SMBO pseudo-algorithm's load-bearing citation
is still the Koehrsen blog post (`conceptualBO`). Martin: *"unclear if a
paper or what"*. `hutter2011sequential` (SMAC — the canonical SMBO paper) is
already in the bibliography. Options: swap; cite both (SMAC for the
framework, blog as accessible exposition); keep blog only.
> DIRECTION:

**D7 (C169, C170)** Figures 4.3/4.4 (`3ref_pair.png`, `3hinge.png`) — Martin
marked the x-axis knot label on 4.3 and the `x_{N,j}` label placement on 4.4.
I cannot verify inside the images. If you have the generating script, tell me
where it lives and what to change; otherwise mark "figures are fine".
> DIRECTION:

**D8 (N13)** `GP.tex:291` — the RBF kernel is written
`σ_f² exp(−ℓ(x−x')²/2)`: `ℓ` multiplies in the numerator, where the standard
form divides by `2ℓ²`. Consistent only if `ℓ` is meant as an inverse length
scale. The `gp_regression_ell.pdf` figure's interpretation depends on which
convention the generating code used.
> DIRECTION: keep as inverse length scale (add a clarifying phrase) / switch
> to standard form:

**D9 (C205)** Chapter 5 — Martin: *"Will read when Completed. But Better to
talk first to make sure."* The chapter is now complete with final results he
has not seen (including the negative mixed-space result). This is a
conversation, not an edit; noting it so it isn't lost.
> STATUS: talked to Martin? when:

**D10** Five uncited bibliography entries remain (harmless — they do not
render): `Frazier_2018`, `Klein_Falkner_Bartels_Hennig_Hutter_2017`,
`Daxberger_Makarova_Turchetta_Krause_2020`,
`Luong_Gupta_Nguyen_Rana_Venkatesh_2019`, `Ross_1996`, `ghahramani2011tutorial`.
Keep (may cite later) or prune for a lean final bib?
> DIRECTION: keep all / prune all / keep these:

**D11 (C037)** `GP.tex:216-228` — Gaussian processes are still discussed
around the Kolmogorov theorem before the formal definition arrives. Fixing
this means either a forward reference ("defined formally below") or
reordering. Small, but it changes text you've polished.
> DIRECTION: forward reference / reorder / leave:

**D12 (C180)** `BASS.tex:111` — Martin underlined *"but this is by design
during the training phase. Subsequently, a pruning step is employed"* in the
MARS conclusion (squiggle, no words). Likely redundancy with the identical
statement earlier in the section. Cut the sentence from the conclusion, or
leave both?
> DIRECTION:

### A2. Writing — Chapter 1 (GP.tex): 2 items

**W1 (C050)** `GP.tex:351` — Martin's strongest mark in the chapter (*"!??"*)
on: *"The error variance σ² and the coefficients β are generated by a process
of minimizing the error of the outputs generated."* Wrong as stated (β̂ comes
from least squares / maximum likelihood; σ̂² from residuals — and "generated
... generated" repeats). Two sentences would fix it.
> DIRECTION (or "draft it and I'll review"):

**W2 (C165 residue)** `BASS.tex:53` display — Martin's arrow points at the
`p` in `j = 1,2,…,p`: `p` (number of predictors) is never introduced. One
clause: "where p is the number of predictor variables". Say the word and I'll
place it.
> DIRECTION:

### A3. Writing — Chapter 3 (SMBO + Surrogates): 8 packages

**W3 (C118)** `SMBO.tex` — Martin: *"example, a clear problem where you
explain the steps one by one."* A worked instance of the 5-step loop (e.g.
tuning ridge λ on a small dataset: surrogate fit → acquisition pick →
evaluate → update, two iterations). ~2 paragraphs. This is the single most
requested didactic addition.
> DIRECTION (pick the example problem, or delegate):

**W4 (C115)** `SMBO.tex:3` — Martin: *"Unclear how hyperpar. relate to the
Above and how the Above is usable Here. Much to expand."* Bridge sentence(s):
hyperparameter tuning IS a black-box optimization instance, so the Ch.2
machinery applies with X = hyperparameter space.
> DIRECTION:

**W5 (N06, C149)** `Surrogate_Models.tex:3` — `\section{Introduction}` is
**empty**. Needs 1-2 paragraphs: what this chapter surveys, on what criteria
(uncertainty quality, categorical capability, data hunger, ecosystem), and
where it lands (BASS motivation). Also resolves Martin's *"the titles say
something, the text is a general review"*.
> DIRECTION (or "draft from the criteria above"):

**W6 (C133, C135, C136, C132→B3)** BNN subsection — three of Martin's notes:
vague phrase *"model the objective function"*; *"So no disadvantage
Apparently?"*; *"How about their use as Surrogate Models? The Section is
about this..."*. Needs: disadvantages (compute cost, approximate inference,
data hunger) + one paragraph on BNNs as BO surrogates. Citation for the
framework claim is B3 below.
> DIRECTION:

**W7 (C138, C139)** RF/GBM subsection — Martin: *"also BNN, TPE..."* (the
black-box criticism applies to them too) and *"And their use as Surrogate
Models?"* (SMAC is now cited, which helps). One honest sentence each.
> DIRECTION:

**W8 (C140, C141)** `Surrogate_Models.tex:44` — §"Other Techniques for
Optimizing Hyperparameters": Martin circled *"Other"* (*"the Above 3.1.2/3/4
Never mention Hyperparam..."*) and asked why the scope shifts from surrogates
(step 1) to search strategies (step 2). Options: retitle ("Non-model-based
baselines") + one framing sentence; or fold into the experimental-baselines
story.
> DIRECTION:

**W9 (C148)** GA subsection close — Martin underlined *"these algorithms used
to tune hyperparameters"*. Minor rephrase; or fold into whatever W8 decides.
> DIRECTION (or skip):

**W10 (C123, C129, C131, C150)** Residual chapter-level polish: the *"So
what?"* sentence at the GP section top; TPE section pitched between
quick-review and rigorous (*"if you want to go descriptive keep it clear...
If not expand rigorously"*); the data-hunger point Martin called *"what
matters to you in the end"* deserving prominence; and an explicit sentence on
how hyperparameter search and surrogate modelling overlap (partially covered
by the new SMBO ¶). Each is a sentence or two once you pick a stance on the
TPE depth question.
> DIRECTION (TPE: descriptive or rigorous? rest:):

### A4. Writing — Chapter 4 (BASS): the campaign, 12 packages

This chapter holds nearly all remaining substance. Ordered as the text flows.

**W11 (C153, C155)** Decision trees: reference for the two-flavours claim
(B7 below) and Martin's *"Put two examples"* — one classification, one
regression example (a sentence each is enough).
> DIRECTION:

**W12 (C156, C157, C158)** Ensembles: citations are B4-B6 (validate below);
plus Martin's *"where are there in Reg. Trees? Unclear"* on "piecewise
constant approximations" — one sentence: a regression tree's prediction is
constant within each leaf, so the fitted surface is a step function; MARS
replaces the steps with hinges.
> DIRECTION (approve my sentence / write your own):

**W13 (C160, C161, C162, C163)** MARS construction prose — four flagged
spots: "piecewise linear" query; "Each pair divided at the value t" (unclear);
the ungrammatical *"with slope changes a selection of observed values"*; and
*"Not too clear what is X_j opposed to x_ij"*. One rewritten paragraph
defining X_j (the j-th predictor variable) vs x_ij (its i-th observed value)
fixes all four.
> DIRECTION (or "draft it"):

**W14 (C164, C166, C168)** MARS notation — Martin: *"Big are R.V.? big are
Matrix? clarify what this is"*; `h` used in the reflected-pair displays
without definition; `M` undefined in the additive-model sum. Tie to D4's
convention; add "where h(·) = (·)₊" and "M is the number of basis functions
in the model".
> DIRECTION (mostly mechanical once D4 is set — approve?):

**W15 (C171, C173)** — *"What, model or process?"* on 𝓜 (used as both), and
*"unclear of what?"* on "a better model for each λ size". Fix: 𝓜 is the model
(set of selected terms), construction is the process; λ indexes model size
after pruning.
> DIRECTION (approve / rephrase):

**W16 (C175, C176)** The interaction update equation — Martin: *"Review /
not clear"* and *"Sure?"*, plus *"or ℓ ∈ M?"* on `h_ℓ ∈ M` and the flagged
*"for any case"*. Needs verification against Friedman §3 and a wording fix
(likely: for each h_ℓ already in 𝓜 and each knot t = x_ij).
> DIRECTION (verify-and-fix / you'll check Friedman yourself):

**W17 (C177 = T005)** The missing transition into interactions — Martin
answered your own margin question: *"explicando bien que son las
interacciones, porque se necesitan y como se extiende A"* — explain what
interaction terms are, why they're needed (additive models can't capture
joint effects), and how the base equation extends. ~1 paragraph where the
`% AUTHOR NOTE` comment sits (BASS.tex:105).
> DIRECTION:

**W18 (C179)** — *"Maybe examples?"* for MARS. Could reuse the two-predictor
setting from the BMARS example for continuity.
> DIRECTION (add / skip):

**W19 (C181, C182, C183, C184, C185)** BMARS block — *"explain better"* on
the random-framing paragraph; Definition 10's `J_i` (circled — never
introduced: it's the interaction degree of basis i); *"Unclear what 'type'
really means in interpretation"*; the m=2 example *"not helping much"*; the
`*` placeholder notation queried. One careful rewrite of definition +
example, introducing J_i and reading "type = which predictor subset the
basis splits on".
> DIRECTION (or "draft it"):

**W20 (T006, C195, C196)** The model of the thesis — Martin: *"This Section
should Be detailed. it is The model of the thesis"* and *"How about the 'B'
of BASS? Unclear where Bayes is in B"*; your own note: *"Cual es el modelo?
Tengo el paper pero no lo veo claro."* What's missing: the priors (on number
of basis functions, knots, signs, coefficients, σ²) and the resulting
posterior that RJMCMC targets — i.e., the explicit Bayesian model statement
from Francom & Sansó §2 / Denison et al. This is the highest-value writing
item in the thesis.
> DIRECTION (source section to follow, level of detail):

**W21 (C189, C192)** — *"some efficiency improvements"* needs one concrete
clause (Francom & Sansó: efficient proposals + parallel tempering); and a
one-sentence signpost "Equations (…) constitute the BASS model" (Martin: *"So
this is BASS"*).
> DIRECTION (approve / adjust):

**W22 (C197, C198, C199)** Cold-start subsection — Martin: *"if the Domain is
unbounded?"* (uniform prior claim), *"?"* on "random search with the same
objective", and *"relevant here?"* on the experiment detail. Fixes: qualify
to bounded/compact domains (true for all benchmarks here); rephrase the
random-search clause; move the experiment sentence to Ch. 5 or cut.
> DIRECTION:

**W23 (C200, C201)** LHS paragraph — Martin: *"this requires knowing the
joint of the Parameters? clarify"* and *"In your case Dist. on what?"*.
Fix: LHS here samples uniformly over the (bounded) search box — no joint
needs to be known; say so explicitly.
> DIRECTION (approve / adjust):

---

## Part B — Proposed new citations (validate before I add them)

Each entry: where it goes, why this source. Tick to approve; strike or
substitute freely. None are in the .bib yet.

**B1 — Breiman 2001, "Random Forests"** → C157, RF mention in ensembles list
(`BASS.tex:33`) and RF/GBM subsection.
```bibtex
@article{breiman2001random,
  title={Random Forests},
  author={Breiman, Leo},
  journal={Machine Learning},
  volume={45},
  number={1},
  pages={5--32},
  year={2001},
  publisher={Springer}
}
```
> APPROVE? 

**B2 — Friedman 2001, "Greedy Function Approximation: A Gradient Boosting
Machine"** → C157, Gradient Boosting mention.
```bibtex
@article{friedman2001greedy,
  title={Greedy function approximation: A gradient boosting machine},
  author={Friedman, Jerome H},
  journal={The Annals of Statistics},
  volume={29},
  number={5},
  pages={1189--1232},
  year={2001}
}
```
> APPROVE?

**B3 — Springenberg et al. 2016, "Bayesian Optimization with Robust Bayesian
Neural Networks" (BOHAMIANN)** → C132/W6, the BNN-as-BO-surrogate claim.
Chosen over Neal 1996 because it is specifically BNNs *as surrogates*, which
is Martin's actual complaint. Add Neal too if you want the foundational cite.
```bibtex
@inproceedings{springenberg2016bayesian,
  title={Bayesian optimization with robust Bayesian neural networks},
  author={Springenberg, Jost Tobias and Klein, Aaron and Falkner, Stefan and Hutter, Frank},
  booktitle={Advances in Neural Information Processing Systems},
  volume={29},
  year={2016}
}
```
> APPROVE? (also add Neal 1996? yes/no)

**B4 — Freund & Schapire 1997** → C157, AdaBoost mention.
```bibtex
@article{freund1997decision,
  title={A decision-theoretic generalization of on-line learning and an application to boosting},
  author={Freund, Yoav and Schapire, Robert E},
  journal={Journal of Computer and System Sciences},
  volume={55},
  number={1},
  pages={119--139},
  year={1997}
}
```
> APPROVE?

**B5 — Breiman 1996, "Bagging Predictors"** → C157, Bagging mention; also
supports the OOB/bootstrap uncertainty sentence alongside the SMAC cite.
```bibtex
@article{breiman1996bagging,
  title={Bagging predictors},
  author={Breiman, Leo},
  journal={Machine Learning},
  volume={24},
  number={2},
  pages={123--140},
  year={1996}
}
```
> APPROVE?

**B6 — Dietterich 2000, "Ensemble Methods in Machine Learning"** → C156, the
"sometimes called ensemble methods" sentence (`BASS.tex:31`).
```bibtex
@inproceedings{dietterich2000ensemble,
  title={Ensemble methods in machine learning},
  author={Dietterich, Thomas G},
  booktitle={International Workshop on Multiple Classifier Systems},
  pages={1--15},
  year={2000},
  publisher={Springer}
}
```
> APPROVE?

**B7 — Hastie, Tibshirani & Friedman, *The Elements of Statistical Learning*
(2nd ed.)** → C153, the classification/regression-trees flavours claim
(`BASS.tex:15`), instead of double-citing Breiman two sentences apart. Also
usable for W12-W14 (MARS is ESL §9.4).
```bibtex
@book{hastie2009elements,
  title={The Elements of Statistical Learning: Data Mining, Inference, and Prediction},
  author={Hastie, Trevor and Tibshirani, Robert and Friedman, Jerome},
  edition={2nd},
  year={2009},
  publisher={Springer},
  address={New York}
}
```
> APPROVE?

Already inserted with existing keys (no action needed): Snoek 2012 for the
"killer app" claim — **flag if you want a different attribution**; Rasmussen
& Williams for GP uncertainty/interpolation; SMAC for tree-surrogate
uncertainty; Bergstra & Bengio for grid+random search.

---

## Part C — Production tasks (no prose from you needed beyond the briefs)

- [ ] **Figure: "theoretical basis for BASS"** (`BASS.tex:8`, currently
      `missing.png`; C152). Brief: CART → MARS → BMARS → BASS lineage
      diagram. I can generate a TikZ version on your go-ahead.
      > GO/brief:
- [ ] **Figure: "A decision tree"** (`BASS.tex:18`, currently `missing.png`;
      N12). Brief: small binary tree, 2 splits, leaf predictions — ties into
      W11's examples. TikZ on go-ahead.
      > GO/brief:
- [ ] Figures 4.3/4.4 label fixes — pending D7.
- [ ] `\date{...}` in `main.tex` (N09) — needs the defense/submission date.
      > DATE:
- [ ] Add the AI-disclosure section (source: `docs/AI_DISCLOSURE.md`) —
      where: after acknowledgements / where the program requires.
      > PLACEMENT:
- [ ] Turn traceability green black for the final PDF: redefine
      `\green`/`\blue` to `#1` in `main.tex` (keep the wrappers in source for
      the advisor-facing diff; flip only for the submitted build).
- [ ] Rebuild + commit `main.pdf` after every batch (convention: PDF travels
      with sources).

## Part D — Repository hygiene

- [x] `run_on_ec2.sh` deleted; `final_results/README.md` reference cleaned (d790f12).
- [x] `written_files/tesis_escrito/PASS2_INSTRUCTIONS.md` deleted (d790f12).
- [x] Bibliography duplicate keys pruned — 8 entries (a9d7932).
- [ ] Remote branch prune (PR #12 merged, #13 closed; run when ready):
      ```bash
      git push origin --delete claude/bass-bo-advantage-wk87mx \
        claude/categorical-candidates claude/machinery-confound-article \
        claude/code-review-restructure-plan-yzjvhi
      git branch -d claude/bass-bo-advantage-wk87mx claude/categorical-candidates \
        feature/tpe-baseline thesis/categorical-update docs/ai-disclosure
      ```
- [ ] `class_presentation/`: include in final public repo? > DECISION:
- [ ] D10's uncited bib entries — per your call above.
- `final_results/elastic_net/` stays (provenance; recorded author decision).
- `written_files/corrections.pdf` + `corrections.json` stay (review
  provenance & ledger). `.claude/skills/` stays (tooling, reproducible).
- [ ] This file deleted last, when everything above is `[x]`.

## Part E — Final verification (after all of the above)

```bash
cd written_files/tesis_escrito
pdflatex -interaction=nonstopmode main.tex && bibtex main && \
  pdflatex -interaction=nonstopmode main.tex && pdflatex -interaction=nonstopmode main.tex
grep -E "^!|Citation.*undefined|Reference.*undefined" main.log      # must be empty
grep -rn "AUTHOR NOTE\|missing.png\|INSERT DATE" TeX_files/ main.tex # must be empty
grep -rn "DIRECTION:$\|APPROVE?$" ../../docs/FINAL_CLEANUP_PLAN.md   # every blank answered
python3 -c "import json;d=json.load(open('../corrections.json'));print(sum(1 for c in d['corrections'] if c['status'] in('open','partial','check')),'items still open')"  # target: 0
```
