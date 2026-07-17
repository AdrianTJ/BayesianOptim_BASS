# Final cleanup and editing plan — v3 (review queue)

Updated 2026-07-17 on branch `written-corrections`, after executing the fully
answered v2 worksheet: all decisions applied, all approved writing packages
drafted into the LaTeX (green-wrapped), all seven citations added, both
missing figures generated, the defense deck built, and the tracker synced.

What remains is exactly two kinds of work: **your review of the flagged
drafts** (Part R) and **deferred production steps** (Part P). Same contract
as before: write your verdicts into the blocks and hand the file back.

---

## Part R — Review queue (drafts in the tree awaiting your approval)

**R1 = W1 (C050)** `GP.tex`, Preamble subsection, green text after the linear
regression display. Drafted: coefficients by ordinary least squares
(minimizing the sum of squared residuals), error variance estimated from the
residuals afterward. Check that this matches how you want least squares
presented (no maximum-likelihood framing was used).
> VERDICT:

**R2 = W13 (C160-C163)** `BASS.tex`, MARS construction paragraph, green.
Drafted around the convention: capital $X_j$ = the $j$-th predictor variable,
lowercase $x_{ij}$ = its observed value at data point $i$; knots placed at
sample values; reflected pair = the two hinges sharing a knot. Check the
convention reads naturally against the surrounding original text.
> VERDICT:

**R3 = W19 (C181-C185)** `BASS.tex`, BMARS section, green. Two things to
check. (a) I renamed basis functions from $X_i$ to $B_i$ inside Definition
10 and its surroundings, because the original notation collided head-on with
R2's "capitals are predictor variables" convention; Denison et al. write
$B_i$ for bases as well, so this follows the source, but it is a notation
change to a quoted definition and deserves your eyes. (b) $J_i$ is introduced
as the interaction degree of basis $i$, and the types example now reuses the
running two-predictor setting.
> VERDICT (a) rename ok? (b) example ok?:

**R4 = W20 (T006, C195, C196)** `BASS.tex`, end of the BASS Models section:
the full Bayesian model statement (priors, posterior, RJMCMC reading),
equations `eq:bass_prior_M` through `eq:bass_posterior`. **This is the one
draft that must be verified against the papers before Martin sees it.** I
wrote it from Francom & Sansó §2 / Denison et al., but from knowledge, not
with the PDFs open. Specifically confirm against Francom & Sansó §2:
- [ ] $M \sim \text{Poisson}(\lambda)$, $\lambda \sim \text{Gamma}(h_1, h_2)$
- [ ] coefficients: $\boldsymbol{a} \mid \sigma^2, \tau, M \sim
      \mathcal{N}(\boldsymbol{0}, \frac{\sigma^2}{\tau}(\boldsymbol{B}^T\boldsymbol{B})^{-1})$
      (Zellner $g$-prior), $\tau \sim \text{Gamma}(g_1, g_2)$
- [ ] $\sigma^2 \sim \text{InvGamma}(\nu_1, \nu_2)$ (check the paper's exact
      choice; some formulations use the improper $1/\sigma^2$)
- [ ] uniform structural priors (interaction degree, variables, signs, knots,
      categorical level subsets)
- [ ] the hyperparameter symbols ($h_1, h_2, g_1, g_2, \nu_1, \nu_2$) - match
      them to the paper's or rename to yours
> VERDICT / corrections:

**R5 = W16 (C175, C176)** `BASS.tex:~100-104` - NOT edited, per your request
for context. Here is the context and a proposal:

*What Friedman's forward pass actually does (Friedman 1991, §3):* at each
step, the candidate additions are products of (i) a basis function $h_\ell$
already in the model, including the constant $h_0 = 1$, and (ii) a new
reflected pair of some predictor $X_j$ that does **not** already appear in
$h_\ell$, with the knot $t$ ranging over the observed values $x_{ij}$ of that
predictor. Both members of the winning pair enter at once, each with its own
coefficient. So the search ranges over triples $(\ell, j, t)$.

*What the thesis currently says:* "where $h_\ell \in \mathcal{M}$ and
$t = x_{ij}$ for any case." The set membership $h_\ell \in \mathcal{M}$ is
right once $\mathcal{M}$ is read as a set of basis functions (Martin's
"or $\ell \in \mathcal{M}$?" would make $\mathcal{M}$ an index set instead;
either convention works, ours is the former). The garbled part is "for any
case", and the restriction on $X_j$ is missing.

*Proposed replacement (green):* "where the search ranges over every basis
function $h_\ell$ already in $\mathcal{M}$ (including the constant $h_0$),
every predictor $X_j$ not already involved in $h_\ell$, and every candidate
knot $t = x_{ij}$; the display equation is the model after the winning
product pair has been added."
> VERDICT: apply as proposed / edit:

**R6 = D7 (C169, C170)** Figures 5.3 (`3ref_pair.png`) and 5.4
(`3hinge.png`): the regeneration conversation you asked to have after the
edits. Current state: no generating scripts exist in the repo (checked), so
both need to be rebuilt from scratch, naturally as new scripts in
`code_files/figure_generations/` next to `decision_tree.py`. Martin's marks:
on 5.3 the x-axis knot label (and you note the image looks cut off); on 5.4
the `x_{N,j}` label placement. Proposed briefs:
- `reflected_pair.py`: one knot $t$ on a clean axis, both hinges labelled
  $h_1 = (X_j - t)_+$ and $h_2 = (t - X_j)_+$, x-axis label visible and
  uncut, knot marked explicitly at $t = x_{ij}$.
- `hinge_collection.py`: several knots $x_{1,j}, x_{2,j}, \ldots, x_{N,j}$
  with their pairs, labels placed above the axis clear of the curves.
> GO/adjust briefs:

---

## Part P — Deferred production (in order, when the review queue clears)

- [ ] Apply R1-R6 verdicts (me, on your word).
- [ ] `\date{}` in `main.tex` is now empty with a source comment; fill with
      the defense date before submission.
- [ ] Send the rebuilt thesis to Martin (D9) - you are handling this
      directly; the Chapter 6 conversation happens there.
- [ ] Turn traceability green black for the submitted build: redefine
      `\green`/`\blue` to `#1` in `main.tex`; keep wrappers in source.
- [ ] Full PDF rebuild and commit of `main.pdf` (deliberately left out of
      the current PR at your direction; the working tree copy is stale).
- [ ] Delete this file last, when everything above is `[x]`.

## Part E — Final verification (unchanged commands)

```bash
cd written_files/tesis_escrito
pdflatex -interaction=nonstopmode main.tex && bibtex main && \
  pdflatex -interaction=nonstopmode main.tex && pdflatex -interaction=nonstopmode main.tex
grep -E "^!|Citation.*undefined|Reference.*undefined" main.log      # must be empty
grep -rn "AUTHOR NOTE\|missing.png\|INSERT DATE" TeX_files/ main.tex # must be empty (it is)
python3 -c "import json;d=json.load(open('../corrections.json'));print(sum(1 for c in d['corrections'] if c['status'] in('open','partial','check')),'items still open')"
# current expectation: 12 (R1-R6 queue: 4 W1/W13 partials + 5 W19/W20 partials tracked
# per-item, 2 open W16 items, 2 figure checks) - reaches 0 as verdicts land
```
