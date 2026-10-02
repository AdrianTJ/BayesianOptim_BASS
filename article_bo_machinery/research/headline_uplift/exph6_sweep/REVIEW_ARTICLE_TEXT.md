# Adversarial review: Sec. "How Far Does the Waste Extend?" (commit d2aca67)

Reviewer: independent of the producer. Text: `/home/claude/article-rev`, branch
`docs/article-claims-revision`, commit `d2aca67`. Data: `exph6_sweep/results.jsonl`
on `research/g-sweep-gp-wave` (16,275 rows, no duplicate keys). Scripts written for this
review: `sweep_text_check.py` (all table cells and quoted numbers, own code, does not import
`analyze_g.py`) and `sweep_text_check2.py` (budget monotonicity and the 60-of-61 denominator),
both in this scratchpad folder.

**Verdict: accept with corrections.** Every table cell and nearly every quoted number
reproduces, and the hypothesis verdicts are reported honestly. There are three blocking
problems. The contribution sentence (and the section) miscounts the fully discrete benchmarks.
The unchanged Limitations paragraph now contradicts the new section. The article branch does
not contain the data and records the section cites. There are also eight should-fix wording
problems: the 60-of-61 denominator, the second-machine sentence, the box-corner trace, and
others.

---

## Findings

### 1. BLOCKING: "14 discrete benchmarks" miscounts. The sweep has 15 fully discrete benchmarks

**Sentences:**
- Contribution 1: "extends the pattern to all 14 discrete benchmarks tested"
- Sec. sweep: "Optuna's TPE re-spends between 20\% and 48\% of its budget beyond chance on each of the 14 discrete benchmarks"

**Found:** `ml_rf_digits` (class E) is fully discrete: cat × cat × int × int, K = 4·2·19·28 in
`analyze_g.py`. The section itself later calls it "the one fully discrete space". So 15
discrete benchmarks were tested, not 14. The 20–48% range is correct only for the 14 in
families A–D at B = 80 (optuna-tpe 4.9: min 0.200 pest_control, max 0.475 maxcut_n20 and
contam_2p25). On ml_rf_digits optuna-tpe has e(80) = 0.153 and hyperopt-tpe 0.041. Every TPE
variant is still above chance on all 15, so the abstract's unnumbered "every discrete
benchmark tested" survives. The number "14" does not.

**Proposed wording:**
- Contribution: "A pre-registered sweep over 23 benchmarks finds TPE waste beyond chance on all 15 fully discrete benchmarks tested and marks where the measure stops: on mixed machine-learning spaces it registers no TPE waste."
- Section: "At budget 80, Optuna's TPE re-spends between 20\% and 48\% of its budget beyond chance on each of the 14 benchmarks in families A--D, Hyperopt's TPE 12--24\% per family, and ..."

### 2. BLOCKING: the Limitations paragraph (unchanged, around lines 1219–1226) now contradicts the new section

**Sentences (Limitations):** "Where a search space carries continuous coordinates, exact combination revisits are near-impossible by construction, and the audit reports accordingly ... A practitioner tuning a continuous-heavy space should therefore not expect this failure mode, and the ecosystem finding should not be read as extending there."

**Found:** The new section says two things this paragraph contradicts. (a) "the measure cannot see a sampler re-spending budget on the same discrete sub-configuration of a mixed space". If the measure cannot see it, the paper cannot tell practitioners not to expect it. (b) Optuna's GP sampler repeats exact configurations on mixed spaces. Its median is 36 of 80 on yahpo_rpart_40981 (25/25 runs). It also repeats in individual runs on yahpo_rpart_41138 (max 119 at B160), yahpo_ranger_1489, ml_gb_bc, ml_svm_digits (B160) and func2C (B160). So exact revisits are not "near-impossible by construction" for every optimizer. The 349-of-350 figure is correct for the ecosystem audit's two CoCaBO spaces only.

**Proposed wording:** Replace the last two sentences of that paragraph with: "Where a search space carries continuous coordinates, a sampler that draws those coordinates continuously almost never repeats an exact configuration, so the measure registers little there (349 of 350 mixed-space runs in the ecosystem audit, and no TPE revisit on any mixed space in the sweep, Sec.~\ref{sec:sweep}). This is a limit of the measure, not evidence of absence: it cannot see re-spending on a discrete sub-configuration, and one sampler (Optuna's GP) does repeat exact mixed configurations. The waste findings should not be read as extending to continuous-heavy spaces, in either direction."

### 3. BLOCKING (merge precondition, not wording): the article branch does not contain what the section cites

**Sentences:** "For the ecosystem audit, its equal-budget control, the generalization sweep, ... this ordering is verifiable from the repository history" and "Every number below was checked against the raw results by an independent reviewer."

**Found:** On `docs/article-claims-revision`, `exph6_sweep/results.jsonl` has 10,203 rows (the `849bf1d` checkpoint), not 16,275. ANALYSIS.md, REVIEW.md, AMENDMENT_2.md, env_sandcastle.txt, POSTHOC_GP_REPEATS.md and posthoc_gp_repeats.py are absent there. They exist only on `research/g-sweep-gp-wave` (up to `4c91309`). From the article's own branch, a reader can reproduce none of the GP-arm numbers (Optuna GP column, the 36 of 80, GH2's skopt cell, the GP160 subset). The pre-registration commits (`4278e4a`, `f93b2bd`) are ancestors of the article branch, so that part is fine.

**Fix:** merge (or rebase onto) `research/g-sweep-gp-wave` before this text lands. The sentence "Every number below was checked ..." becomes true only once this review's corrections are applied.

### 4. SHOULD-FIX: "60 of 61" uses a different cell set from the registered one it is set against

**Sentence:** "Among the cells where $e(B)$ varies across budgets, the rank correlation between budget and waste is non-negative in 60 of 61, ..."

**Found:** 61 is the script's 89-cell set (including 15 optuna-gp cells with only three budgets), minus 26 constant cells, minus 2 cells whose variation is floating-point residue (optuna-gp labs_n25 at about 1e-7, pest_control at about 1e-16). Without that exclusion the count is 60 of 63. The next sentences frame GH3 on the 74 four-budget cells, so a reader takes 61 to be a subset of 74. It is not: 11 of the 61 are three-budget cells. On the registered four-budget set, the count is 49 of 50 non-artefact varying cells (49 of 51 including the pest_control residue). The one real exception is optuna-gp on catf_rosen_d4L7, where e stays below chance (−0.004 → −0.026). "Wherever it exists" holds: all 60 cells with any positive median e have ρ ≥ 0, 48 of them strictly increasing.

**Proposed wording:** "Waste grows with budget wherever it exists. Of the four-budget cells where $e(B)$ varies (setting aside one whose variation is a $10^{-16}$ rounding residue), the rank correlation between budget and waste is non-negative in 49 of 50; the exception is Optuna's GP sampler on categorical Rosenbrock, which stays below chance throughout."

### 5. SHOULD-FIX: "42–61% at budget 160" is a range of family medians, not of benchmarks

**Sentence:** "and Optuna's TPE reaches 42--61\% at budget 160."

**Found:** optuna-tpe e(160) family medians: A 0.458, B 0.419, C 0.606, D 0.444, giving 42–61%. Per benchmark the range is 0.288–0.606. The preceding sentence gives a per-benchmark range (20–48%), so a reader will take this one as per-benchmark too.

**Proposed wording:** "and Optuna's TPE reaches family medians of 42--61\% at budget 160."

### 6. SHOULD-FIX: the second-machine sentence claims path differences do not affect the waste measure. They do

**Sentence:** "for some arms the same seed follows a different optimizer path on different hardware, which affects solve counts but not the waste measure, which is a property of each run."

**Found:** If a seed takes a different optimizer path, its revisit count can change as well as its best value. `env_sandcastle.txt` lists optuna-tpe contam_2p25/B80/4006 as not reproducing across machines, and optuna-tpe is the arm whose waste the paper measures. What is true is narrower. Each run's waste is measured on that run's own trajectory, and the verdicts are cell medians over 25 seeds, not per-seed replications. (Minor: three GP-arm timing-smoke rows ran on the original machine, so "the GP-family arms" ran there in all but three rows.)

**Proposed wording:** "Part of the sweep (nearly all GP-family runs and part of SMAC's) ran on a second machine, with objective values checked identical across machines. For some arms the same seed follows a different optimizer path on different hardware, so individual runs (their solve and revisit counts alike) need not reproduce elsewhere; the results above are medians over 25 seeds and do not depend on per-seed replication."

### 7. SHOULD-FIX: the box-corner sentence does not say the traced runs are post-hoc re-runs that do not reproduce the sweep's own runs

**Sentence:** "On a scikit-learn task where it does the same, every repeat we traced sits at a corner of the box (learning rate and subsample both at their upper bounds), consistent with its acquisition optimizer clipping to the bounds; we have not traced the YAHPO cases."

**Found:** The bounds are correct: logLR ∈ [−3, 0] and subsample ∈ [0.5, 1] in `benchmarks_g.ml_gb_bc`. The traced repeats have logLR = 0.0, subsample = 1.0 and max_depth = 1, its lower bound. But the trace (POSTHOC_GP_REPEATS.md) is not pre-registered. It re-ran 4 seeds under scikit-learn 1.9.1 (the sweep pinned 1.9.0), and its revisit counts do not match the sweep's rows for the same seeds:

| seed | traced revisits | sweep revisits |
|---|---|---|
| 4001 | 0 | 2 |
| 4002 | 2 | 14 |
| 4003 | 3 | 1 |
| 4004 | 1 | 6 |

So it describes repeats in re-runs, not the repeats the sweep counted. The hedge "consistent with" is appropriate. "Every repeat we traced" is accurate but hides the scope.

**Proposed wording:** "On a scikit-learn task where it also repeats (gradient boosting on breast-cancer), a post-hoc re-run of four seeds, in a slightly different scikit-learn version and not reproducing the sweep's own revisit counts, found every repeat at a corner of the box (learning rate and subsample both at their upper bounds), consistent with its acquisition optimizer clipping to the bounds; we have not traced the sweep's own runs or the YAHPO cases."

### 8. SHOULD-FIX: "$e(B) = \dots$, zero for spaces with a continuous dimension" says the metric is zero there; it is the baseline that is zero

**Sentence:** "The metric is the excess-waste fraction $e(B) = (\text{revisits} - \text{pigeonhole}(K,B))/B$, zero for spaces with a continuous dimension."

**Found:** As written, "zero" attaches to $e(B)$. The section then reports $e(80) = 0.45$ for Optuna's GP on a YAHPO space with continuous dimensions. The DESIGN sets the pigeonhole term to zero there.

**Proposed wording:** "The metric is the excess-waste fraction $e(B) = (\text{revisits} - \text{pigeonhole}(K,B))/B$, where $\text{pigeonhole}(K,B)$ is the expected collision count of uniform sampling defined above, taken as zero for spaces with a continuous dimension."

### 9. SHOULD-FIX: "committed before the first run" is literally false

**Sentence:** "seven hypotheses (GH1--GH7) and the analysis script were committed before the first run."

**Found:** DESIGN.md: "Tool bring-up smokes at tiny budgets preceded this". Its procedure runs the timing smoke (step 2) before committing analyze_g.py (step 3). Commit `f93b2bd` adds analyze_g.py together with the 4 smoke rows. Those rows (smac catf_michal B80; optuna-gp nk_n20k8 B80; ax ml_rf_digits B40; skopt-gp labs_n25 B80, all seed 4001) are retained in the 16,275 analysed rows. DESIGN (`4278e4a`) predates every matrix row.

**Proposed wording:** "seven hypotheses (GH1--GH7) were committed before any protocol-scale run, and the analysis script before the first matrix run."

### 10. SHOULD-FIX: "on a technicality" softens a failed pre-registered clause

**Sentence:** "The pre-registered bound on the deduplicating arms (GH2) nevertheless fails on a technicality: it was two-sided, ..."

**Found:** GH2's literal clause is "skopt-gp: median |e(B)| ≤ 0.07 in every cell". It fails on cat_ackley_d3_L5/B20, e = −0.07247 (pigeonhole 1.449 at K = 125, B = 20; revisits {0: 15, 1: 8, 2: 2}). DESIGN states "Falsification is symmetric". The facts in the sentence are correct and already explain the failure. "On a technicality" is an evaluative gloss that the earlier review asked to keep out.

**Proposed wording:** "The pre-registered bound on the deduplicating arms (GH2) nevertheless fails: it was two-sided, and scikit-optimize misses it on one cell at $e = -0.0725$, that is, by revisiting \emph{less} than chance."

### 11. SHOULD-FIX: "86 of 89" leaves the 89 unexplained

**Sentence:** "The analysis script committed before the runs counted them as passes (86 of 89); we report the hypothesis as failed."

**Found:** 89 = 74 four-budget cells + 15 three-budget optuna-gp cells that the DESIGN wording excludes. As written, a reader cannot reconcile 89 with the 74 cells of the preceding sentence. (Also "before the runs", see finding 9.)

**Proposed wording:** "The analysis script, committed before the matrix ran, counted them as passes and also included 15 Optuna GP cells with only three budgets (86 of 89); we report the hypothesis as failed."

### 12. MINOR: "Optuna 3.6 behaves like 4.9"

GH4 is a threshold test (> 0.05 in ≥ 4/5 classes), not an equivalence test. The data support "similar": family medians A 30.3 vs 32.1, B 29.4 vs 30.7, C and D identical; per benchmark they differ by up to 0.038.
**Proposed:** "and Optuna~3.6 wastes at similar levels (family medians within two points of 4.9's), so ..."

### 13. MINOR: Optuna GP's mixed-space repeats are wider than "two of the YAHPO scenarios"

The median is above zero on two YAHPO scenarios (40981: 36 at B80; 41138: 1 at B80, 9 at B160). Individual runs repeat on all three YAHPO scenarios, plus ml_gb_bc (18/25 runs at B80), ml_svm_digits and func2C at B160.
**Proposed:** "Optuna's GP sampler repeats exact configurations in median on two of the YAHPO scenarios (36 of 80 evaluations on one), and in individual runs on four other mixed spaces."

### 14. MINOR: "seven hypotheses" / "three of the sweep's seven" — GH6 is descriptive with no gate

DESIGN marks GH6 "(descriptive, no gate)". The count of three failures (GH2, GH3, GH5) is correct. GH1, GH4 and GH7 pass, and 3 + 3 = 6 is arithmetically right.
**Proposed (Experiments):** "and three of the sweep's six gated hypotheses (Sec.~\ref{sec:sweep})". Separately, outside this diff: the footnotes elsewhere record a failed gap-decay clause and a failed ceiling-proximity gate, neither counted in "six". The author should confirm that the tally means "hypotheses" in a sense that excludes clauses and gates.

### 15. MINOR: table caption does not explain negative cells

Add: "Negative values mean fewer revisits than uniform sampling would make."

### 16. MINOR: Limitations now stale in two more places

"the pest-control space is 25-dimensional but appears only in the ecosystem audit": the sweep also runs pest_control, contam_2p25 (25 binary) and labs_n25. "a single, version-pinned snapshot": the sweep adds Optuna 3.6.1. Suggested replacements:
- "...appears only in the ecosystem audit and the sweep"
- "...a version-pinned snapshot (with one earlier Optuna release checked in Sec.~\ref{sec:sweep})"

### 17. MINOR: bib entry and benchmark citations

`pfisterer2022yahpo`: authors, title, booktitle ("Proceedings of the First International Conference on Automated Machine Learning"), series PMLR, volume 188 and year 2022 are correct. It lacks pages and a URL. Add `pages = {3/1--39}` and `url = {https://proceedings.mlr.press/v188/pfisterer22a.html}`. The NK, MaxCut, LABS and contamination benchmarks are uncited. Pest control and contamination come from the COMBO/BOCS line, and `oh2019combo` is already in the bib.

---

## Checked and found correct

- **Table tab:sweep, all 47 filled cells** at whole-percent rounding:
  - Random: A −0.00, B −0.38, C −0.00, D −0.00, E 0, F 0 (all round to 0)
  - Optuna TPE 4.9: 30.32 / 29.40 / 46.25 / 33.75 / 0 / 0
  - Optuna TPE 3.6: 32.12 / 30.65 / 46.25 / 33.75 / 0 (over the 4 ml_* only) / 0
  - Hyperopt TPE: 15.87 / 11.90 / 24.37 / 15.62 / 0 / 0
  - Optuna GP: 40.32 / 43.15 / 1.25 / 0.62 / 0.33 / 0
  - skopt GP: −0.93 / −0.60 / 0 / 0 / 0 / 0
  - Ax: −1.25 / −0.60 / 0 / 0 / 0 / 0
  - SMAC: −1.25 / −0.60 / 0 / 0 / — / 0
- **Caption statements:** the 3.6 YAHPO exclusion (Amendment 1); SMAC not covering E.
- **Family membership A–F** matches `run_g.CLASSES`. "Three sizes above" matches Sec. wild's 5^3, 5^5 and 11^6. "Seven arms of Sec. wild plus Optuna 3.6" matches. "Budgets 20, 40, 80, 160; 25 seeds; 16,275 runs" matches the DESIGN's 16,575 minus Amendment 1's 300.
- **GP-at-160 subset:** all three GP arms have B160 on exactly the 8 pre-named benchmarks.
- **"20% and 48%"** for optuna-tpe 4.9 across A–D at B80 (0.200–0.475). Note that 3.6 reaches 0.4875.
- **Hyperopt "12–24% per family"** (11.90–24.37).
- **"Ax and SMAC register a median of zero revisits in every cell at every budget"** (77 ax cells, 64 smac cells, none with median > 0).
- **GH2:** skopt-gp's only out-of-bound cell is d3/B20 at e = −0.07247.
- **GH3:** 74 four-budget cells; 23 constant at exactly 0; 49/74 = 0.66; script 86/89.
- **GH1 and GH4 pass; GH5** best arm 1 of 7; **GH7** pass.
- **"No TPE variant exceeds 5%" on class E except ml_rf_digits** (optuna-tpe 4.9 and 3.6: 0.153). TPE-family arms have zero revisits on every float-bearing space at every budget.
- **"A median of 36 of 80"** on yahpo_rpart_40981 (optuna-gp, B80, 25/25 runs nonzero).
- **Hypothesis wording vs DESIGN:**
  - GH1: "Hyperopt ... per family" and "Optuna ... each benchmark" are both consistent.
  - GH2: two-sided |e| ≤ 0.07, correctly described.
  - GH3: "cells with four budgets", correctly described.
  - GH4: correctly described.
  - GH5: "≥4 of 7" rendered as "most of them", acceptable.
  - Verdicts match ANALYSIS.md and REVIEW.md.
- **Scope language:** "The waste claims in this paper are therefore claims about discrete and categorical spaces" and "the measure cannot see ... discrete sub-configuration" are supported. The abstract's mixed-space clause is an accurate summary.
- **LaTeX:**
  - `tabular{lrrrrrr}` has 7 columns and every row has 7 entries.
  - `ruledtabular` with `\hline` is used the same way as tab:wild.
  - Labels sec:sweep and tab:sweep exist and are referenced. The cite key resolves.
- **`check_article.py`:** `PASS (TODOs: 3, high-water 3; 30 cite keys OK; 0 unused-bib warnings)`, exit 0. `article_state.json` was restored with `git checkout --`, and the working tree is clean afterwards.
