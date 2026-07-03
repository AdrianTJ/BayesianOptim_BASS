# Pass 2: finishing the thesis after the final benchmark run

Pass 1 (already done) made the thesis consistent with the implemented pipeline
and removed every claim that ran ahead of the data. Pass 2 inserts the real
numbers. This file is the complete worklist; hand it (plus the committed
`final_results/`) to whoever does the editing — human or Claude session.

Every text edit made during these passes is wrapped in `\green{}` per the
advisor-traceability convention; keep doing that in Pass 2.

## Step 0 — produce and publish the final results

```bash
# Optional 10-minute end-to-end validation first:
SMOKE=1 bash run_on_ec2.sh          # or locally: BUDGET=8 REPS=2 bash code_files/run_all_final.sh

# The real run (budget 80, 25 seeds, TPE on; ~1-2 h on 32 vCPU):
bash run_on_ec2.sh                  # or: bash code_files/run_all_final.sh

# Publish into the tracked homes (from the repo root, results in
# code_files/results/ or RESULTS_ROOT=<untarred EC2 results>):
bash code_files/collect_thesis_artifacts.sh

git add final_results written_files/tesis_escrito/Figures
git commit   # "Final benchmark results (budget 80, seeds 1001-1025)"
```

The collector must exit 0 with 11 copied plots and no `MISSING:` lines.

## Step 1 — the marker worklist

`grep -rn "PASS2" written_files/tesis_escrito/TeX_files/` lists every site.
Current inventory (11 markers):

**Experiment.tex**
1. §Branin: add `\includegraphics{Figures/conv_branin.png}` as the results
   figure (keep the surface illustration as context).
2. §Rastrigin: swap the `\fbox` for `Figures/conv_rastrigin.png`.
3. §Func-2C/3C: swap the `\fbox` for `conv_func2C.png` + `conv_func3C.png`,
   and add a paired-stats table per benchmark (see Step 2).
4. §Cat-Ackley: swap the `\fbox` for the three instance plots
   (`conv_cat_ackley_easy/medium/hard.png`) + paired-stats tables.
5. §TPE sensitivity: swap the `\fbox` for `conv_tpe_gamma_branin.png` +
   `conv_tpe_gamma_cat_ackley.png`, and quote the TPE final-best range across
   gamma for each objective (from
   `final_results/tpe_sensitivity/*/final_summary.csv`: max minus min of
   `mean_final` over the four TPE rows), contrasted with BASS-BO/GP-BO having
   no such spread.
6. §Result Interpretation and Limitations (empty): write it. Agreed structure
   is in the comment scaffold in the section itself: (a) continuous outcome
   as measured; (b) the three Cat-Ackley sizes read together — capability on
   the easy instance vs relative progress on the hard one; (c) the Func-2C/3C
   paired outcome reported honestly, including parity with Random if that is
   what held; (d) limitations (2–6d, synthetic, single budget, deterministic
   objectives, one real tuning task).

**Introduction.tex**
7. Final paragraph of the pipeline description: may now state actual outcomes
   instead of "the experiments test whether...".

**Conclusions.tex**
8. Results paragraph: replace the conditional phrasing with measured outcomes.
9. Scope-limits paragraph: state the categorical outcomes (easy-instance solve
   rates, hard-instance paired wins, Func-2C/3C result).
10. Practical-takeaway sentence: state the measured continuous outcome.
11. Final summary paragraph: finalize with the measured outcomes.

## Step 2 — tables from the CSVs

For each benchmark, `final_results/<label>/` contains:

- `final_summary.csv` — mean/sd of each method's final best, ranked. One
  compact `tabular` for the continuous benchmarks and one for the
  categorical/mixed ones works well.
- `paired_vs_random.csv` — per method: `wins/ties/losses` vs Random across the
  25 seeds, `median_final`, `median_baseline`, `p_wilcoxon` (paired Wilcoxon
  signed-rank). These are the headline numbers for the categorical story; the
  Statistical Analysis subsection of Experiment.tex already explains the
  methodology, so the tables only need the numbers.

Transcribe (don't screenshot) into LaTeX tables; cite the exact seed range and
budget in each caption.

## Step 3 — decisions that need the author

- **Elastic Net**: the case study runs in the protocol
  (`final_results/elastic_net/`, `conv_elastic_net.png`,
  `test_rmse_summary.csv`) but the thesis never mentions it. Either add a
  short "real tuning task" subsection to Experiment.tex or consciously leave
  it out — currently it is unused evidence.
- **Interpretation honesty check**: whatever Func-2C/3C shows, report it. In
  the pre-final diagnostic (10 seeds, budget 60) BASS-BO was at parity with
  Random there (5W/5L) while clearly winning on Cat-Ackley (10/10 on easy and
  hard); if the full 25-seed run reproduces that, the parity result is a
  finding, not a failure — the MC-EI resolution hypothesis is already noted in
  `code_files/3_categorical_diagnostics/README.md`.

## Step 4 — remaining author-only debt (not Pass 2 blockers)

- Two figures in BASS.tex still use the `missing.png` stand-in (theoretical
  basis diagram; decision-tree diagram) — need real content.
- Two `% AUTHOR NOTE` comments in BASS.tex (a transition sentence; the
  explicit BMARS model statement) — need author text.
- `\date{INSERT DATE HERE WHEN DONE}` in main.tex.
- At the very final version: turn the `\green`/`\blue` traceability markup
  black (redefine both macros to `#1`, or strip them).

## Step 5 — verify

```bash
cd written_files/tesis_escrito
pdflatex -interaction=nonstopmode main.tex && bibtex main && \
  pdflatex -interaction=nonstopmode main.tex && pdflatex -interaction=nonstopmode main.tex
grep -E "^!|Citation.*undefined|File .* not found" main.log   # must be empty
grep -rn "PASS2" TeX_files/                                    # must be empty when Pass 2 is done
```

Commit `main.pdf` together with the source edits so the tracked PDF always
matches the sources.
