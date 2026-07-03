# final_results/ — the numbers behind the thesis

This directory is the **versioned copy** of one complete run of the final
benchmark protocol (`code_files/run_all_final.sh`: budget 80, 25 seeds
1001–1025, TPE baseline on the categorical/mixed benchmarks; Elastic Net case
study with its own defaults). It is populated by

```bash
bash code_files/collect_thesis_artifacts.sh
```

which copies the (git-ignored) scratch tree `code_files/results/` here
verbatim and refreshes the stable-named convergence figures under
`written_files/tesis_escrito/Figures/conv_*.png` that the thesis includes.

Per objective you should find `all_runs.csv` (every best-so-far curve),
`summary_curve.csv` (mean ± 95% CI per iteration), `final_summary.csv`
(final-best leaderboard), `paired_vs_random.csv` (per-seed wins/ties/losses
vs Random + paired Wilcoxon signed-rank), and `convergence_mean_ci.png`.

To regenerate from scratch: run the benchmarks (locally via
`run_all_final.sh` or remotely via `run_on_ec2.sh`), then re-run the
collector and commit the diff. The commit history of this directory is the
audit trail of every headline-results refresh.
