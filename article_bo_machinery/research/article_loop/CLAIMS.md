# Claim ledger

Every claim the article intends to make, with current status and evidence.
Updated in every cycle's re-center step. Statuses: **supported** /
**contradicted** / **untested** / **superseded**. A claim slated for the
article may not stay *untested* without a queued experiment or an explicit
user decision to cut it.

| ID | Claim | Status | Evidence | Moves if |
|---|---|---|---|---|
| K1 | The oracle-ceiling audit (acquisition ← true objective) upper-bounds any surrogate's attainable performance for a given candidate machinery, at negligible cost | supported (R pipeline); Python replication pending | `code_files/3_categorical_diagnostics/` oracle A/B; novelty check found no prior equivalent (`../novelty_check/REPORT.md`) | E1 fails to reproduce |
| K2a | A restricted local generator (forced categorical flip) caps even a perfect surrogate far from the optimum on mixed benchmarks | supported (pre-fix R: keep-combo beat forced-flip 15/15 paired seeds, Func-2C/3C; −0.148 plateau vs −0.2063) | diagnostics README pre-fix findings | E2 final-protocol re-run diverges |
| K2b | Encoding-level dedup silently re-spends a majority of budget on known combinations | supported (BASS on Cat-Ackley-125: 25–29/40 revisits pre-fix, 0/40 post) | diagnostics README part 3 | E5 shows it is BASS-specific rather than machinery-general |
| K3 | With machinery repaired and shared, surrogate rankings change *meaning*: the mixed-benchmark failure localizes to BASS+MC-EI (GP-BO/TPE win through the identical loop) | supported (final 25-seed run) | `final_results/`: func2C BASS 5W/2T/18L p=0.043, func3C 8W/3T/14L p=0.048; GP/TPE beat Random | — (this replaces K3-old) |
| K3-old | Repairing machinery moved BASS-BO to parity on Func-2C (5W/5L) | **contradicted** — must be removed from the scaffold | preliminary 10-seed run vs final 25-seed run | (kept only as a record) |
| K4 | On purely categorical benchmarks with solvable instances, BASS-BO matches GP-BO and beats Random/TPE | supported | final_results cat_ackley_d3_L5: BASS 25/25 exact optimum (median 4 iters), GP 25/25 | — |
| K5 | The machinery confound generalizes beyond BASS-vs-GP: generator/dedup choices move rankings for other surrogate families (RF, TPE) | **untested** → queued E3/E5 | article Experiments section promises this matrix | E3 results |
| K6 | The oracle audit A/Bs machinery variants cheaply enough to run as a pre-registration step in any pool-based BO study | supported in principle (no fits needed); cost numbers pending | E2 will record wall-clock/eval counts | E2 |
| K7 | Benchmark instances must span solvable and unsolvable-in-budget sizes or surrogate capability is unmeasurable | supported | Cat-Ackley d6/L11 Spearman ≈0.47 near-intercept fits (uninformative at budget) vs d3/L5 solvable | — |
| K8 | No prior work performs an oracle-style machinery audit or a machinery-controlled cross-surrogate protocol in mixed/categorical BO | supported (39-source sweep, 5 near-misses downgraded) | `../novelty_check/REPORT.md`; caveat: full PDFs of MCBO/Tripp/Daulton unchecked (egress) | full-text check finds otherwise |

## Narrative implication (current)

The article's spine: K1+K2 (the confound is real and cheap to expose) →
K3/K4 (removing it changes what comparisons mean, not who wins — the audit
*exonerates* machinery and localizes failures) → K5/K6 (it generalizes and
should be standard practice) → protocol. The scaffold's Section VI punchline
(K3-old) is replaced by K3's stronger, honest version.
