# Claim ledger

Every claim the article intends to make, with current status and evidence.
Updated in every cycle's re-center step. Statuses: **supported** /
**contradicted** / **untested** / **superseded**. A claim slated for the
article may not stay *untested* without a queued experiment or an explicit
user decision to cut it.

| ID | Claim | Status | Evidence | Moves if |
|---|---|---|---|---|
| K1 | The oracle-ceiling audit (acquisition ← true objective) upper-bounds any surrogate's attainable performance for a given candidate machinery, at negligible cost | supported — Python replication agrees with R (E1: 5/6 checks; 135 runs in 67 s, zero fits) | E1 `experiments/exp01_harness_validation/ANALYSIS.md`; R diagnostics; novelty check (`../novelty_check/REPORT.md`) | — |
| K2a | A restricted local generator (forced categorical flip) places a ceiling on every surrogate on mixed benchmarks: permissive beats restricted on every paired seed | supported, **magnitude revised down at n_cand=1000** (E1: 15/15 both benchmarks; func2C −0.2053 vs −0.206326, func3C −0.7106 vs −0.722140; gap is pool-size dependent) | E1 ANALYSIS; pool-size sweep queued (E2) | E2 n_cand sweep shows no widening at small pools |
| K2a-fig | Scaffold figure claim: restricted arm "plateaued at −0.148 at budget 10" on func2C | **unverifiable — quarantined**: inconsistent with the committed R config by tail probability (P(f<−0.148)≈1.4e-3 × 500 globals/iter) and with the exact historical generator reimplemented (−0.199@10) | E1 ANALYSIS §V2; R re-run queued (E4 / user machine) | R re-run reproduces or corrects it |
| Kopt | Exact benchmark optima: func2C −0.206326 at (2,2); func3C −0.722140 at (2,2,1) | supported (per-combo continuous optimization, this cycle) | E1 ANALYSIS; replaces −0.2062/−0.2063/−0.7216 variants in scaffold/README | — |
| K2b | Encoding-level dedup silently re-spends a majority of budget on known combinations | supported (BASS on Cat-Ackley-125: 25–29/40 revisits pre-fix, 0/40 post) | diagnostics README part 3 | E5 shows it is BASS-specific rather than machinery-general |
| K3 | With machinery repaired and shared, surrogate rankings change *meaning*: the mixed-benchmark failure localizes to BASS+MC-EI (GP-BO/TPE win through the identical loop) | supported (final 25-seed run) | `final_results/`: func2C BASS 5W/2T/18L p=0.043, func3C 8W/3T/14L p=0.048; GP/TPE beat Random | — (this replaces K3-old) |
| K3-old | Repairing machinery moved BASS-BO to parity on Func-2C (5W/5L) | **contradicted** — must be removed from the scaffold | preliminary 10-seed run vs final 25-seed run | (kept only as a record) |
| K4 | On purely categorical benchmarks with solvable instances, BASS-BO matches GP-BO and beats Random/TPE | supported | final_results cat_ackley_d3_L5: BASS 25/25 exact optimum (median 4 iters), GP 25/25 | — |
| K5 | The machinery confound generalizes beyond BASS-vs-GP: generator/dedup choices move rankings for other surrogate families (RF, TPE) | **untested** → queued E3/E5 | article Experiments section promises this matrix | E3 results |
| K6 | The oracle audit A/Bs machinery variants cheaply enough to run as a pre-registration step in any pool-based BO study | supported — E1 measured: 90 oracle runs (2 variants × 3 benchmarks × 15 seeds, budget 80, pool 1000) in ~1 min on one core | E1 ANALYSIS | — |
| K7 | Benchmark instances must span solvable and unsolvable-in-budget sizes or surrogate capability is unmeasurable | supported | Cat-Ackley d6/L11 Spearman ≈0.47 near-intercept fits (uninformative at budget) vs d3/L5 solvable | — |
| K8 | No prior work performs an oracle-style machinery audit or a machinery-controlled cross-surrogate protocol in mixed/categorical BO | supported (39-source sweep, 5 near-misses downgraded) | `../novelty_check/REPORT.md`; caveat: full PDFs of MCBO/Tripp/Daulton unchecked (egress) | full-text check finds otherwise |

## Narrative implication (current)

The article's spine: K1+K2 (the confound is real and cheap to expose) →
K3/K4 (removing it changes what comparisons mean, not who wins — the audit
*exonerates* machinery and localizes failures) → K5/K6 (it generalizes and
should be standard practice) → protocol. The scaffold's Section VI punchline
(K3-old) is replaced by K3's stronger, honest version.
