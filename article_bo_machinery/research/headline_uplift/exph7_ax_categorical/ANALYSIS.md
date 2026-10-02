# H7 analysis — Ax with unordered categoricals

Status: **reviewed** (`REVIEW.md`, independent reviewer, verdict "accept
with corrections"). This version applies the corrections and adds the
same-environment ordinal control the review called for (Amendment 1).
The first version is commit 24b83ba.

Inputs: `results.jsonl` (nominal, 150/150) and `results_ordinal.jsonl`
(ordinal control, 150/150, no failures), both run in this environment
(`env.txt`); `failures.log` holds attempt 1's 146 transient oneMKL/torch
load failures, all re-run in attempt 2. The committed H1 ordinal rows
(`../exph1_matrix/results.jsonl`, environment unrecorded) are shown
alongside, labelled cross-environment. All runs: Ax 1.3.1, seeds
3001–3025, budget 80. Numbers from `analyze_h7.py` and the Amendment 1
comparison below.

## Pre-registered hypotheses

- **X1 — PASS.** 0 revisits in all 150 nominal runs (and in all 150
  ordinal-control runs). The dedup half of the Ax story is unchanged.
- **X2 — PASS.** On 5^5 Cat-Ackley nominal Ax solves **10/25** against
  the committed ordinal **18/25**. The same-environment ordinal control
  also solves 18/25, matching H1 on all 25 seeds, so on this benchmark
  the comparison is not affected by environment drift.
- **X3 — reported, no pass/fail.** Under H2's ranking metric on 5^5:
  Optuna TPE as shipped (7/25) vs nominal Ax (10/25): medians tie at
  16.18, the solve tie-break puts **nominal Ax ahead**; Optuna TPE
  memoized (22/25) vs nominal Ax: **Optuna TPE ahead**. The pair flips
  under equalization, as it did with ordinal Ax.

## Nominal vs ordinal, same environment

Seeds do not pair the two arms: with the same `random_seed`, ordered and
unordered Ax share only their first trial (the centre point), so the
comparison is between independent samples. Mann–Whitney on final best:

| benchmark | median best, nominal vs ordinal | nominal better / worse / equal (by seed) | Mann–Whitney p | ordinal control = H1? |
|---|---|---|---|---|
| 5^3 | both 4.4e-16 (25/25 solves) | 0/0/25 | 1 | 25/25 |
| 5^5 | 16.18 vs 4.4e-16 (solves 10 vs 18; Fisher p = 0.045) | 3/12/10 | 0.013 | 25/25 |
| 11^6 | 17.15 vs 15.95 (solves 0 vs 2) | 7/18/0 | 0.001 | 23/25 |
| pest_control | 14.08 vs 14.08 | 9/0/16 | 0.001 | 22/25 |
| func2C | −0.2057 vs −0.2046 | 12/13/0 | 0.57 | 0/25 |
| func3C | 0.0091 vs −0.4608 | 7/18/0 | 0.005 | 0/25 |

Ordinal treatment helps Ax on 5^5, 11^6 and func3C, hurts it slightly on
pest_control, and makes no detectable difference on func2C. Against the
cross-environment H1 rows, the first version of this analysis reported
func2C as favouring nominal (17/8); that was environment drift, and
func2C and func3C's H1 rows do not reproduce here on any seed.

Why ordering helps where it does (reviewer's check of the code): Cat-
Ackley uses one fixed level permutation (`make_cat_ackley(seed=1)`),
under which three of the five 5^5 dimensions are unimodal in label
order, so an ordinal kernel finds usable structure in this particular
benchmark instance. On func3C the label order is informative by
construction (several levels are identical and one multiplier is
linear in the label). Neither is a property of Ax.

## Seed-set sensitivity

The G-sweep ran the same nominal Ax configuration on 5^5 with seeds
4001–4025 (on a second machine) and got **16/25** solves. Against 18/25
ordinal that difference is not significant (Fisher p = 0.76); against
this experiment's 10/25, p = 0.16. Ordering helps Ax on 5^5 on this seed
set (p = 0.045 on solves, 0.013 on final best), but the size of the
effect is not established.

## What this changes in the article

1. Table 1 reports solves for 5^3 and 5^5 only. The Ax 5^5 cell becomes
   10/25 with a note that H1 treated Ax's categoricals as ordered (18/25)
   and that nominal Ax's solve count is seed-set sensitive (16/25 at
   seeds 4001–4025). The 5^3 cell (25/25) and all Ax revisit cells are
   unchanged.
2. Sec. 6's ranking-change count, recomputed by the reviewer with H2's
   own `ranking_pairs` and nominal Ax in both rankings, **stays at 3 of
   4**. One pair changes on 11^6: Ax ahead of Hyperopt TPE as shipped is
   lost after equalization, and nominal Ax also falls below SMAC there.
   (The claims audit already found the 3-of-4 count to be within
   bootstrap noise; this does not change that.)
3. The Optuna TPE 7 → 22 refund does not involve Ax and is unaffected.

## Corrections after review

From `REVIEW.md`: the H1 ordinal baseline is replaced by a
same-environment control (finding 1, Amendment 1); seed pairing is no
longer called paired, unpaired tests reported (2); "H1 overstated Ax"
replaced by the seed-set caveat (3); the mechanism sentence replaced by
the reviewer's check (4); the headline claim checked against Sec. 6's
actual Z-of-W (5); `failures.log` committed (6); Table 1 item limited to
the cells Table 1 has (7). Finding 8 (DESIGN said "H1 cell runner";
`run_h7.py` uses the released package's cell with the same caps) is
recorded here.
