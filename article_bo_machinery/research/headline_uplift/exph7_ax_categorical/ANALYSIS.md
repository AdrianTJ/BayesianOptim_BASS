# H7 analysis — Ax with unordered categoricals

Status: **unreviewed**. Per the repository rule, none of this enters
`main.tex` until someone other than its producer has reviewed it.

Inputs: `results.jsonl` (150/150 runs; 0 failures or timeouts in the final
attempt, and the 146 transient oneMKL/torch load failures in attempt 1
were all re-run, see `failures.log`), compared against the committed
ordinal Ax rows in `../exph1_matrix/results.jsonl` (same seeds 3001–3025,
budget 80). Numbers come from `analyze_h7.py`; full table in `h7_agg.md`.

## Pre-registered hypotheses

- **X1 — PASS.** 0 revisits in all 150 runs. The dedup half of the Ax
  story is unchanged: Ax never re-proposes a combination, with or without
  ordering.
- **X2 — PASS.** On 5^5 Cat-Ackley, nominal Ax solves **10/25** against
  the committed ordinal **18/25** (paired: nominal better on 3 seeds, worse
  on 12, equal on 10; Wilcoxon p = 0.020, descriptive). The probe from the
  claims audit (10/25) reproduces exactly.
- **X3 — reported, no pass/fail.** Under H2's ranking metric (median final
  best, solve-count tie-break) on 5^5:
  - Optuna TPE as shipped (H1: 7/25 solves) vs nominal Ax (10/25): both
    medians are 16.18, so the tie-break decides and **nominal Ax is ahead**.
  - Optuna TPE memoized (H2: 22/25) vs nominal Ax: **Optuna TPE is ahead**
    (median 4.4e-16 vs 16.18).
  So the Ax/TPE pair still flips under equalization with nominal Ax, as it
  did with ordinal Ax (18/25 was ahead of 7/25 and behind 22/25). What
  changes is the size of the margins: 10 vs 7 is a narrow lead before
  equalization, and 22 vs 10 is a wide gap after it.

## Descriptive, other benchmarks

| benchmark | nominal vs ordinal, paired (better/worse/equal) | median best, nominal vs ordinal |
|---|---|---|
| 5^3 | 0/0/25 | both solve 25/25 |
| 11^6 | 7/18/0, p = 0.001 | 17.15 vs 15.95; solves 0/25 vs 2/25 |
| pest_control | 9/2/14, p = 0.007 | 14.08 vs 14.08 |
| func2C | 17/8/0, p = 0.34 | −0.2057 vs −0.1805 |
| func3C | 7/18/0, p = 0.0006 | 0.0091 vs −0.7009 |

Ordering helped Ax where the integer label happens to track the
objective (Cat-Ackley's levels and func3C's encoding are not arbitrary
with respect to it), and slightly hurt it on pest control. That is the
expected signature of a spurious ordinal prior, and it is why the H1 Ax
rows overstated Ax on the Cat-Ackley benchmarks.

## What this changes in the article (after review)

1. Table 1 Ax rows: replace solve counts and medians with these
   (5^5: 10/25; 11^6: 0/25), with a note that H1 ran Ax ordinal.
2. Sec. 6: any sentence that uses Ax's 18/25 as a reference point uses
   10/25. The equal-budget headline (Optuna TPE 7 → 22) does not depend on
   Ax and is unaffected.
3. Revisit and dedup claims for Ax: unchanged (X1).
