# G-sweep analysis (H6)

Status: **reviewed** (`REVIEW.md`, independent reviewer, verdict "accept
with corrections"). This version applies those corrections; the list is
at the end. The first version is commit caea148.

Inputs: `results.jsonl`, 16,275 rows, exactly the planned matrix after
Amendment 1 (no missing, extra or duplicate keys). Every planned run is
present; failed attempts, if any, were retried, and no failure logs are
retained (`*.log` is git-ignored and the second machine wrote none), so
the repository supports "no unrecovered failures", not "no failures".
Longest run: 0.52 of its cap. Numbers come from the committed
`analyze_g.py` (unchanged since before wave 1) unless marked as the
reviewer's recomputation; full script output is in `g_agg.md`.

## Letter evaluation

| ID | verdict | evidence |
|---|---|---|
| GH1 | **PASS** | optuna-tpe and hyperopt-tpe both exceed e(80) > 0.05 in classes A–D (4/5); class E is 0.000 for both |
| GH2 | **FAIL** | ax and smac: median revisits 0 in every covered cell at every budget. skopt-gp: one cell outside \|e\| ≤ 0.07, cat_ackley_d3_L5/B20 at e = −0.0725 |
| GH3 | **FAIL by DESIGN's wording; PASS by the committed script** | see below |
| GH4 | **PASS** | optuna-tpe-3.6 passes classes A–D (class E over ml_* only, Amendment 1) |
| GH5 | **FAIL** | best no-dedup arms reach e(80) ≥ 0.05 on 1 of 7 class-E benchmarks (needed 4) |
| GH6 | descriptive | median Kendall τ(B20, B160) over benchmarks where it is defined: A +0.70 (4 of 5), B +0.32, C +0.16, D −0.20, E +0.44 (6 of 7), F +0.10. The script reports A and E as NaN because one benchmark in each is undefined; DESIGN's solve tie-break is not implemented |
| GH7 | **PASS** | random has median 0 revisits on every float-bearing space |

### GH3

DESIGN: Spearman ρ(B, median e(B)) ≥ 0 in ≥ 70% of no-dedup cells *with
all four budgets*. The committed script differs from that wording in two
ways: it counts cells where e(B) is constant (ρ undefined) as passes, and
it includes 15 optuna-gp cells that have only three budgets.

| reading | pass / cells | verdict |
|---|---|---|
| committed script | 86/89 = 0.97 | PASS |
| four-budget cells, undefined ρ = pass | 72/74 = 0.97 | PASS |
| four-budget cells, undefined ρ ≠ pass (DESIGN's letter) | 49/74 = 0.66 | **FAIL** |

Of the 25 four-budget cells that miss, 23 have waste exactly zero at
every budget (mostly TPE-family arms on float-bearing spaces), where
"waste grows with budget" has nothing to grow; 2 have ρ < 0. Among cells where
e(B) varies and is not a floating-point artefact, ρ ≥ 0 in 60 of 61
(reviewer's count; the one exception is optuna-gp on catf_rosen_d4L7,
where e is below chance and falls further). Two of the script's three
negative cells are artefacts of a ~1e-7 or ~1e-16 pigeonhole term.

### GH2

FAILED by 0.0025, on the side the clause did not anticipate. skopt-gp's
median revisits on that cell is 0 (fifteen 0s, eight 1s, two 2s); the
pigeonhole baseline at K = 125, B = 20 is 1.449, so e = −0.0725: below
chance, as a deduplicating sampler is on a small space. The verdict
stands. Cell medians of the three deduplicating arms never exceed chance;
individual runs do (skopt-gp 58 runs, ax 3).

### GH5

FAILED, and what it shows is narrower than "no waste on ML spaces":

- The TPE-family arms make **no exact repeats** on any space with a
  float dimension, at any budget. The revisit key includes floats rounded
  to 6 decimals, which a continuous sampler almost never repeats exactly,
  so this is close to built into the metric (GH7 asserts the same for
  random). The metric cannot see whether these samplers re-spend budget
  on the same *discrete* sub-configuration.
- **Optuna's GP sampler does repeat exact float configurations** on
  float-bearing ML spaces: median e(80) = +0.45 on yahpo_rpart_40981 (all
  25 runs, median 36 of 80), and revisits in 75 runs across six
  float-bearing benchmarks (yahpo_rpart_41138/B160 max 119). The
  mechanism is not checked.
- The one all-discrete class-E space (ml_rf_digits) shows TPE waste
  (optuna-tpe e(80) = +0.153). With n = 1 the difference cannot be
  attributed to the float dimensions specifically.

## Descriptive findings

1. **optuna-gp wastes heavily on finite spaces and still solves at B = 80.**
   e(80) per class: A +0.40, B +0.43, C +0.012, D +0.006; catf_michal_d5L9
   e(160) = +0.71. At B = 80 it solves 25/25 on six of the seven class A–B
   benchmarks with solve thresholds (catf_rosen_d4L7: 9/25). These rows
   carry no solve times, so whether its duplicates come after it solves
   (as the claims audit traced for 5^5, B6) is not tested here.
2. **optuna-gp float repeats on YAHPO rpart**: see GH5.
3. **TPE waste grows with budget** where it is nonzero, reaching
   e(160) = 0.42–0.61 for optuna-tpe on classes A–D. Optuna 3.6 also
   passes GH1's threshold (GH4); no test of 3.6 vs 4.9 equality was run.
4. **Ax with unordered categoricals solves cat_ackley_d5_L5 in 16/25 runs
   here** (seeds 4001–4025, second machine) against 10/25 in H7 (seeds
   3001–3025, original container). Seed set and machine both changed. This
   weakens H7's "ordering inflated Ax on 5^5": against ordinal 18/25,
   16/25 is not distinguishable (reviewer: p = 0.76).

## Cross-machine caveat

Objective values were checked identical across the two machines, but
some arms follow different optimizer paths for the same seed on
different hardware (examples in `env_sandcastle.txt`). Five cells mix
machines: smac contam_2p25/B20 (1 original, 24 new), smac
pest_control/B160 (24 original, 1 new), and the three GP timing-smoke
cells kept from the original container (optuna-gp nk_n20k8/B80, ax
ml_rf_digits/B40, skopt-gp labs_n25/B80; the last is one of the rows
`env_sandcastle.txt` records as not reproducing on the second machine).
The second machine ran 12 workers with one BLAS thread each, against
DESIGN's 4; Amendment 2 does not mention this. The waste metric is a
property of each run, so the GH verdicts do not depend on machine.

## What this supports for the article

- TPE-family exact-revisit waste generalizes across all 14 benchmarks in
  classes A–D (optuna-tpe e(80) from +0.20 to +0.475), in Optuna 3.6 and
  4.9 and in Hyperopt (GH1, GH4).
- On mixed ML spaces with continuous dimensions, the exact-revisit
  measure shows no TPE waste. The article can say that; it should not say
  waste "does not generalize", because the measure cannot see discrete
  re-spending and Optuna's GP sampler does waste there.
- "Waste grows with budget" holds where waste exists (60 of 61 varying
  cells) but fails GH3 as literally registered; any use must say so.
- optuna-gp's float repeats are new and need their own check before
  being cited.

## Corrections after review

From `REVIEW.md`: GH3 re-reported with both readings (was PASS only);
GH5 reading and article implications narrowed (was "waste is a property
of finite, all-discrete spaces; cut any sentence implying otherwise");
cross-machine list corrected from two cells to five, and worker count
disclosed; "0 failed runs" replaced by what the repository supports;
GH6 classes A and E given their defined-benchmark medians; finding 1's
unchecked mechanism removed; finding 4 attributes the swing to seed set
and machine and states its bearing on H7; ranges and rounding corrected
(e(160) 0.42–0.61, pigeonhole 1.449, GH2 margin 0.0025, e(80) top
0.475); "3.6 and 4.9 indistinguishable" removed; "never above chance"
restricted to cell medians.
