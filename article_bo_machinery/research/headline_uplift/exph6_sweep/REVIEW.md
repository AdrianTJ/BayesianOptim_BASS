# Adversarial review: G-sweep (H6), 16,275 runs

Reviewer: independent of the producer. Checkout `/home/claude/BayesianOptim_BASS`,
branch `research/g-sweep-gp-wave` at `caea148`. Scripts referenced below are in
this scratchpad folder.

**Verdict: accept with corrections.** The data are complete and clean, and
GH1, GH2 (FAIL), GH4, GH5 (FAIL) and GH7 are correctly evaluated. Two problems
block the analysis as written. First, GH3's PASS depends on two choices in the
script that the DESIGN's wording does not support; under that wording GH3
fails. Second, the GH5 reading and the proposed article change generalize past
the data, and the analysis's own finding 2 contradicts them. Several
descriptive numbers and the cross-machine statement also need correcting.

---

## Findings

### 1. BLOCKING: GH3's PASS rests on the script's NaN rule and on cells the DESIGN excludes; under the DESIGN's wording it fails

**Claim:** "GH3 PASS: ρ(B, e) ≥ 0 in 86/89 = 97% of no-dedup cells."

**DESIGN wording:** "Spearman ρ(B, median e(B)) ≥ 0 in ≥70% of (no-dedup
arm × benchmark) cells **with all four budgets**."

**What I found (`g_gh3_detail.py`, `g_recompute.py`):**

- *Cell set.* `analyze_g.py` includes 15 optuna-gp cells that have only
  three budgets (all non-GP160 benchmarks). Under the DESIGN wording the
  denominator is 23 + 20 + 23 + 8 = **74**, not 89.
- *NaN handling.* 26 of the 89 cells have constant median e(B), all exactly
  0: every TPE-family cell on float-bearing spaces, plus several optuna-gp
  cells. Spearman ρ is undefined for these. The script counts them as passes
  ("constant e(B) counts as non-decreasing"), but NaN ≥ 0 is not true.
- *Negative cells.* Of the three ρ < 0 cells, two are numerical artefacts:
  - optuna-gp labs_n25: all revisits are 0, and e = −pigeonhole/B with
    pigeonhole about 1e-7.
  - optuna-gp pest_control: e = −1.78e-16 at B = 80/160. Whether this cell
    comes out negative or NaN depends on the last ulp of `-1.0/k` versus
    `-1/k` for k = 5^25. My first pass used int/int division and got
    pigeonhole = 0.0, which made this cell NaN and the count 87/89.
  The only substantive negative is optuna-gp catf_rosen_d4L7 (e falls from
  −0.004 to −0.026).

| reading | pass / cells | fraction | verdict |
|---|---|---|---|
| script as committed (NaN = pass, 3-budget cells in) | 86/89 | 0.97 | PASS |
| DESIGN wording (4-budget cells only), NaN = pass | 72/74 | 0.97 | PASS |
| DESIGN wording, NaN ≠ (ρ ≥ 0) | **49/74** | **0.66** | **FAIL** |
| script cell set, NaN ≠ (ρ ≥ 0) | 60/89 | 0.67 | FAIL |

`analyze_g.py` was committed before wave 1 and has not changed since
(`f93b2bd`), so the NaN rule is pre-registered in code. The DESIGN says
hypotheses are "evaluated by the letter", and ANALYSIS reports PASS without
mentioning that it depends on this rule. Separately, the hypothesis's content
is nearly vacuous here: only about 63 cells vary at all.

**Fix:** report GH3 as "PASS under the committed script's NaN convention;
FAIL (0.66) under the DESIGN's literal ρ ≥ 0 on four-budget cells". Name the
optuna-gp three-budget inclusion as a script/DESIGN mismatch. Report the
substantive number: of non-constant, non-artefact cells, ρ ≥ 0 in 60 of 61.

### 2. BLOCKING for the article change: "waste is a property of finite, all-discrete spaces" goes beyond the data and contradicts finding 2

**Claims:**
- (GH5 reading) "Revisit waste in these libraries is a property of finite,
  all-discrete spaces. It does not carry over to typical ML search spaces
  with continuous hyperparameters."
- (Article change) "They do **not** generalize to real ML spaces with
  continuous dimensions (GH5). Any sentence that implies otherwise is cut."

**What I found:**

1. *Optuna's own GP sampler contradicts it.* optuna-gp, a no-dedup arm named
   in GH5, has median e(80) = **+0.45** on yahpo_rpart_40981, a float-bearing
   ML space. All 25 runs revisit there (median 36 of 80, max 46). It also
   revisits exact float configurations in 75 runs across six float-bearing
   benchmarks (yahpo_rpart_41138: 41 runs, max 119 revisits at B = 160;
   yahpo_ranger_1489: max 28; ml_gb_bc: 19 runs). `g_agg.md` itself lists
   `optuna-gp: 1` in the GH5 dict, alongside optuna-tpe's 1. "These
   libraries" therefore cannot cover Optuna.
2. *For TPE, the absence is close to definitional.* The revisit key includes
   float coordinates rounded to 6 decimals. Any sampler that draws floats
   continuously, TPE and random included, almost never produces an exact
   repeat. Across all budgets and runs, TPE-family arms show **zero**
   revisits on every float-bearing space (`g_descriptive.py`). GH7's metric
   null asserts exactly this for random. GH5 FAIL is a legitimate
   pre-registered outcome. What it shows is that *exact-key* waste is absent
   when a continuous coordinate is in the key. It does not show that
   samplers stop re-spending budget on the discrete sub-configuration, which
   this metric cannot see.
3. *Attribution to "continuous dimensions" rests on n = 1.* Only one class-E
   space is all-discrete (ml_rf_digits, which does show waste: optuna-tpe
   e(80) = 0.153). Float presence is confounded with every other difference
   between ml_rf_digits and the other six spaces.

**Fix:** narrow the statement to something like: "TPE-family samplers make no
exact repeats on spaces with a continuous coordinate (as the exact-key metric
implies). Optuna's GP sampler does, heavily on YAHPO rpart." Drop "Any
sentence that implies otherwise is cut" as worded. The article can say the
exact-revisit measure does not show TPE waste on mixed ML spaces. It should not
say that waste does not generalize.

### 3. SHOULD-FIX: the cross-machine statement is false

**Claim:** "No cell mixes machines except smac contam_2p25/B20 (1 original
seed, 24 new) and smac pest_control/B160 (24 original, 1 new)."

**What I found:** I split rows by machine, taking the 10,203 rows present at
`0f52c12` as original-container rows; the history is append-only (zero
removed lines in `git diff 0f52c12 HEAD`). Three more cells mix machines: the
three GP timing-smoke rows kept from the original container.

- `optuna-gp nk_n20k8 B80` (1 original / 25)
- `ax ml_rf_digits B40` (1 / 25)
- `skopt-gp labs_n25 B80` (1 / 25)

For the last of these, `env_sandcastle.txt` itself records that the committed
original-machine row does not reproduce on sandcastle (0.2112 vs 0.128), yet
the original row is the one kept in the cell. The Ax smoke row is also the
only ax row labelled `"random_seed; one trial per ask"` without
`is_ordered=False`. Amendment 2 discloses this and argues that the categories
are string-valued. I confirmed ml_rf_digits' categoricals are strings
(`benchmarks_g.py` l. 193–194), so the row is consistent.

**Fix:** list all five mixed cells.

### 4. SHOULD-FIX: "0 failed runs, 0 cap-outs" cannot be verified from the repository

**Claim:** "16,275 rows … 0 failed runs, 0 cap-outs."

**What I found:** `failures.log` does not exist in this checkout and would be
git-ignored (`.gitignore:20 *.log`). `analyze_g.py` prints
"distinct failed run attempts: 0" whenever the file is absent, so that line
in `g_agg.md` is not evidence. The fast wave's history (`5b32d2e` "after
worker restart", `9a1a9d0` path-bug fix) suggests failed attempts did occur
and were retried. What the repository supports is "no missing keys" (verified)
and "max wall_s / cap = 0.52" (verified). It does not support "0 failed runs".

**Fix:** say "complete, with no unrecovered failures". Commit the logs from
both machines, or state that they are not retained.

### 5. SHOULD-FIX: GH6 reports classes A and E as "undefined" when only one benchmark in each is undefined

**Claim:** "A and E undefined (tied rankings)."

**What I found:** the per-benchmark τ values are:
- A: [nan, 0.87, 0.60, 0.80, 0.60]
- E: [−0.52, 0.58, 0.55, nan, −0.33, 1.00, 0.33]

`np.median` returns NaN when any element is NaN. The defined-benchmark medians
are **A +0.70** (4 of 5) and **E +0.44** (6 of 7). Class A's ranking
stability is the highest of any class and is currently hidden. The script also
omits the "solve tie-break where defined" that the DESIGN specifies for GH6.
With the tie-break the d3 NaN would probably resolve, since solve counts
differ among arms that sit at the optimum.

**Fix:** report nan-median with counts, and note the missing tie-break.
(GH6 is descriptive, so this does not affect any verdict.)

### 6. SHOULD-FIX: descriptive finding 1 asserts a mechanism the data do not test

**Claim:** "its duplicates come once it has found the optimum, so waste and
solve record coexist. (Solve times are not recorded … inferred from B6's
traces, not checked here.)"

The numbers are correct: e(80) A +0.403, B +0.432, C +0.012, D +0.006;
michal e(160) = +0.705; 25/25 on six of seven thresholded A–B benchmarks at
B = 80, rosen 9/25. But the first clause is stated as fact and the caveat then
withdraws it. These rows contain no timing information. Note also that at
B = 20 optuna-gp solves only 2/25 on griewank, 5/25 on michal and 0/25 on
rosen.

**Fix:** turn the first clause into a hypothesis ("consistent with B6's traces
that …"), or drop it.

### 7. SHOULD-FIX: descriptive finding 4 attributes the 10 → 16 swing to seed sets alone, but machine also changed

**Claim:** "Ax … solves cat_ackley_d5_L5 in 16/25 runs here … against 10/25
in H7, same version and settings. A 6-solve swing between seed sets …"

16/25 is correct (B = 80; 16/25 at B = 160, 12/25 at B = 40). The G-sweep Ax
ran on sandcastle and H7 on the container, so seed set and hardware both
changed. (My H7 review shows Cat-Ackley d5 does reproduce across
environments, which makes seed set the likelier cause, but ANALYSIS should say
so rather than "same settings".) This finding matters more than ANALYSIS
suggests. Fisher's exact test gives 16/25 vs H1's ordinal 18/25 p = 0.76, and
16/25 vs H7's 10/25 p = 0.16. The G-sweep therefore weakens H7's conclusion
that ordinal treatment inflated Ax's 5^5 record. That should be raised against
H7, not left as a footnote here.

### 8. MINOR: descriptive finding 3's range is off, and "indistinguishable" was never tested

- optuna-tpe e(160) class medians are B 0.419, C 0.606, D 0.444. The range is
  **0.42–0.61**, not "≈ 0.45–0.61".
- "Version 3.6 and 4.9 are indistinguishable (GH4)": GH4 is a threshold test
  (> 0.05), not an equivalence test. Per-benchmark e(80) differs by up to
  0.038 (michal 0.224 vs 0.262; griewank 0.298 vs 0.335). The class medians
  agree closely (A 0.303/0.321, B 0.294/0.307, C and D identical). Say
  "similar" and give the numbers.

### 9. MINOR: descriptive finding 5, "never exceed chance anywhere", holds only for cell medians

No ax, smac or skopt-gp cell has median e > 0 (verified). At the run level,
skopt-gp exceeds the pigeonhole baseline in 58 runs (for example d3/B40 seed
4009: 7 revisits vs 5.65 expected), and Ax has 3 runs with one revisit each on
yahpo_ranger_1489, where chance is 0, probably from active-parameter
canonicalization merging Ax proposals that differ only in an inactive value.
The article wording "never above chance" should say "in median".

### 10. MINOR: rounding and wording in the GH2 paragraph

- The pigeonhole at K = 125, B = 20 is 1.449, so "1.44" should be "1.45".
- e = −0.0725, which exceeds the bound by 0.0025, so "by 0.002" should be
  "by 0.0025".
- "the negative side was not thought through when it was registered" guesses
  at authors' intent. H1's P3 used the same two-sided |excess| form. State the
  fact (the clause is two-sided and the violation is on the below-chance side)
  without the speculation. The FAIL verdict is correct and is not softened.

### 11. MINOR: "What this changes", bullet 1 rounding

optuna-tpe e(80) on the 14 A–D benchmarks ranges from 0.200 (pest_control)
to **0.475** (maxcut_n20, contam_2p25). Write "+0.20 to +0.48", or give three
decimals. "(GH1, GH3, GH4)" cites GH3, which is subject to finding 1.

### 12. MINOR: undisclosed worker-count deviation

DESIGN fixes 4 workers. Sandcastle ran 12 (`env_sandcastle.txt`, commit
`a9181dd` "worker-count override"). Amendment 2 does not mention this. Thread
pinning was kept and max wall/cap is 0.52, so cap-outs could not have been
induced. Disclose it in a status note.

---

## Checked and found correct

- **Completeness.** 16,275 rows = planned matrix after Amendment 1
  (8 arms; smac excluded from class E; optuna-tpe-3.6 excluded from YAHPO; GP
  arms at B = 160 only on GP160). No duplicate keys, no missing keys, no extra
  keys (`g_recompute.py`).
- **Labels.** All 1,924 sandcastle ax rows say `is_ordered=False on
  categoricals`. Versions are uniform per arm (optuna 4.9.0 / 3.6.1, hyperopt
  0.3.0, skopt 0.10.2, ax 1.3.1, smac 2.4.0).
- **Pre-registration ordering.**
  - DESIGN `4278e4a` comes before any matrix rows.
  - `analyze_g.py` was committed at `f93b2bd` before wave 1 and is unchanged
    since.
  - Amendment 2 (`33a19c9`, 01:59) is earlier than any GP-wave row: only the 3
    smoke GP rows exist up to `a9181dd`.
  - The env record (`a9181dd`, 02:09) predates the first sandcastle checkpoint
    (`f1fa899`, 02:34).
  - results.jsonl is append-only across all checkpoints.
- **evals ≠ budget.** Only 29 smac rows on cat_ackley_d3_L5 (B = 80: 4;
  B = 160: 25), all flagged `early_termination`. This is consistent with H1's
  disclosed SMAC exhaustion behaviour.
- **sitecustomize shim** (`numpy.set_printoptions(legacy='1.25')`) cannot
  change revisit keys: `core.key_of` uses `f"{v}"` (str, not repr) for
  categoricals and `round(float(v), 6) + 0.0` for floats.
- **GH1** (both TPEs pass A–D; class E 0.000) and **GH4** (optuna-tpe-3.6
  passes A–D; E over ml_* only): recomputed independently.
- **GH2 FAIL.** Ax and smac have median revisits 0 in every covered cell at
  every budget. skopt-gp's only out-of-bound cell is d3/B20, e = −0.0725
  under either reading, |median e| or median |e|; its revisit counts are
  {0: 15, 1: 8, 2: 2}, all sandcastle rows.
- **GH5 FAIL** (max 1 of 7 for any no-dedup arm) and **GH7 PASS** (random has
  0 revisits on every float-bearing space at every budget and seed).
- **GH6** values for B, C, D and F (+0.32, +0.16, −0.20, +0.10) match.
- **Descriptive finding 2 numbers** match: 40981/B80 25/25 runs, median 36;
  41138/B160 21/25, median 9, max 119; ml_gb_bc/B80 18/25, median 1.
- **Descriptive finding 1 numbers** match (see finding 6).

Scripts: `g_recompute.py`, `g_gh3_detail.py`, `g_descriptive.py`;
`g_results_original_container.jsonl` is the `0f52c12` snapshot used for
machine attribution.
