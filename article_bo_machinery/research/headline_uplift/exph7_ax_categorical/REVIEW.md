# Adversarial review: H7 (Ax with unordered categoricals)

Reviewer: independent of the producer. Checkout `/tmp/claude-0/h7wt`, branch
`research/ax-categorical` at `af67a0d`. Scripts referenced below are in this
scratchpad folder.

**Verdict: accept with corrections.** X1 and X2 stand as written, and so does
X3's 5^5 comparison. The descriptive claims for pest_control, func2C and
func3C do not hold: the H1 ordinal baseline they are compared against does not
reproduce in H7's environment on those three benchmarks, so those comparisons
mix the ordering change with an environment change. The paired-test framing
also needs to change, and so do several pieces of interpretive wording.

---

## Findings

### 1. BLOCKING: on pest_control, func2C and func3C, the ordinal baseline does not reproduce in H7's environment

**Claim (ANALYSIS, "Descriptive, other benchmarks"):** pest 9/2/14 (p = 0.007),
func2C 17/8/0, func3C 7/18/0 (p = 0.0006, median 0.0091 vs −0.7009), and
"Ordering helped Ax on Cat-Ackley and func3C and slightly hurt it on pest
control."

**What I found:** I replayed H1's ordinal Ax with H1's own code path (the
research copy `headline_uplift/bo_audit` plus `machinery`) in the H7
interpreter (`/tmp/claude-0/ax-venv`, the env frozen in `env.txt`), using
`h7_rerun_cell.py ordinal <bench> <seed>`. Within this environment the replays
are deterministic: each cell gave bit-identical output on two runs. They do not
match the committed H1 rows on the mixed and pest benchmarks:

| cell (ordinal Ax) | H1 committed best | replay in H7 env (×2 where noted) |
|---|---|---|
| cat_ackley_d5_L5 / 3001 | 4.44e-16 | 4.44e-16 (matches) |
| cat_ackley_d6_L11 / 3001 | 16.873669089136243 | 16.873669089136243 (matches) |
| func3C / 3001 | −0.66566 | **+0.04208** (×2, identical) |
| func3C / 3002 | −0.71580 | **−0.71782** |
| func3C / 3003 | −0.02307 | **−0.45783** |
| func2C / 3001 | 0.002178 | **0.001333** (×2) |
| func2C / 3002 | 0.000510 | **−0.205857** |
| pest_control / 3001 | 14.0336 | **14.0800** (×2) |

Nominal H7 cells do reproduce exactly (d5/3001 = 16.1816, func3C/3001 =
−0.714247, identical to `results.jsonl`). The claims audit also reports an
exact match on 10 replayed 5^5 seeds. The drift therefore affects the
mixed-space and pest benchmarks and not Cat-Ackley. H1 recorded no environment
(no `pip freeze` in `exph1_matrix/`), so the cause cannot be pinned down: it
could be torch/botorch versions or hardware float paths.

**Consequence:** the 6 of 6 replayed non-Cat-Ackley cells changed under
environment alone, and in two of them by a large amount (func3C/3001: −0.67 →
+0.04; func2C/3002: 0.0005 → −0.206). The func3C, func2C and pest rows of the
paired table therefore measure "ordering plus environment". At pest/3001 the
same-environment ordinal replay (14.08) equals the nominal result (14.08), so
that seed's "nominal worse" call disappears. The sentence "Ordering helped Ax
on … func3C and slightly hurt it on pest control" is not supported.

**Fix:** re-run the ordinal arm in the H7 environment for all six benchmarks
(150 runs; ordinal is about 3–7× faster than nominal). Alternatively, restrict
the descriptive claims to Cat-Ackley d5/d6 and drop or caveat the
pest/func2C/func3C rows. Record this drift as a finding against H1's
reproducibility on the mixed benchmarks. It also bears on Table 1 and the
func2C/func3C rows used elsewhere.

### 2. SHOULD-FIX: the "paired" Wilcoxon has almost no pairing, because the seed only shares the first trial

**Claim:** "Paired per-seed comparison … (seeds are shared)", with paired
Wilcoxon p-values throughout ANALYSIS (p = 0.020 for X2, and others).

**What I found:** `h7_sobol_pairing.py` builds Ax 1.3.1 clients on 5^5 with
the same `random_seed`, once ordered and once unordered. Only trial 1 matches
between them: it is the centre point (3,3,3,3,3). Trials 2–6 differ completely
(seed 3001: ordered (4,3,1,2,1),(1,4,4,4,5)…; unordered
(1,1,2,1,1),(2,3,5,2,2)…). Ordered runs are deterministic on repeat. A shared
seed therefore does not couple the two arms' randomness beyond one point, and
the paired test is in effect a test on arbitrarily matched independent
samples. Its p-values are not wrong as numbers, but "paired" overstates the
design.

**Unpaired numbers (`h7_unpaired.py`):** Mann–Whitney gives d5 p = 0.013,
d6 p = 0.0017, pest p = 0.022, func2C p = 0.074, func3C p = 0.0016. Fisher
exact on solves gives d5 10/25 vs 18/25 p = 0.045 and d6 0/25 vs 2/25
p = 0.49. The direction on d5 survives. The solve-count difference is only
just under 0.05.

**Fix:** state that seed pairing shares only Ax's initial centre point, and
report Mann–Whitney or Fisher alongside or instead. The DESIGN asked for the
paired test, so keep it, but label it accurately.

### 3. SHOULD-FIX: "the H1 Ax rows overstated Ax" rests on one seed set, and the G-sweep contradicts it in size

**Claim:** "Either way, the H1 Ax rows overstated Ax on the Cat-Ackley
benchmarks", and Table 1 is to go from 18/25 to 10/25.

**What I found:** the G-sweep's nominal Ax (same driver, same Ax 1.3.1, seeds
4001–4025, B = 80) solves cat_ackley_d5_L5 in **16/25**. Fisher exact:
16/25 (G nominal) vs 18/25 (H1 ordinal) p = 0.76, and 16/25 vs 10/25 (H7
nominal) p = 0.16. Changing the seed set moves nominal Ax by 6 solves, about
as much as the 8-solve ordinal-to-nominal effect. (The G-sweep ran on a
different machine, so this comparison is also cross-hardware, but finding 1
shows d5 is reproducible across environments.) The 10/25 figure is one draw.
"Overstated" is directionally plausible (Fisher p = 0.045 on one seed set), but
the size of the change is not established.

**Fix:** the Table 1 note should say that the solve count is seed-set
sensitive (10/25 here, 16/25 at seeds 4001–4025). The ANALYSIS should cite
both and drop "overstated" in favour of "ordinal treatment helped Ax on this
seed set".

### 4. SHOULD-FIX: the mechanism sentence is checkable and is worded wrongly

**Claim:** "A plausible reading, not checked here, is that the integer labels
happen to track the objective on the first two, which is how a spurious
ordinal prior would show up."

**What I found:** both halves can be checked cheaply.

- Cat-Ackley uses a fixed permutation (`make_cat_ackley(seed=1)`,
  `perms[j] = default_rng(1000+j).permutation(L)`). For 5^5 the per-label |g|
  profiles are dim0 [16,0,16,33,33], dim1 [33,0,16,16,33],
  dim2 [33,16,0,33,16], dim3 [16,0,33,33,16], dim4 [33,33,16,0,16]. Three of
  the five dimensions are unimodal in label order, so this particular
  permutation does give an ordinal kernel some usable structure. The reading
  is plausible, and it is specific to the seed-1 permutation, not to Ax.
- func3C is not a case of labels that "happen" to track the objective. By
  construction h2 levels 3, 4 and 5 are identical (all Beale), and h3's
  multiplier is (h3−1) for levels 3–4. The label order is genuinely
  informative there, so "spurious" is the wrong word. Finding 1 also makes the
  func3C effect itself uncertain.

**Fix:** report the permutation profiles. Say that ordinal treatment exploits
structure that exists in this fixed permutation and in func3C's level
definitions. Drop "spurious".

### 5. SHOULD-FIX: the claim that the headline is unaffected was not checked against the article's actual headline (Z = 3)

**Claim:** "The equal-budget headline (Optuna TPE 7 → 22) does not depend on
Ax and is unaffected."

**What I found:** the article's Sec. 6 headline is "the apparent optimizer
ranking changes on three of the four benchmarks" (H2's Z-of-W). That ranking
includes Ax pairs. I recomputed Z with H2's own `ranking_pairs`, with nominal
Ax substituted for ordinal Ax in both the H1 and H2 rankings
(`h7_z_recompute.py`). **Z stays 3**, but the change set on 11^6 now
includes an extra flip: (ax ≻ hyperopt-tpe) as shipped is lost after
equalization. With nominal Ax, Ax also falls below SMAC on 11^6. The
conclusion holds, but ANALYSIS should show it rather than assert it.

**Fix:** add the Z recomputation and the 11^6 pair change.

### 6. SHOULD-FIX: failures.log is not committed, so the provenance trail is incomplete

**Claim:** "the 146 transient oneMKL/torch load failures in attempt 1 were
all re-run, see `failures.log`".

**What I found:** `failures.log` is matched by `.gitignore:20 *.log` and is
not tracked (`git status --ignored` shows `!!`). The local file does check out:
146 distinct keys, all present in results; 4 runs (d3 seeds 3001–3004)
succeeded on attempt 1; nothing failed after "=== attempt 2". But a reader of
the repository cannot verify any of this. DESIGN says failures are "logged in
failures.log, never dropped".

**Fix:** force-add the log (`git add -f`) or commit a summary of it.

### 7. MINOR: "What this changes", item 1 names a cell Table 1 does not have

Table 1 (`main.tex` around l. 491) reports solves only for 5^3 and 5^5, so
"11^6: 0/25" has no place in it. The 5^5 cell changes 18/25 → 10/25 (subject
to finding 3). No revisit column changes.

### 8. MINOR: the DESIGN wording and the run path differ

DESIGN says "run through the H1 cell runner with the fixed driver". `run_h7.py`
actually uses the released `bo-audit` package with the vendored
`benchmarks_h1` (`machinery` is not on the path in `CELL`). This was committed
together with the pre-registration, so it is not a post-hoc change, and the
parity test passes (`tests/test_benchmarks_h1_parity.py`: 6 passed). ANALYSIS
should still mention it. DESIGN also cites `../../claims_audit/REPORT.md`,
which is not on this branch; it lives on `research/claims-audit` (`f8d00bc`).

### 9. MINOR: X3's "narrow lead" rests on the tie-break alone

Before equalization, "nominal Ax is ahead" of TPE purely on the solve-count
tie-break, 10 vs 7, with identical medians of 16.18. Fisher p is about 0.54.
The article's "below Ax" then sits on a 3-solve gap. "Narrow" is fair; the
article sentence should not read as a firm ordering.

---

## Checked and found correct

- **Integrity.** 150 rows; no duplicate (benchmark, seed) keys; 25 seeds
  (3001–3025) per benchmark; library = ax, budget = 80, evals = unique = 80,
  version 1.3.1 on every row. All 150 rows carry
  `non_defaults = "random_seed; one trial per ask; is_ordered=False on categoricals"`.
  All 150 H1 ax rows carry the old label. Max wall 454 s, well under the caps.
- **Append-only history.** The checkpoint at `543e576` (64 rows) is a prefix
  of the final file (no `-` lines in the diff). DESIGN, `env.txt`, `run_h7.py`
  and the driver fix are all in the pre-registration commit `35431e0`, before
  any results.
- **Driver diff.** It changes only `is_ordered=False` and the label
  (`git show 35431e0 -- bo-audit/bo_audit/drivers.py`).
- **X1.** Total revisits = 0 over 150 runs: PASS.
- **X2.** d5 solves are 10/25 nominal and 18/25 ordinal at either threshold
  (< 1e-6 or ≤ 1e-9; no values fall between): PASS. better/worse/equal
  3/12/10 with only exact zeros among the equals, and scipy's default `wilcox`
  zero handling drops those 10. p = 0.0202 reproduced (pratt variant 0.018).
- **Every number in the ANALYSIS table and h7_agg.md** reproduced
  independently (`h7_recompute.py`): medians 17.15/15.95, 14.08/14.08,
  −0.2057/−0.1805, 0.0091/−0.7009; counts 7/18/0, 9/2/14, 17/8/0, 7/18/0;
  p-values 0.00108, 0.00661, 0.339, 0.000631; d6 solves 0/25 vs 2/25.
  Finding 1 affects how these are interpreted, not the arithmetic.
- **X3 logic matches H2's `ranking_pairs`.** Median with TIE = 1e-9, then
  solve-count tie-break. H7 uses < 1e-6 where H2 uses ≤ 1e-9; no value lies
  between, so this changes nothing. TPE H1: median 16.18, 7/25. TPE H2:
  median 4.4e-16, 22/25. Nominal Ax: 16.18, 10/25. Both orderings as stated.
  Under ordinal Ax, H2 TPE ties Ax on median and wins 22 > 18, as ANALYSIS
  says.
- **Reproducibility of H7's own rows.** Nominal d5/3001 and func3C/3001
  re-run bit-identically.
- **Missing fields.** None. "func2C" was omitted from the "ordering helped/hurt"
  prose (nominal was better 17/8 there, not significant); noted, not a
  separate finding.

Scripts: `h7_recompute.py`, `h7_unpaired.py`, `h7_sobol_pairing.py`,
`h7_z_recompute.py`, `h7_rerun_cell.py` (outputs `rr_*.json`).
