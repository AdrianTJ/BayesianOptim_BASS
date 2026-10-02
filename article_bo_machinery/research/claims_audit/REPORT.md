# Claims audit of `main.tex` (2026-10-01)

An adversarial review of the article's claims by a session that did not
produce them, requested by the author before submission. Every number
below was recomputed from the committed raw results (read-only) by
`recompute.py` in this folder, or produced by fresh runs whose scripts and
outputs are kept here. Nothing under `research/` other than this folder
was changed, and `main.tex` is untouched: what to change in the paper is
the author's call.

## Verdict in one paragraph

The measurements are real. Every table cell, count, and p-value that
`main.tex` quotes reproduces from the raw files, the revisit counts
reproduce in a fresh environment, and the two short proofs are correct.
What is not solid is a layer of interpretation on top of the
measurements. Five things need to change before submission because a
reviewer can falsify them from the repository itself: the "ranking changes
on 3 of 4 benchmarks" headline, the "oracle ceiling was never exceeded"
claim, the Hyperopt refund result, the claim that pre-registration is
verifiable from git history for the controlled experiments, and the Ax
rows, which ran with categories treated as ordered integers. Several more
statements are accurate but framed more strongly than the evidence.

## Files in this folder

- `recompute.py`: recomputes every quoted number from the committed raw
  results (sections C1 to C6 of its output map to the findings below).
- `replay_tpe_cells.sh`, `replay_one.py`: fresh runs with the recorded
  library versions; outputs in `replay_*.jsonl`; environments in
  `env_*.txt` (Optuna 4.9.0, Hyperopt 0.3.0, scikit-optimize 0.10.2,
  Ax 1.3.1, numpy 2.4.6, scipy 1.17.1).

## A. Claims that need to change

### A1. "The apparent optimizer ranking changes on three of four benchmarks"

Abstract, Introduction, contribution 2, Sec. 6, protocol item 6.

- The count reproduces under the pre-registered metric (median final
  best, solve-count tie-break, any pairwise flip counts).
- That metric changes almost every time from seed noise alone. Two
  bootstrap replicates of the *same* as-shipped data give different
  rankings with probability 0.91 (5^3), 0.76 (5^5), 0.82 (11^6) and 0.20
  (5^25). The expected "Z" under no effect at all is about 2.7 of 4.
- None of the pairwise orders that flipped is significant after the
  refund (Mann-Whitney p between 0.08 and 1.0). On 5^3 every flip is a
  tie-break on solve counts with all medians at 0. On 11^6 the flip is
  0.45 apart in median with p = 0.62.
- Solve counts are not stable across environments (A3), which on its own
  moves these tie-breaks.

What *is* solid: refunding duplicates makes Optuna TPE much better on
5^5, from 7 to 22 of 25 solves, better on 16 seeds and worse on none.
A fresh replay gives 5 to 20. This is a paired, within-library effect and
it survives any reasonable test.

Suggested change: lead with the paired Optuna TPE result; report pairwise
order changes descriptively, or gate "ranking change" on a significant
pairwise difference. Drop "3 of 4" from the abstract.

### A2. "Empirically was never exceeded by any surrogate arm" and "0 of 2,250"

Sec. 5 (oracle remarks), contribution 3, Appendix Prop. B.

- The 2,250 count reproduces, but it includes the TPE and Random arms (750
  of the runs × 3 budgets) and compares every arm, including the
  restricted-generator and encoding-dedup arms, against the *strongest*
  oracle (permissive generator, combination dedup).
- The audit's own definition is the ceiling of a given (generator, dedup,
  budget). Matching each GP/RF arm to the oracle under its own generator
  and dedup, surrogates beat their ceiling in **5 of 1,800** comparisons,
  all on Func-2C with the restricted generator (for example seed 1022, RF,
  budget 10: −0.2048 against the oracle's −0.1867).
- These are exactly the adaptive-pool violations Prop. B's discussion
  says are possible, so they do not break any proved statement. They do
  falsify the sentence in Sec. 5.

Suggested change: replace "never exceeded" with the like-for-like record
(5 of 1,800, all restricted-generator, magnitudes up to 0.018 at budget
10), and present them as empirical evidence that the ceiling is not
pathwise for adaptive pools. This strengthens the honesty of the theory
section.

### A3. Hyperopt's refund "does not help"

Sec. 6, contribution 2 ("converts for one TPE implementation and not the
other"), claim H2-REFUND.

- Replaying Hyperopt 0.3.0 on 5^5 with the committed drivers gives **8 of
  25** solves as shipped (committed: 3), deterministic within this
  environment (two replays identical on all 25 seeds).
- The memoized replay gives 10 of 25 and is strictly better than its
  paired as-shipped run on 6 seeds (committed: 1). The pre-registered
  strict-improvement prediction that failed in the committed data passes
  here.
- The difference comes from transitive dependencies (numpy/scipy), which
  the results do not record; the paper already discloses that per-seed
  runs do not replay. What it does not say is that the aggregate solve
  counts move this much.

Suggested change: drop "freeing its wasted budget does not help" and the
heterogeneity framing, or report both environments. Record the full
`pip freeze` for any rerun.

### A4. Pre-registration "independently verifiable from the repository history"

Sec. 9 (Experiments) and the K10 footnote.

- H0, H1, H2 and H4 verify: each DESIGN.md is committed before its
  results (H1 DESIGN 15:13, results only as a 1-line smoke file until
  18:57).
- E1 to E8, which carry the oracle table, the dedup table, the guidance
  dial and the K10 retest, first appear in a single commit (8f879d3)
  together with their results, on every branch and pull-request ref on
  GitHub. The history that showed the ordering was rewritten.
- The K10 footnote's "criteria were fixed and committed before the run"
  is therefore not checkable by a reader.

Suggested change: recover the original commits (an old local clone of
`claude/machinery-confound-article` would have them) and push them as a
tag, or reword to say ordering is verifiable for the ecosystem program and
recorded in REVIEW files for the controlled experiments.

### A5. Ax was run on ordered integers, not categorical parameters

The audit's benchmark spaces encode category labels as integers
(`[1, 2, 3, 4, 5]`; pest control `[0..4]`). The Ax driver passes them as
`ChoiceParameterConfig(parameter_type="int")` with no `is_ordered`, and
Ax 1.3.1 then defaults to `is_ordered=True` (confirmed on the installed
library: all five parameters of 5^5 come out ordered). Every Ax run in
Tables 1 and 2 therefore treats permuted Cat-Ackley levels as an ordinal
scale. Optuna, Hyperopt, scikit-optimize and SMAC (ConfigSpace
`Categorical`) all receive them as nominal.

Re-running Ax on 5^5 with `is_ordered=False`: **10 of 25** solves instead
of 18 (on the 10 seeds also replayed as shipped, 4 against 9; the
as-shipped replay matched the committed runs exactly). Revisits stay at 0
either way, so the waste table and the dedup claim for Ax are unaffected.
What changes: Ax's solve counts in Table 1, and the headline example in
Sec. 6, where Optuna TPE's refund is said to reverse its order "against
Ax". Against Ax run as a categorical optimizer (10/25), Optuna TPE as
shipped (7/25) is already close and the refunded 22/25 is far ahead.

Suggested change: re-run the Ax rows with `is_ordered=False` (or string
labels) and fix the driver in `bo-audit` so users do not hit the same
default; alternatively disclose that Ax ran with ordinal treatment. The
"documented defaults" fairness rule does not cover this, because the
ordinal choice came from the audit's integer encoding, not from Ax.

## B. Accurate but overstated

### B1. "Encoding-level dedup re-spends a median of 78 of 80 oracle picks" (abstract)

The oracle solves Cat-Ackley 5^3 within the first 10 evaluations on all 25
seeds, so all 78 revisits are re-picks of an optimum already found. They
cost nothing. The figure shows the mechanism, not a cost. The cost
evidence is the GP row of Table 4 (20 of 25 solves, p = 0.025, replicated
in the second generator cell at 19 of 25, p = 0.014). Suggest removing the
78/80 from the abstract.

### B2. "The duplicate leak taxes every method family by a comparable margin"

Comparable in revisit counts (52 to 55 of 80), not in outcomes. The RF
drop (25 to 23 solves) is not significant (p = 0.16). On a purely
categorical space the keep and flip cells are identical machinery run
with different RNG streams, so they are replicates: RF under encoding
dedup solves 25 of 25 in the other replicate. Only GP shows an outcome
cost.

### B3. "No real surrogate was measurably affected by the generator"

True, but the surrogates tested barely guide. On Func-3C the sklearn GP's
median (−0.154) is worse than Random's (−0.172); the thesis's R GP-BO
reaches −0.42 there. In the K10 retest, the Func-2C anchor (σ = 10) lands
at Random level (keep median 0.000 and −0.017) rather than at GP level
(−0.153), so "the noise level where attained performance matches a
surrogate of this grade" is loose for Func-2C. A second post-anchor
residual exists that the footnote does not name: Func-2C, N = 50, σ = 30,
18 of 25, p = 0.030.

### B4. "Both failure modes have live counterparts in the audited libraries"

The ecosystem audit measures revisits only. Nothing in it shows a
deployed library with the restricted generator; Optuna GP's saturation
(about 30 unique configurations in 400 proposals) is a different
mechanism. Suggest limiting "live counterpart" to duplicate leakage.

### B5. Abstract: "We prove the audit is a genuine ceiling for exogenous candidate pools"

Correct, but every experiment in the paper uses adaptive pools. The
abstract should say the experiments are covered by the conjecture, not
the lemma.

### B6. "Solving masks waste" (Optuna GP)

A fresh trace of 10 seeds (exact replays of the committed runs) shows
Optuna GP solving 5^3 at evaluation 9 to 14, and all but at most one of
its 52 to 55 revisits come *after* the optimum is found. On the cells
where it matters (11^6) it revisits once. So the waste in this cell is
real but cost-free: nothing was masked. The 5^5 cell (38 revisits, still
25/25) is likely the same. "Solving masks waste" reads as if the waste
hurt; "the heaviest waster re-spends its budget on an optimum it has
already found, and a convergence curve cannot show this" is what the data
supports. H2-SAT (about 30 unique configurations in 400 proposals) is
consistent with this: the sampler parks on the incumbent once it has it.

### B7. The Optuna maintainers' stated rationale

optuna#5440 was closed by a maintainer saying repeated suggestions are
intentional so stochastic objectives can be re-sampled. The paper's
noise discussion covers this, but citing the rationale next to the issue
numbers pre-empts the obvious reviewer response.

## C. Small corrections

- Sec. 5: "per-seed residual ≤ 3×10⁻⁵ even at N = 50" holds for Func-2C
  only; Func-3C at N = 50 reaches 1.2×10⁻⁴.
- Sec. 7: the 8/17/0 p = 0.006 is R's normal approximation; the exact
  signed-rank p for 8 positive differences is 0.0078. The other thesis
  p-values also come from the approximation; all conclusions hold under
  the exact test.
- Appendix Lemma A is stated without the dedup mask. The proof extends:
  if the best point π picked was already evaluated by the oracle, the
  oracle's best-so-far is at most its value; otherwise it was admissible
  to the oracle, whose pick was at least as good. Worth one sentence.
- hyperopt#608 reports 48 trials on the same best combination out of 500
  (plus repeats of others); "48 identical trials" is a fair paraphrase.

## D. Verified as stated

- Table 1 (in-the-wild): every median, excess and solve count; SMAC's six
  early terminations at 59 to 75 evaluations; 349 of 350 mixed-space runs
  at zero revisits; Optuna GP's 52 to 55 revisits and 25 to 28 unique
  configurations on 5^3; Ax and SMAC at zero revisits in all 300 runs.
- Revisit counts replay in a fresh environment with the recorded library
  versions: Optuna TPE 5^5 median 30 (committed 29), Hyperopt 5^5 14 (14),
  Optuna TPE 5^25 16 (16).
- Optuna GP's 400-proposal saturation at a median of 30 unique.
- Table 3 (oracle): every mean and gap, 25/25 wins in all six cells,
  p = 5.96×10⁻⁸ (the exact n = 25 floor), the ×14 and ×8 ratios.
- Table 4 (dedup) and Table 5 (thesis W/T/L), and the NLP task's W/T/L.
- The K10 anchor statistics (all four cells p ≥ 0.13) and the σ = 100
  residual (18/25, p = 0.004).
- The pigeonhole formula, and Prop. C (a) and (b): the Gaussian product
  formula, its limits and monotonicity, and the Gumbel/softmax identity
  and logit expression are all correct.
- Citations checked: hyperopt#608, optuna#5440 and #2021 say what the
  paper says; Recht et al.'s 11 to 14% ImageNet drop is correctly quoted.
- The repository's three checkers pass and the bo-audit suite passes
  (17 tests).

## Not checked

Ax's ordinal effect was measured on 5^5 only. SMAC was not replayed (it needs its own venv with sklearn < 1.8). The
related-work characterizations of papers other than the three issue
trackers and Recht et al. were not re-read. The R/BASS pipeline was not
re-run.
