# E1 — Analysis

**Date:** 2026-08-21 · **Protocol:** as DESIGN.md (15 seeds, budget 80,
n_cand 1000, shared per-seed inits). Raw per-seed values: `results.csv`.
Runtime: 67 s for all 135 runs.

**Independent review: UPHELD** (see `REVIEW.md`). Two code defects found
(unstable RNG seeding; V6 min/max) were fixed and the experiment re-run
deterministically before this analysis was finalized; verdicts unchanged.
Final regenerated numbers: flip func2C best@10 −0.2009, final −0.2051;
func3C flip final −0.7062. Numbers below reflect the regenerated run where
they differ in the third decimal.

## Verdict: harness VALIDATED; one *reference number* found unreliable

Five of six pre-registered checks pass. The sixth (V2) fails in a way that,
on investigation, indicts the recorded R-side number rather than the Python
harness — a finding that matters for the article itself.

| Check | Result | Detail |
|---|---|---|
| V1 keep hits func2C optimum | **PASS** | mean final −0.2063 (exact optimum −0.206326, computed this cycle); reached by budget 10 |
| V2 flip early plateau ≈ −0.148 | **FAIL** | our flip arm: mean best@10 = −0.2006 |
| V3 func2C paired wins | **PASS** | keep beats flip **15/15** (R: 15/15); flip mean final −0.2053 (R: −0.205) |
| V4 func3C | **PASS** | keep −0.7221 (exact optimum −0.722140), flip −0.7106, wins **15/15** |
| V5 pure-cat Cat-Ackley d6/L11 | **PASS** | both arms reach <0.1 in 15/15 (both means 0.000) — pool not binding, as R found |
| V6 random sanity | **PASS** | random worse than both oracle arms everywhere |

## The V2 investigation: "−0.148 at budget 10" is not reproducible from the committed code

Three independent lines of evidence:

1. **Tail probability.** Under uniform sampling, P(func2C < −0.148) ≈
   1.40×10⁻³ (2M-sample MC). The generator's global half is 500 fresh LHS
   points per iteration regardless of variant, so by budget 10 the oracle has
   seen ≈5000 global draws → ≈7 expected points below −0.148. A 10-seed mean
   plateau of −0.148 at budget 10 is therefore inconsistent with the
   committed configuration (n_cand=1000) for *any* local-move variant.
2. **Exact historical variant.** The pre-fix generator (commit `1ecc5a1`)
   flipped k~U{1..min(3,n_cat)} coords, not per-coord 1/n_cat. Reimplemented
   exactly: mean best@10 = −0.1990, best@80 = −0.2053 — same story.
3. **Config check.** The pre-fix diagnostics script used cfg defaults
   (n_cand=1000, budget 60) and the same full-pool oracle; no smaller pool
   was configured that could explain a −0.148 plateau.

**Interpretation.** The direction and per-seed consistency of the R findings
reproduce perfectly (15/15 on both benchmarks, −0.205 final for the
restricted arm, optimum-hit for the permissive arm). The single figure
"−0.148 at budget 10" (diagnostics README; quoted in the article scaffold's
Section IV as a planned headline) is likely a transcription or
different-config artifact. **The article must not quote it** pending an
R-side re-run (queued: E4/R attempt, or user machine —
`Rscript code_files/3_categorical_diagnostics/run_diagnostics.R`).

## Claim-relevant magnitudes (this changes K2a's framing)

With n_cand=1000, the restricted-generator ceiling is real but **modest** on
func2C: −0.2053 vs −0.206326 at budget 80 (15/15 paired, but a small
absolute gap, driven by the big global half eventually finding the basin).
func3C shows a larger gap (−0.7106 vs −0.722140). The dramatic "caps any
surrogate far from the optimum" framing is **pool-size dependent**: the
smaller the candidate pool (i.e., the more realistic the expensive-BO
setting), the more binding the generator restriction should become. That is
now a designed axis of E2 (ceiling gap vs n_cand ∈ {50, 200, 1000}).

## New reference values (canonical for the article)

- func2C exact optimum: **−0.206326** at combo (h1=2, h2=2) — six-hump camel
  twice; continuous argmin (unit coords) ≈ (0.5225, 0.3218).
- func3C exact optimum: **−0.722140** at combo (2, 2, 1);
  ≈ (0.4775, 0.6782). (Scaffold's −0.7216 was slightly off; our oracle
  reaches −0.72214.)

## Threats to validity

- RNG streams and Cat-Ackley permutation instances differ from R (declared
  in DESIGN.md); all conclusions are statistical, none per-seed-vs-R.
- scipy LHS (init) vs R maximinLHS: same design class, not identical
  optimization; paired within-Python throughout.
- V5's "pool not binding" on d6/L11 matches R, but our permutation instance
  differs; the qualitative conclusion (oracle clears pure-cat pools at this
  size) is the claim, not the instance.

## What this changes for the ledger

- K1 (oracle audit works, cheap): **supported** — Python replication agrees;
  cost measured: 135 oracle/random runs in 67 s, zero surrogate fits.
- K2a: **supported with revised magnitude** — 15/15 direction confirmed;
  "far from optimum" framing demoted; pool-size dependence queued (E2).
- New sub-claim K2a-fig ("−0.148@10"): **unverifiable** — quarantined until
  an R re-run; article scaffold must drop or re-derive it.
- Exact optima now canonical (replaces −0.2062/−0.2063/−0.7216 variants
  floating in scaffold/README).
