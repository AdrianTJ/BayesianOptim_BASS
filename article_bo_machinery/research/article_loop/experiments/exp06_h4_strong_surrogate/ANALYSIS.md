# H4 — Analysis (pilot gate: decision record)

**Date:** 2026-08-25 · **Result: GATE FAILED — no confirmatory run, per DESIGN.md's pre-registered rule.**

## What happened

The pilot (20 runs, budget 80, n_cand=50, fresh seeds 1051–1055; runner
committed as 5074596 before any result) ran the strengthened GP-EI
(n_restarts_optimizer 10, seed-derived random_state) against the DESIGN's
pre-named pilot gate on func2C:

| quantity | value |
|---|---|
| E3 vanilla GP mean final (keep+combination, b80, n_cand=1000), committed E3 results | −0.104869 |
| E2 oracle ceiling, keep+combination, n_cand=50, b80 (committed E2 results) | −0.206319 (median) |
| Gate threshold ($\ge$50% of the gap closed) | ≤ −0.155594 |
| **Strengthened GP mean final (pilot, b80, n_cand=50)** | **−0.026144** |
| Gap closure | **−77.6% (worse than E3; gate FAILED)** |

Pilot cells (mean best_b80): func2C keep **−0.0261** / flip −0.0104;
func3C keep **−0.2841** / flip −0.1926.

## Mechanism attribution (diagnostic probe, not part of the confirmatory path)

The collapse to −0.026 is the cost of the **binding pool (n_cand=50), not a
defect in the strengthened knob**. Running E3's **vanilla** `gp_ei_acquire`
(restarts=1) on the identical pilot gives the same mean on the gate arm,
−0.026144, and E3's own GP reached −0.105 only because it ran at
n_cand=1000. The strengthened surrogate therefore sits well short of the
ceiling in pool terms, so it cannot count as "near the ceiling," and the
pre-named gate correctly refused to let an uninterpretable null become the
H4 finding.

**Correction (post-review, same day): the knob is not inert.** The first
draft of this file generalized the gate arm's result into "10 restarts leave
the Matern-EI optimum unchanged at this pool." That is false, and the
pilot's own data refutes it. Comparing the recorded strengthened rows
against a vanilla (restarts=1) baseline on the same seeds, the knob changes
the trajectory in 4 of the 20 pilot runs, in mixed directions:

| cell | seeds changed by restarts=10 | effect on mean best_b80 |
|---|---|---|
| func2C keep | none as recorded (but see the re-run note below) | unchanged |
| func2C flip | 1054, 1055 | better (−0.0104 vs −0.0075) |
| func3C keep | 1055 | better (−0.2841 vs −0.2556) |
| func3C flip | 1055 | worse (−0.1926 vs −0.2193) |

Inertness held only on the single arm the gate evaluated. The defensible
statement is the pool statement above; nothing here licenses a claim about
what acquisition-restart count does to GP-EI in general.

## Ledger / paper consequences applied per DESIGN.md (fixed before the run)

- **Outcome: gate failed → "strength knob is insufficient" → NO confirmatory
  run; E3 fallback stands; K5 unchanged.** The Discussion's
  practical-relevance passage carries one footnote recording that the
  attempt failed its gate, and naming the pool-size difference behind the
  −0.026 vs −0.105 comparison so the shortfall is not misread as evidence
  about strengthening.
- The diagnostic probe (a real surrogate at n_cand=50 landing far below the
  same-pool oracle ceiling) is consistent with E3's fallback, but per DESIGN
  no confirmatory or declarative claim is authorized from it: it is recorded
  here in the open, not staged into the ledger as SUPPORTED/REFUTED.
- No confirmatory matrix, no oracle-240 reference, no paper-text change
  beyond the footnote.

## Why this is the designed outcome, not a bug

The pilot gate exists precisely to refuse a null from a surrogate still far
below the ceiling (DESIGN "Threats" / "Pilot gate" paragraphs). H4's test:
*that* would have been uninterpretable; the gate returned it before any
confirmatory spend. In that sense the experiment worked as designed — it
failed the pre-registered precondition and stopped, which is the honest
intersection of the audit's scope and the generator-axis claim's limits.

## Re-run finding (recorded after the gate decision)

Re-running the committed runner in the same environment that produced the
pilot (the local `code_files/5_nlp_hpo/.venv`: numpy 2.4.6, scikit-learn
1.9.0, scipy 1.17.1) reproduces 19 of the 20 rows of `results_pilot.csv`
bit-exactly. One row does not: **func2C, seed 1055, keep**, which re-runs to
−0.156604 against the recorded −0.041748. The re-run value is stable across
repeat trials, across serial and pooled execution, and across BLAS thread
counts of 1, 2, 4, 8 and unset, so it is not run-to-run noise, a parallelism
artifact, or a threading artifact. The cause has not been isolated.
Consequences, in order of importance:

- **The gate decision is unaffected.** The gate arm's mean moves from
  −0.026144 to −0.049115 against a threshold of −0.155594. The gate fails by
  a wide margin either way, so the pre-registered outcome (no confirmatory
  run) stands.
- **The inertness observation loses its last support.** On re-run the
  strengthened and vanilla values differ on this arm too.
- `results_pilot.csv` is left as run rather than retro-fitted to the re-run.
  A re-analyst should expect the func2C keep mean to come out near −0.049,
  and should treat any single pilot row as environment-fragile at this pool.

## Defect in DESIGN.md's confirmatory rule (recorded, never took effect)

DESIGN.md's `STRENGTHENED-NULL` criterion (W ≤ 8/25 with p ≥ 0.05 in all
four cells) is mis-specified. W ≤ 8 is the mirror image of the SUPPORTED
band, so it demands evidence that flip beats keep, which is a reversal
rather than a null; under a true null P(W ≤ 8) ≈ 0.054 per cell, on the
order of 1e-5 across four cells, while the expected result of a genuine
null (W ≈ 12–13) would have been recorded as INCONCLUSIVE. The house form,
used by E8, is `W ≤ 17 with p ≥ 0.05`. The defect is recorded here rather
than corrected in DESIGN.md, which stays frozen as pre-registered. It never
bound: the pilot gate stopped the confirmatory run before any decision rule
was evaluated.

## Reproducibility

- Runner: `run_h4.py` (commit `5074596` + thread-pin fix), mode `pilot`.
- Raw results: `results_pilot.csv` (20 rows).
- Gate constants sourced from committed `exp03`/`exp02` results.csv (no
  re-derivation, no hand-picked numbers). The gate is evaluated on the
  **keep** arm, against the E2 oracle **median** at n_cand=50; DESIGN's
  "matching seeds where available" clause is vacuous here, since the pilot
  seeds (1051–1055) do not overlap E2's (1001–1025).
- Header dates: DESIGN.md's header reads 2026-08-24, but it was committed
  2026-08-25 15:46:09 (`33f1d1e`), the runner at 17:05:46 (`5074596`), and
  the results at 17:25:36 (`902e07e`). The pre-registration order is intact
  in git; only the hand-written header date is off by a day, and DESIGN.md
  is left unedited so the pre-registered text stays frozen.