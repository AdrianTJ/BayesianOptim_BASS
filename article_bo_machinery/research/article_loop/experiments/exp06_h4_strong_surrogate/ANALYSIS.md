# H4 — Analysis (pilot gate: decision record)

**Date:** 2026-08-24 · **Result: GATE FAILED — no confirmatory run, per DESIGN.md's pre-registered rule.**

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

The strengthened GP did not move the vanilla GP at all at n_cand=50: running
E3's **vanilla** `gp_ei_acquire` (restarts=1) on the identical pilots gave the
same mean, **−0.026144**. So the collapse to −0.026 is the cost of the
**binding pool (n_cand=50), not a defect in the strengthened knob** — and the
knob itself (10× restarts) is *inert* at this pool: Matern+WhiteKernel L-BFGS
finds the same optimum regardless of restarts on these 9-start, budget-80
designs. The strengthened surrogate sits **below** the vanilla E3 surrogate
(in pool terms), so it cannot count as "near the ceiling," and the pre-named
gate correctly refused to let an uninterpretable null become the H4 finding.

## Ledger / paper consequences applied per DESIGN.md (fixed before the run)

- **Outcome: gate failed → "strength knob is insufficient" → NO confirmatory
  run; E3 fallback stands; K5 unchanged.** The article's Discussion footnote
  is updated (§) to record that the strengthening attempt did not move
  the surrogate at the binding pool.
- The diagnostic probe (vanilla GP at n_cand=50 == strengthened GP) is a
  *reinforcement* of E3's fallback (audit over-ceiling, even when a knob is
  added), but per DESIGN no confirmatory/declarative claim is authorized for
  capture from it — it is recorded here in the open, not staged into the
  ledger as SUPPORTED/REFUTED.
- No confirmatory matrix, no oracle-240 reference, no paper-text change
  beyond the footnote.

## Why this is the designed outcome, not a bug

The pilot gate exists precisely to refuse a null from a surrogate still far
below the ceiling (DESIGN "Threats" / "Pilot gate" paragraphs). H4's test:
*that* would have been uninterpretable; the gate returned it before any
confirmatory spend. In that sense the experiment worked as designed — it
failed the pre-registered precondition and stopped, which is the honest
intersection of the audit's scope and the generator-axis claim's limits.

## Reproducibility

- Runner: `run_h4.py` (commit `5074596` + thread-pin fix), mode `pilot`.
- Raw results: `results_pilot.csv` (20 rows).
- Gate constants sourced from committed `exp03`/`exp02` results.csv (no
  re-derivation, no hand-picked numbers).