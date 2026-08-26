# H4 — Strong-surrogate generator attempt (design, written before any run)

**Date:** 2026-08-24 · **Purpose:** Settle the article's one inferential
exposure (K5 generator axis; Discussion "What the audit can and cannot
say"). E3 found no generator effect on real surrogates (all p ≥ 0.09;
pre-registered H1 FAILED, fallback recorded); the E7/E8 dial explains why —
generator sensitivity decays from total (σ=0) to undetectable at the
surrogate-grade anchor — and E3's sklearn GP (`n_restarts_optimizer=1`)
sits far below the machinery ceiling. **H4 hypothesis:** the E3 null is a
guidance-quality artifact; a strengthened real surrogate, run near the
ceiling on the pool size where the restriction binds hardest, shows the
generator effect. This is the second look at the generator axis on real
surrogates (E3 was the first); the multiplicity controls are fresh seeds,
letter-first evaluation, and the no-third-test rule.

## Protocol

Benchmark: func2C and func3C (E3's instances; E2 measured the oracle
generator gap there at ×14 and ×8 for n_cand 1000→50). Pool: **n_cand=50
only** — the binding pool. Dedup: combination (isolates the generator
axis). Arms: {keep, flip} × budget ∈ {80, 240} × 2 benchmarks.

**Strengthened surrogate (pre-named knobs; nothing else changes):**
GP-EI as in E3's `surrogates.py` but with `n_restarts_optimizer=10`
(10× E3) and `random_state` derived from the run seed. Budget 240 is the
second strengthening lever (3× E3's budget; more guidance data). The
budget-80 arm keeps E3 comparability; the budget-240 arms are the primary
cells.

**Seeds (fresh ranges; 1001–1025 E3, 1026–1050 E8, 3001–3025 ecosystem all
burned):** pilot 1051–1055 (5 seeds); confirmatory 1056–1080 (25 seeds).
Initial design n0 = max(2d+1, 8), shared per seed across arms; generator
RNG seed formula as in E3.

**Pilot gate (evaluated before any confirmatory run):** on the pilot
seeds at budget 80, the strengthened GP's mean final must close ≥50% of
the gap between E3's GP mean final and the per-seed E2 oracle ceiling
(n_cand=50, matching seeds where available, else oracle median) on
**func2C**. Gate fails → the strength knob is insufficient → **no
confirmatory run**; outcome recorded as "strength knob failed," the E3
fallback stands, and the article footnotes the attempt. This gate is what
makes both confirmatory outcomes decisive: a null from a surrogate still
far below the ceiling would be uninterpretable.

**Runs:** pilot 2×2×5 = 20; confirmatory 2 arms × 2 budgets × 2 benchmarks
× 25 = 200; oracle ceiling reference at budget 240 (keep-oracle × 2
benchmarks × 25) = 50. Total ≈ 270 E3-class runs, well under an hour.

## Pre-registered decision rules (confirmatory; evaluated by the letter)

Primary cells: budget 240 × {func2C, func3C}, n_cand=50. Per cell,
W = paired keep-better wins/25, p = Wilcoxon signed-rank.

| Outcome | Criterion |
|---|---|
| **SUPPORTED** | W ≥ 17/25 and p < 0.05 in **both** primary cells (house criterion, same as E3 H1 / E8) |
| **STRENGTHENED-NULL** | W ≤ 8/25 and p ≥ 0.05 in **all four** cells (both benchmarks × both budgets) |
| **INCONCLUSIVE** | anything else |

Budget-80 cells are descriptive (E3 comparability) and carry no decision
weight.

## Ledger consequences (fixed now)

- **SUPPORTED** → K5's generator axis upgrades from "not detected" to
  "detected under strengthened guidance": the article's practical-relevance
  passage becomes observational (real surrogate, pre-registered), and the
  Discussion paragraph is rewritten accordingly.
- **STRENGTHENED-NULL** → E3's fallback upgrades to "robust to
  strengthened guidance": the Discussion gains one sentence ("even with
  10× acquisition restarts and 3× budget, no measured effect"), which
  strengthens the honest-scope position.
- **INCONCLUSIVE** → K5 stays as-is; E3 fallback stands; **no third test**
  without a new pre-registered mechanism hypothesis. No re-litigation in
  any branch.

## Threats

- The strengthened GP is a different surrogate instance than E3's; the
  budget-80 arm plus unchanged everything-but-the-named-knobs is the
  comparability control.
- Third look at the generator axis overall (E2 oracle, E3, E8 dial, now
  H4): H4 is the first on *real surrogates under strengthened guidance*;
  fresh seeds and the stopping rule are the multiplicity control.
- Ceiling reference borrows E2's oracle runs (seeds 1001–1025) for the
  budget-80 gate; per-seed matching where seeds coincide, oracle median
  otherwise — stated here so the gate is computed identically by any
  re-analyst.
- The i.i.d.-noise degradation-model limitation (Discussion) does not
  apply here: H4 uses a real surrogate with no dial — this experiment is
  precisely the response to that limitation's "biased surrogate" gap being
  untested.
