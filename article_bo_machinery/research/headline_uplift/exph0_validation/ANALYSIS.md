# H0 — Analysis (instrumentation validation)

**Date:** 2026-08-22 · **Protocol:** as DESIGN.md. Raw: `results.json`,
`h0_stdout.log`. **Gate verdict: OPEN — all pre-registered checks pass.**
H0 is a validation gate; nothing here is a headline measurement (n=10
seeds, one benchmark). H1 does the measuring.

## Counter correctness (G-gates)

| gate | result |
|---|---|
| G1 scripted repeats | exact: 6 revisits, 4 unique |
| G2 continuous false positives | 0 in all 5×200-eval runs |
| G3 pigeonhole expectation | mean 36.0 vs analytic 36.0 |
| G4 recount | exact match, all 20 runs — with the review's caveat that as implemented this is a transcription check (same algorithm re-run on the same call log), not an independent semantic check; G1's hand-computed case carries the semantic validation |

## Documented-behavior detection (D-gates), Cat-Ackley d3/L5, budget 80, seeds 2001–2010

| library (defaults) | median revisits/80 | mean | mean best | vs pigeonhole baseline (~20.7) |
|---|---|---|---|---|
| optuna-tpe | **46** | 45.6 | 1.82 (best) | **+25 above random's rate** |
| hyperopt-tpe | **35** | 34.0 | 3.64 | +13 above |
| skopt-gp | 19 | 18.3 | 0.000 | ≈ random's rate |
| random | 21 | 20.0 | 7.27 | = (analytic 20.7 ✓) |

D1 (hyperopt ≥5): PASS at 35. D2 (optuna >0): PASS at 46. D4 (random
pigeonhole ±5): PASS. Detection gates validated against the ecosystem's own
admitted issues — the instrumentation sees what the trackers describe, at
larger magnitude than the gates required.

## Three observations to carry into H1 (hypotheses, not conclusions)

*(This section was rewritten after adversarial review: the first draft
claimed optuna "wins the benchmark", contradicting the table above —
skopt-gp wins it, 0.000 mean best with 10/10 exact optima vs optuna's
1.82 with 9/10. The corrected reading follows.)*

1. **Dedup-equipped machinery wins on both axes.** The one library whose
   pipeline attempts duplicate handling (skopt-gp) both solves the
   benchmark on every seed AND shows the least excess waste (≈ pigeonhole
   only); the no-dedup defaults waste 44–57% of budget and solve less
   reliably (optuna 9/10, hyperopt with mean best 3.64). H1 hypothesis:
   across benchmarks, the presence/absence of combination-aware dedup in a
   library's machinery predicts both its waste and its solve rate — the
   paper's protocol claim, observed in deployed code. The deployment
   irony to verify carefully before any framing: the waste-heavy sampler
   (optuna TPE) is the ecosystem's most-installed default, while the
   dedup-equipped winner (skopt) is archived.
2. **The right metric is excess-over-pigeonhole**, not raw revisits: on a
   125-combination space at budget 80, ~20.7 revisits are unavoidable for
   any non-dedup sampler. H1's headline metric: revisits minus the
   matched pigeonhole baseline (and, where meaningful, minus a
   dedup-controlled variant of the same library).
3. **skopt's dedup detects far more than it prevents** — measured, per
   review reconciliation: its duplicate warning fired ≈ 59.7 times/run
   (597 total across 10 seeds) while final revisits were 18.3/run, i.e.
   the unconditional-uniform fallback (source-verified: `space.rvs()`
   unfiltered against history) finds a genuinely new point on ~70% of
   firings and re-collides on ~30%. Detection without combination-aware
   replacement, quantified. H1 cell candidate; diplomatically framed
   (archived library, behavior visible in its own warnings).

## Deviations & threats

- SMAC3 install broken in-container (private-sklearn-symbol import from a
  resolved old build); pinned retry queued for H1 — logged, not dropped.
  Ax/BoTorch installed; optuna-gp deferred to H1 by design.
- Single benchmark, 10 seeds: sufficient for gates (analytic baselines +
  admitted-issue detection), insufficient for any library-level claim.
- Revisit keys use exact category tuples (6-decimal continuous rounding
  irrelevant here — pure categorical); G2 guards the continuous side.
- Wrapper fairness: all libraries at documented defaults, seed only;
  configs recorded in drivers.py.

## Ledger impact (post-review)

- New **H-VAL** (supported): the bo-audit revisit counter is exact against
  analytic and scripted ground truth and detects the duplicate behavior
  documented in hyperopt #608 / optuna #5440-class issues. Caveats
  recorded: G4 is a transcription check; a −0.0 float-key false-negative
  was found by review and fixed (core.py) before any continuous-space use.
- No library-level claims enter the ledger from H0 (gate only). Review
  verdict REFUTED the first draft's flagship observation (an optuna-wins
  misreading contradicted by the table); corrected above.
