# E3 — Analysis

**Date:** 2026-08-22 · **Protocol:** as DESIGN.md. 750 runs (10 arms × 3
benchmarks × 25 paired seeds, budget 80), 4-way parallel. Raw:
`results.csv`. Two pre-registered hypotheses failed and two passed — the
failures are the finding.

**Independent review: REFUTED on interpretation, computations upheld**
(see `REVIEW.md`). The reviewer verified every number in this document
against results.csv with zero mismatches and upheld the code, but
correctly identified the first draft's "regime" reframing of H1's failure
as a confounded post-hoc narrative (distance-to-ceiling and search *type*
— omniscient argmax vs model-based EI — are perfectly confounded between
E2 and E3, and the reframing escalated past DESIGN.md's own pre-registered
fallback). This revision adopts the pre-registered fallback framing and
demotes the regime idea to a hypothesis with a designed de-confounding
experiment (next cycle).

## H1 FAILED (all 4 cells): the generator ceiling does not bind these surrogates

For GP-EI and RF-EI (combination dedup), keep vs flip is statistically
indistinguishable on both mixed benchmarks (wins 10–14/25, all p ≥ 0.09).
Compare the levels against E2's oracle and Random:

| func2C, mean final | | func3C, mean final | |
|---|---|---|---|
| oracle (ceiling) | −0.2063 | oracle | −0.7221 |
| GP keep+comb | −0.1049 | RF keep+comb | −0.2406 |
| RF keep+comb | −0.0847 | GP flip+comb | −0.2303 |
| TPE (own machinery) | −0.0581 | TPE | −0.2253 |
| Random | −0.0369 | GP keep+comb | −0.1772 |
| | | Random | −0.1780 |

The surrogates run **far below the machinery ceiling** (GP reaches half the
oracle's value on func2C; on func3C GP is at Random's level). In that
regime, which generator variant they get doesn't measurably matter: their
own guidance quality, not the pool, is the binding constraint.

**Interpretation (per DESIGN.md's pre-registered fallback):** the generator
ceiling effect is **oracle-demonstrated but not detected for these
model-based surrogates** at this budget — the audit's ceiling
*overestimates* machinery sensitivity for surrogates of this strength. Two
hypotheses fit this data and E3 **cannot distinguish them**, because
distance-to-ceiling and search type are confounded between E2 (omniscient
argmax, at the ceiling by construction) and E3 (model-based EI, far below
it):

- *H-regime:* generator restrictions bind any search process in proportion
  to how close it runs to the machinery ceiling; weak guidance masks the
  cap. (Circumstantially echoed by the thesis: fixing the generator did not
  change BASS's func2C standing — but that is the same confound, not a
  test.)
- *H-oracle-specific:* the generator effect is an artifact of
  omniscient/exhaustive scoring and does not operate on model-based EI
  surrogates at any strength.

The article's defensible claims from E2+E3 as they stand: the restricted
generator caps *attainable* performance (oracle, 25/25), and a null
machinery effect for a given surrogate cannot be distinguished from a
binding one without running the audit. Which of the two hypotheses holds is
now a designed experiment (next cycle): a **guidance dial** — the oracle's
acquisition corrupted with calibrated noise, `score = −f + σ·ε`, σ swept so
final performance spans ceiling-to-Random *within the identical pool-argmax
search type*. If keep-vs-flip sensitivity decays as σ grows, H-regime
survives de-confounded; if it persists at GP-level guidance, H-regime is
refuted and the oracle-specificity framing is final.

## H2 PASSED emphatically: the dedup leak is fatal for real surrogates

On cat_ackley d3/L5 (125 combos, optimum 0), with the keep generator:

| arm | mean final | solved (<0.1) | mean revisits/80 |
|---|---|---|---|
| GP comb-dedup | **0.000** | 25/25 | 0.0 |
| GP enc-dedup | 3.64 | 20/25 | 52.2 |
| RF comb-dedup | **0.000** | 25/25 | 0.0 |
| RF enc-dedup | 1.46 | 23/25 | 54.5 |
| TPE (own machinery) | 2.91 | 21/25 | 52.7 |
| Random | 4.36 | 19/25 | — |

Under encoding-level dedup, every pool surrogate re-spends **52–55 of 80
picks** on known combinations and degrades toward Random; with
combination-level dedup every surrogate solves every seed. Contrast with
E2: under the *oracle* the leak was invisible in final values (it re-picked
the optimum it had already found); under *real surrogates* it costs
solve-rate. Both facts belong in the article: invisible in curves at the
ceiling, fatal below it.

**Cross-method bonus:** stock optuna TPE — mature, widely deployed, its own
machinery — revisits a median 53/80 (66% of budget), landing at
Random-level performance on this benchmark. The ~2/3 budget-waste figure
the R diagnostics found for pre-fix BASS reappears for an entirely
different method family: the leak is a machinery property, not a BASS
quirk.

## H3 mixed (2/4), consistent with H1's null

Machinery cells barely move GP/RF's standing vs Random on mixed benchmarks
(func2C: directionally as predicted; func3C: reversed by 1–2 seeds). In the
guidance-limited regime, machinery doesn't move rankings either way — the
confound risk concentrates where surrogates are strong (or the benchmark
categorical/solvable, where H2's dedup axis dominates).

## H4 FAILED: TPE does not beat Random here (15/25, 12/25) — discrepancy flagged

The thesis's final run had TPE beating Random comfortably on these same
benchmarks. Differences that could explain the gap, none verified yet: this
TPE lacks the shared LHS init (optuna's own random startup), optuna 4.9
defaults may differ from the thesis's reticulate configuration (check
`code_files/R/tpe.R`), and pairing vs Random shares only the seed.
**Queued as an open investigation — no thesis-contradicting claim enters
the ledger from this arm.** The review settled the main cause candidate as
real: `tpe.R` seeds the thesis's optuna study with the *same shared
maximin-LHS initial design* (injected via `add_trial`) that all other
methods use, while E3's TPE starts from optuna's own random draw — a
material machinery difference. TPE's revisit measurement (above) is
*plausibly* init-insensitive (~72 post-startup trials dominate the count)
but that was not ablation-tested; treat the 53/80 figure as
startup-configuration-conditional until a shared-init TPE run confirms it.

## Threats to validity

- sklearn GP/RF are not the thesis's GPfit surrogate; on func3C this GP is
  Random-level, weaker than thesis GP-BO. H1's null does NOT establish that
  thesis-grade GP-BO would be generator-insensitive.
- All 750 GP fits share `random_state=0` (fit-optimizer seed) — uniform
  across arms so paired comparisons are unbiased, but it suppresses one
  source of GP-fit stochasticity; disclosed per review.
- TPE arm caveats above; treat as reference + revisit-counter only.
- RF flip+enc solving d3L5 25/25 (vs keep+enc 23/25) shows enc-dedup noise
  is high; solve-rate differences of ±2–3 seeds are within that noise.

## Ledger impact (post-review)

- K5 → **split verdict**: dedup axis generalizes with large effect (all
  pool surrogates + TPE's own machinery); generator axis: effect **not
  detected** for sklearn GP/RF at budget 80 (pre-registered fallback:
  the audit's ceiling overestimates machinery sensitivity for surrogates
  of this strength). Whether that is oracle-specificity or
  strength-dependence is undetermined — guidance-dial experiment queued.
- K2b → upgraded to cross-surrogate, cross-method, with practical cost
  (52–55/80 revisits; solve-rate 25/25 → 20–23/25; TPE 53/80, pending a
  shared-init confirmation for the TPE figure).
- K10 → **hypothesis only** (review: REFUTED as a supported claim —
  confounded with search type). Lives or dies on the guidance-dial
  experiment.
- H4 discrepancy → cause candidate confirmed real (shared-LHS init in
  `tpe.R` vs own startup here); config-matched re-run still queued.
