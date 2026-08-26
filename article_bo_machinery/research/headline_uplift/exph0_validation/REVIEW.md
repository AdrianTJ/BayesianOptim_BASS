# H0 — Adversarial review record

**Reviewer:** independent verifier pass (worker ≠ verifier), full access to
DESIGN.md, run_h0.py, results.json, h0_stdout.log, bo_audit source, and the
first-draft ANALYSIS.md.

## Verdict

**REFUTED (first-draft analysis) / UPHELD (computations and gate verdict).**
The run itself, the G/D gate evaluations, and the "H0 GATE: OPEN" verdict
were verified correct against DESIGN.md's pre-registered criteria. But the
first draft's flagship observation — "optuna-tpe … wins the benchmark (best
mean final)" — was directly falsified by the document's own results table:
skopt-gp has mean best 0.000 with 10/10 seeds at the exact optimum, versus
optuna-tpe's 1.82 with 9/10. Under the loop's process rule (a refuted
flagship claim refutes the analysis document), ANALYSIS.md was rewritten
before anything fed the ledger.

## Findings and dispositions

1. **[MAJOR — refuted observation]** "optuna wins the benchmark" contradicted
   the table. → Observations section rewritten: the corrected reading is
   that the one dedup-equipped pipeline (skopt-gp) wins on *both* axes
   (solve rate and excess waste), which is the paper-relevant story.
   Rewrite verified against results.json.
2. **[MINOR — arithmetic]** hyperopt row said "+14 above" pigeonhole;
   34.0 − 20.7 ≈ 13.3 and 35 − 20.7 ≈ 14.3; the table reports the
   mean-consistent "+13". → Fixed.
3. **[MINOR — code, latent]** `key_of` float branch could emit `-0.0` and
   `0.0` as distinct keys for numerically equal rounded values (false
   negative on continuous spaces). Not exercised by H0 (pure-categorical
   D-gates; G2 never hits −0.0). → Fixed in core.py (`+ 0.0`
   normalization) before any continuous-space use in H1.
4. **[MINOR — disclosure]** hyperopt driver's `non_defaults` said
   "seed only" but also passes a fresh `Trials()` and disables the
   progress bar. Both cosmetic, but the fairness rule requires listing
   them. → drivers.py string corrected.
5. **[MINOR — honesty]** G4 "independent recount" re-runs the same
   algorithm on the same call log — a transcription check, not an
   independent semantic check. → Caveat added to the gates table;
   G1's hand-computed case is the semantic validation.
6. **[RECONCILIATION]** skopt fired 597 duplicate warnings (~59.7/run)
   yet shows only 18.3 revisits/run. Source-checked: on a duplicate ask,
   skopt substitutes an unfiltered `space.rvs()` uniform draw, which can
   itself re-collide. ~70% of fallback draws land new, ~30% re-collide —
   consistent, no contradiction. Recorded as measured Observation 3.

## What survives into the ledger

Only the instrumentation-validation claim (**H-VAL**, supported with
caveats 3 and 5 recorded). No library-level claim enters from H0 —
n=10 seeds, one benchmark, gate-only by design.
