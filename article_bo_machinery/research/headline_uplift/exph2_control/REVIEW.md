# H2 — Adversarial review record

**Reviewer:** independent verifier (worker ≠ verifier), instructed to
refute; recomputed all cells from raw JSONL, re-ran the G5 gate itself,
and checked the paired weak-improvement property on all 275 individual
runs (beyond the pre-committed script's per-cell check).

## Verdict

**REFUTED (first-draft analysis) / data pipeline UPHELD.** The headline
"Z = 2 of 4" was wrong: the pre-committed analyze_h2.py implemented only
the median-comparison half of DESIGN's registered ranking metric,
silently dropping its solve-count tie-break clause. Under the registered
definition, cat_ackley_d3_L5 also changes ranking (7 pairwise flips via
solve counts: optuna-tpe 21→25/25 exits the bottom tier) and d5_L5's
"5 flips" collapse to the single genuine ax↔optuna-tpe reversal (the
other 4 were tie-handling artifacts). **Corrected Z = 3 of 4** — derived
two independent ways by the reviewer, then reproduced by the fixed script
(Amendment 2). Row accounting, all medians/solve counts, saturation
numbers, Q2/Q3 arithmetic, and the path-bug remediation all verified
clean.

## Findings and dispositions

1. **[MAJOR]** ranking_pairs() omitted the registered solve-count
   tie-break → Z under-counted (2 vs 3), d3 mislabeled "unchanged", d5
   flip list inflated. → analyze_h2.py fixed toward the registered
   definition (Amendment 2, logged in the code and ANALYSIS); ANALYSIS
   rewritten; h2_agg.md regenerated and now matches the reviewer's
   hand-derived flip sets exactly.
2. **[MAJOR]** DESIGN's "carried rows are duplicate-free" premise is
   false for skopt-gp on d3_L5 (median 20/80 H1 revisits → ~60 unique
   evals in the carried column) and was undisclosed. ax/smac (100 rows)
   and optuna-gp pest (25 rows) verified 0-revisit row-by-row — those
   carryovers are exact. → Disclosure added to Threats; outcome
   unaffected (skopt-gp already at 25/25 with ~60 unique).
3. **[MINOR]** "pulling level with the GP tier" contradicted the
   adjacent 22/25-vs-25/25 parenthetical and the registered metric
   (optuna-gp/skopt-gp still rank above optuna-tpe in H2). → Reworded to
   "closing most of the gap … though not all of it".
4. **[MINOR]** "~80% a machinery artifact" was not derivable from any
   stated calculation. → Dropped; the solve counts (7/25 → 22/25) speak
   for themselves.
5. **[NOTE — strengthens]** Weak-improvement verified per-row: all 275
   paired runs satisfy H2 best ≤ H1 best + 1e-12; zero violations.
   Trajectory-superset claim itself is unverifiable from stored
   summaries (stated in Threats); its checkable consequences hold on all
   rows.
6. **[NOTE — verified]** optuna-gp saturation (30/400 d3, 55/400 d5,
   ~85 asks for 80 unique on d6), G5 gate (re-run: PASS 4/4, 6/6), Q2
   sub-clause accounting (7/8), 275-row integrity
   (unique+revisits==proposals on every row), and the 3 pre-fix
   path-bug failures all reproduce exactly.

## What enters the ledger

H2-ZOFW (Z=3/4 in its review-corrected form), H2-SAT, H2-REFUND (with
Q2's composite FAIL on record). Wording per the corrected ANALYSIS.md.
