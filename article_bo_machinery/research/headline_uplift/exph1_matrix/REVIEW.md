# H1 — Adversarial review record

**Reviewer:** independent verifier (worker ≠ verifier), instructed to
refute; recomputed everything from results.jsonl with its own code,
including a 60-digit mpmath check of the pigeonhole baselines and a
git-history check of what was committed before the data.

## Verdict

**UPHELD-WITH-CORRECTIONS.** All 42 cells' statistics, hypothesis
evaluations P1–P6, the headline X/N arithmetic (3/6 ≥10%, 2/6 ≥25%,
1/6 ≥40%), F2's optuna-gp distributions, F3's random-zero and F4's
smac-zero counts, and the row accounting (1050 rows, 42/42 cells at
25/25, no duplicate keys, unique+revisits==evals on every row)
reproduced digit-for-digit. One flagship prose claim was falsified and
has been corrected.

## Findings and dispositions

1. **[MAJOR — falsified prose]** "P7 … PASS (0 exact revisits
   everywhere)" and F5's "every library shows 0" were contradicted by the
   data AND by the document's own embedded table (skopt-gp func3C mean
   0.04): seed 3019 has 1 revisit (unique 79/80); 349/350 mixed runs are
   exactly 0. P7's *verdict* stands — DESIGN's wording is "≈0" and the
   median≤1 operationalization was in the pre-data commit — but the prose
   had upgraded it to an absolute. → Reworded in both places; the lone
   true positive is now also cited as incidental evidence the mixed-space
   float key is live code (see 4).
2. **[MINOR]** F1's "dedup group ≈0 excess in every cell" conflated
   ax/smac (0 raw revisits, strongly negative excess) with skopt-gp
   (excess ≈0). → Split.
3. **[MINOR]** "failures.log entries: 170" counted traceback lines, not
   failed attempts (23 FAILs + 2 smoke timeouts, all superseded by valid
   re-runs). → analyze_h1.py footer now counts marker lines (25).
4. **[NOTE — validation gap]** H0's G2 never injected a known duplicate
   on a *mixed* space, so P7 alone could not distinguish "no waste" from
   "metric dead on mixed keys". The skopt-gp seed-3019 revisit supplies
   incidental true-positive evidence; a scripted mixed-space gate is
   queued for H1b. → Added to Threats.
5. **[NOTE — process integrity CONFIRMED]** The post-data pigeonhole
   expm1/log1p fix matches 60-digit ground truth at every K and moves the
   analysis *toward* DESIGN's registered baselines (which match the fixed
   values, not the buggy ones, and predate the data); under the buggy
   baseline P1 would have mis-evaluated FAIL via pest excess 16−80=−64.
   Fix direction verified neutral, not convenient.
6. **[NOTE — verified]** Amendment 2's SMAC narrative reproduced exactly:
   8 crashed seeds identified; on re-run 6 early-terminate at 59–75/80
   and 2 complete; all 150 smac rows have 0 revisits. "Would rather stop
   than duplicate" tightened to the mechanism-level phrasing.

## What enters the ledger

H1-WASTE, H1-MASK, H1-REFUSE (upheld as measured), and H1-NULLMIX (in
its corrected 349/350 form). Wording follows the corrected ANALYSIS.md.
