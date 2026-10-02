# G-sweep analysis (H6)

Status: **unreviewed.** DESIGN step 5 requires a worker≠verifier
adversarial review before anything here reaches CLAIMS.md or the paper.

Inputs: `results.jsonl`, 16,275 rows, which is exactly the planned
matrix after Amendment 1 (no missing, extra or duplicate keys; 0 failed
runs, 0 cap-outs). The fast arms ran in the original container; the GP
wave (optuna-gp, skopt-gp, ax), the 300-run smac backfill and the last
pest_control/B160 smac seed ran on a second machine (`env_sandcastle.txt`,
Amendment 2). All numbers below come from the committed `analyze_g.py`,
unchanged since before wave 1; its full output is in `g_agg.md`.

## Letter evaluation

| ID | verdict | evidence |
|---|---|---|
| GH1 | **PASS** | optuna-tpe and hyperopt-tpe both exceed e(80) > 0.05 in classes A–D (4/5); class E is 0.000 for both |
| GH2 | **FAIL** | ax and smac: median revisits 0 in every covered cell at every budget. skopt-gp: one cell outside \|e\| ≤ 0.07, cat_ackley_d3_L5/B20 at e = −0.072 |
| GH3 | **PASS** | ρ(B, e) ≥ 0 in 86/89 = 97% of no-dedup cells |
| GH4 | **PASS** | optuna-tpe-3.6 passes classes A–D (class E over ml_* only, Amendment 1) |
| GH5 | **FAIL** | best no-dedup arm reaches e(80) ≥ 0.05 on 1 of 7 class-E benchmarks (needed 4) |
| GH6 | descriptive | median Kendall τ(B20, B160): B +0.32, C +0.16, D −0.20, F +0.10; A and E undefined (tied rankings) |
| GH7 | **PASS** | random has median 0 revisits on every float-bearing space |

### Reading the two failures

**GH2** fails by 0.002, and in the direction the clause did not
anticipate. skopt-gp's median revisits on that cell is 0 (25 runs:
fifteen 0s, eight 1s, two 2s); the pigeonhole baseline at K = 125, B = 20
is 1.44, so e = −0.072. The arm wastes *less* than chance, which is what
a deduplicating sampler does on a small space. The clause bounded |e| in
both directions and the negative side was not thought through when it
was registered. It stays FAILED; the correction belongs in how the
article states skopt's behaviour ("never above chance"), not in the
verdict.

**GH5** fails cleanly, and this is the result with the most bearing on
the paper. On the seven real-ML benchmarks the TPE-family samplers show
essentially no excess waste: only ml_rf_digits, the one class-E space
with no float dimension (K = 4,256), crosses 0.05. Revisit waste in
these libraries is a property of finite, all-discrete spaces. It does
not carry over to typical ML search spaces with continuous
hyperparameters, and the article should not suggest that it does.

## Descriptive findings worth review

1. **optuna-gp wastes heavily on finite spaces and still solves.**
   e(80) per class: A +0.40, B +0.43; on catf_michal_d5L9 e(160) = +0.71.
   It solves 25/25 on six of the seven class A–B benchmarks with solve
   thresholds (catf_rosen_d4L7: 9/25). On classes C and D its waste is
   small (+0.012, +0.006). This repeats the claims-audit finding B6 at sweep scale:
   its duplicates come once it has found the optimum, so waste and solve
   record coexist. (Solve times are not recorded in these rows; the
   ordering is inferred from B6's traces, not checked here.)
2. **optuna-gp repeats exact float configurations on YAHPO rpart.** On
   yahpo_rpart_40981/B80 all 25 runs revisit (median 36 of 80); on
   yahpo_rpart_41138/B160 21/25 runs (median 9, max 119); ml_gb_bc/B80
   18/25 runs (median 1). These are float-bearing spaces where chance
   revisits are ~0, so these are organic true positives in GH7's sense.
   The mechanism (for example proposals pinned to box bounds) is not
   checked.
3. **TPE waste grows with budget** (GH3), reaching e(160) ≈ 0.45–0.61 on
   classes B–D for optuna-tpe. Version 3.6 and 4.9 are indistinguishable
   (GH4), so this is not a 4.x regression.
4. **Ax with unordered categoricals solves cat_ackley_d5_L5 in 16/25 runs
   here** (seeds 4001–4025) against 10/25 in H7 (seeds 3001–3025), same
   version and settings. A 6-solve swing between seed sets of 25 is a
   reminder of the claims audit's A1: single-seed-set solve counts of
   this size are noisy.
5. **The deduplicating arms (ax, smac, skopt-gp) never exceed chance
   anywhere**, so GH2's substance holds for all three.

## Cross-machine caveat

Objective values were checked identical across the two machines, but
some arms follow different optimizer paths for the same seed on
different hardware (examples in `env_sandcastle.txt`). No cell mixes
machines except smac contam_2p25/B20 (1 original seed, 24 new) and
smac pest_control/B160 (24 original, 1 new). Comparisons between arms
that ran on different machines carry this as unquantified noise; the
waste metric is a property of each run, so the GH verdicts do not depend
on it.

## What this changes in the article (after review)

- The waste results generalize across all 14 benchmarks in classes
  A–D (optuna-tpe e(80) between +0.20 and +0.47 on each) and two Optuna major versions (GH1, GH3, GH4).
- They do **not** generalize to real ML spaces with continuous
  dimensions (GH5). Any sentence that implies otherwise is cut.
- optuna-gp's float-space repeats (finding 2) are new and would need
  their own check before being cited.
