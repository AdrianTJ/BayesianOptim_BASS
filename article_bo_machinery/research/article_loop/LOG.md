# Article loop log

Append-only; one entry per cycle, newest last; "Next:" is the fresh-context
resume instruction.

## Cycle 0 — 2026-08-21
- **Phase:** setup / re-center
- **Did:** Restructured per user direction: experimentation is now the core
  of the loop (research → decide → experiment → analyze → re-center →
  repeat), writing deferred to the terminal phase (writing plan retained
  unchanged). Imported main's content needed as ground truth
  (final_results/, updated thesis TeX, diagnostics, NLP HPO, R library) as
  files — history merge with rewritten main is classifier-blocked and
  queued as a user decision. Installed the Python experiment stack (numpy,
  scipy, scikit-learn, optuna, pandas). Wrote the experiment program
  (E1–E6) and the initial claim ledger (K1–K8; K3-old marked contradicted).
- **Files:** PLAN.md, CLAIMS.md, LOG.md, ../README.md, imported content
- **Tests:** check_research.py PASS
- **Next:** Cycle 1 = E1 (harness validation), per PLAN.md experiment
  program: DESIGN.md first, Python machinery harness vs the R reference,
  reproduce the pre-fix oracle numbers, ANALYSIS.md + independent review +
  check_experiments.py, then re-center the ledger.

## Cycle 1 — 2026-08-21
- **Phase:** experiment (E1 harness validation) + analyze + re-center
- **Did:** Built the Python machinery harness (`experiments/machinery.py`)
  from the R reference; pre-registered DESIGN.md with 6 checks; ran 135
  runs (3 benchmarks × 15 seeds × 3 arms, budget 80) in 67 s. 5/6 checks
  PASS including the load-bearing 15/15 paired keep-vs-flip wins on both
  mixed benchmarks and exact-optimum hits. V2 FAIL led to a real find: the
  recorded R figure "−0.148 at budget 10" is unreproducible from the
  committed R code (tail-probability argument + exact historical generator
  test) — quarantined as K2a-fig; the article must not quote it pending an
  R re-run. Exact optima computed and analytically proven in review:
  func2C −0.206326 at (2,2), func3C −0.722140 at (2,2,1). Independent
  Sonnet review UPHELD the analysis and caught two code defects (unstable
  hash-based RNG seeding; V6 min/max) — both fixed, experiment re-run
  deterministically. Ledger updated: K1, K6 supported with measured cost;
  K2a supported with magnitude revised down at n_cand=1000 (pool-size
  dependence queued for E2); Kopt added; K2a-fig quarantined.
- **Files:** experiments/machinery.py, exp01_harness_validation/{DESIGN,
  ANALYSIS,REVIEW}.md + run_e1.py + results.csv, tools/check_experiments.py,
  CLAIMS.md
- **Tests:** check_research.py PASS; check_experiments.py PASS; E1 V-checks
  5/6 PASS (V2 = the quarantined reference number, documented)
- **User decisions (open):** title/co-authors/venue (unchanged); branch
  history reconciliation with rewritten main (unchanged).
- **Next:** Cycle 2 = E2 (oracle-ceiling matrix at final protocol): DESIGN
  first — 2 generator variants × 2 dedup levels (add encoding-level dedup
  to the harness as a loop option), 25 seeds (1001–1025), budget 80,
  benchmarks func2C/func3C/cat_ackley d3L5+d6L11, plus the pool-size axis
  n_cand ∈ {50, 200, 1000} on func2C/func3C to test K2a's magnitude
  dependence. Reuse validated machinery.py; independent review before
  ledger update.

## Cycle 2 — 2026-08-21
- **Phase:** experiment (E2 oracle matrix) + analyze + re-center; plus the
  R-side resolution of K2a-fig
- **Did:** E2 ran 700 runs in 5m04s: all five pre-registered hypotheses
  PASS. Headlines: keep beats flip 25/25 in every cell (Wilcoxon floor
  p=6.0e-8); the ceiling gap grows ×8–14 as the pool shrinks 1000→50 while
  keep stays at the optimum (K2a upgraded — severity is pool-size
  dependent, worst where pools are realistic); encoding-level dedup wastes
  a median 78/80 picks on the solvable categorical benchmark with no
  visible movement in any final value (K2b mechanism at protocol scale).
  Installed R in-container (CRAN egress-blocked; lhs via apt; BASS/GPfit
  unavailable → E4 BASS cells stay queued for the user's machine) and ran
  the historical generator through the actual R library: R gives
  −0.1971@10 at n_cand=1000 — K2a-fig **refuted as recorded** (−0.148 sits
  in small-pool territory); R and Python agree to ~2 decimals everywhere,
  cross-language-validating the harness. Independent review UPHELD E2 with
  five minor findings, all incorporated (REVIEW.md). Ledger: K2a upgraded,
  K2b protocol-scale, K2a-fig refuted-as-recorded, K6 strengthened.
- **Files:** exp02_oracle_matrix/{DESIGN,ANALYSIS,REVIEW}.md, run_e2.py,
  results.csv, r_check/{k2afig_check.R,r_results.csv}, machinery.py
  (dedup= + revisits), CLAIMS.md
- **Tests:** check_research.py PASS; check_experiments.py PASS; E2 H1–H5
  all PASS
- **User decisions (open):** unchanged (title/co-authors/venue; branch
  history).
- **Next:** Cycle 3 = E3 (surrogate × machinery matrix): DESIGN first —
  surrogates {GP-relaxation (sklearn GPR + closed-form EI), RF (sklearn,
  SMAC-style EI over trees' variance), TPE (optuna)} through the SAME
  run_bo loop and generator/dedup cells, vs Random, paired 25 seeds,
  budget 80, benchmarks func2C/func3C/cat_ackley d3L5; fold a benchmark
  index into the RNG seed formula (E2 review finding 1). Measures whether
  machinery choices move rankings for surrogate families beyond BASS/GP
  (K5) and the surrogate-level revisit cost (E5 piggybacks: count revisits
  per surrogate under encoding dedup on d3L5). Independent review before
  ledger update.

## Cycle 3 — 2026-08-22
- **Phase:** experiment (E3 surrogate matrix) + analyze + review-forced
  re-center
- **Did:** E3 ran 750 runs (relaunched once after a container restart
  killed the first run; pre-registration was already committed). Two
  pre-registered hypotheses FAILED, two PASSED. H2 passed emphatically:
  encoding-level dedup costs every real pool surrogate 52–55/80 revisits
  and drops d3L5 solve-rate from 25/25 to 20–23/25, and stock optuna TPE's
  own machinery wastes 53/80 — the dedup leak is cross-method (K2b
  upgraded). H1 failed in all four cells: keep-vs-flip makes no
  significant difference for sklearn GP/RF (p≥0.09). The adversarial
  reviewer UPHELD every computation (zero numeric mismatches) but
  **REFUTED the first-draft "regime" reframing** of the H1 null as
  confounded HARKing; the analysis was rewritten to the pre-registered
  fallback (audit ceiling overestimates machinery sensitivity for weak
  surrogates), K10 demoted to hypothesis, and the de-confounding
  experiment designed: a guidance dial (noisy oracle, score = −f + σ·ε,
  same pool-argmax search type, σ swept ceiling→Random). H4 also failed
  (TPE ≯ Random); review confirmed the cause candidate is real (thesis
  tpe.R shares the LHS init via add_trial; ours didn't) — config-matched
  re-run queued. Minor code fixes: numeric param-key sort in combo_of; GP
  random_state disclosure.
- **Files:** exp03_surrogate_matrix/{DESIGN,ANALYSIS,REVIEW}.md, run_e3.py,
  results.csv, surrogates.py, CLAIMS.md
- **Tests:** check_research.py PASS; check_experiments.py PASS; E3 checks:
  H2 PASS ×2, H3 2/4, H1 FAIL ×4 + H4 FAIL ×2 (pre-registered
  falsifications, recorded as findings)
- **User decisions (open):** unchanged.
- **Next:** Cycle 4 = E7 (guidance dial, decides K10): DESIGN first —
  noisy-oracle acquisition score = −f(Xc) + σ·ε with σ calibrated per
  benchmark so mean final spans ceiling→~Random across σ levels (pilot σ
  grid on a few seeds first, then pre-register the σ set); keep vs flip,
  combination dedup, func2C + func3C, 25 paired seeds, budget 80,
  n_cand 1000 and 50 (small pool = where E2 says the gap is largest —
  strongest test). Pre-register: H-regime supported iff keep-vs-flip
  paired win-rate/gap decays monotonically toward chance as σ grows;
  refuted if it persists at σ where performance ≈ GP/Random level. Also
  (cheap, same cycle if time permits): shared-init TPE re-run on
  func2C/3C to close K-TPE. Independent review before ledger update.
