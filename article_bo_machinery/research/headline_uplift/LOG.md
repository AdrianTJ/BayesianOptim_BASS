# Headline uplift loop log

Append-only; one entry per cycle, newest last; "Next:" is the fresh-context
resume instruction. Claim ledger: new H-claims live in
../article_loop/CLAIMS.md alongside K1-K10.

## Cycle 0 — 2026-08-22
- **Phase:** H0 (instrumentation validation gate) — complete.
- **Did:** Built `bo_audit/` (AuditedObjective wrapper + drivers for
  optuna-tpe, optuna-gp, hyperopt-tpe, skopt-gp, random). Pre-registered
  DESIGN.md (G1–G4 correctness gates, D1–D4 detection gates), then ran
  run_h0.py: **H0 GATE OPEN** — counter exact vs analytic (G3 36.0 vs
  36.0; D4 20.0 vs 20.7), zero continuous false positives, and it detects
  admitted ecosystem behavior at large magnitude (optuna-tpe median 46/80
  revisits = 2.2× pigeonhole; hyperopt-tpe 35/80; skopt-gp 18.3 ≈
  pigeonhole with 597 fired dedup warnings). Adversarial review REFUTED
  the first-draft analysis (optuna-wins misreading contradicted by its own
  table — skopt-gp wins 0.000/10-10); ANALYSIS.md rewritten, core.py −0.0
  key normalization fixed, drivers.py disclosure corrected, REVIEW.md
  records all findings. Ledger: **H-VAL** added (supported, with caveats).
- **Environment notes:** SMAC3 import-broken (old build resolves private
  sklearn symbol) — pinned-version retry queued for H1, not dropped.
  Ax/BoTorch installed OK; optuna-gp driver written but deferred by design.
- **Next:** Cycle 1 = **H1** (in-the-wild audit matrix). Fresh-context
  resume: read `PLAN.md` phase H1 + `exph0_validation/{ANALYSIS,REVIEW}.md`.
  Steps: (1) retry SMAC3 with a version pin, smoke-test optuna-gp and an
  Ax/BoTorch driver; (2) install/smoke-test YAHPO Gym and JAHS-Bench-201,
  vendored COMBO pest-control fallback if egress blocks data files;
  (3) write pre-registered H1 DESIGN.md — libraries × benchmarks × ≥25
  seeds, headline metric = excess-over-pigeonhole (per H0 Observation 2),
  per-library hypotheses named before running; (4) run, analyze,
  worker≠verifier review, ledger, log, commit, push.

## Cycle 1 — 2026-08-21
- **Phase:** H1 setup + pre-registration — complete; timing smoke in flight.
- **Did:** Tooling: SMAC 2.4.0 isolated in a pinned venv (requires
  sklearn ≥1.6.1 yet imports a symbol our 1.9 removed; venv has 1.7.2) and
  driven via subprocess with in-subprocess bo-audit counting
  (`bo_audit/smac_runner.py`) — smoke-verified on categorical and mixed
  benchmarks (0 revisits at 20 trials, consistent with its runhistory
  mechanism). Ax driver written on the modern `ax.Client` API
  (smoke: 9 s/15 trials, 0 revisits); optuna-gp smoke showed **1 revisit in
  15 trials** (P6 motivation). Benchmarks: COMBO pest control vendored
  into `bo_audit/benchmarks.py` (pure numpy, determinized with a local
  seeded RNG — disclosed; original draws MC scenarios from the global RNG
  per call), determinism + value-range validated; shared `bench_by_name`
  adapter so main-env and smac-venv runs use identical benchmark code.
  **Logged drops:** JAHS-Bench-201 (requires Python <3.11), HEBO (GPy
  1.9.9 build failure in main env AND a py3.10 venv), YAHPO deferred to
  exploratory H1b (conditional spaces need active-parameter key semantics;
  rbv2_svm data downloaded, surrogate evaluates, SMAC×YAHPO impossible:
  ConfigSpace ≥1.0 vs 0.6.1). Pre-registered `exph1_matrix/DESIGN.md`
  (7 libraries × 6 benchmarks × 25 seeds, budget 80, seeds 3001–3025,
  excess-over-pigeonhole metric, hypotheses P1–P7, 20-min per-run cap,
  never-silent drop accounting) and committed it BEFORE any full run;
  `analyze_h1.py` (fixed aggregation + by-the-letter P1–P7 evaluation)
  also committed before data. Timing smoke (slow libs × 6 benchmarks,
  budget 80) launched in background.
- **Next:** Cycle 2 = H1 run + analysis. On smoke completion: check
  failures.log and wall times against the 20-min cap (ax on pest_control
  is the flagged risk), adjust nothing silently — any cap-driven cell drop
  goes in DESIGN deviations; launch `run_h1.py full` in background
  (resumable JSONL); when the matrix is complete run `analyze_h1.py`,
  write ANALYSIS.md, adversarial worker≠verifier REVIEW.md, ledger update
  (H1 claims only via review), LOG, commit, push. YAHPO/H1b only after
  the core matrix is banked.

## Cycle 2 — 2026-08-21
- **Phase:** H1 run + analysis — complete. **The headline exists.**
- **Did:** Full matrix ran in ~3.3 h wall (13.2 core-hours): 1050 runs,
  42/42 cells at 25/25 (8 smac crashes recovered via Amendment 2
  prefix-reporting; 25 failed attempts total, all superseded). All seven
  pre-registered hypotheses PASS by the letter. Headline numbers:
  **3/6 libraries re-spend ≥10% of budget** on already-evaluated
  combinations beyond pigeonhole, 2/6 ≥25%, 1/6 ≥40% — the max being
  optuna-gp's median 53/80 revisits (~27 unique configs) on Cat-Ackley-125
  *while posting the matrix's best solve record* (H1-MASK). optuna-tpe
  revisits 16/80 on the 5^25 pest space where random collides never
  (H1-WASTE); smac raises rather than duplicates (H1-REFUSE); mixed-space
  metric null holds 349/350 (H1-NULLMIX). Two disclosed post-data fixes:
  analyze_h1.py pigeonhole float-cancellation repaired TOWARD the
  registered baselines (verifier confirmed with 60-digit mpmath + git
  history: neutral, not convenient; buggy baseline would have flipped P1
  to FAIL), and smac_runner now reports crashed-run prefixes. Adversarial
  review: **UPHELD-WITH-CORRECTIONS** — everything reproduced
  digit-for-digit except one falsified prose absolute ("0 revisits
  everywhere" vs the actual 349/350), corrected everywhere along with
  F1/F4 wording and the failures.log footnote. Ledger: H1-WASTE, H1-MASK,
  H1-REFUSE, H1-NULLMIX added; H-VAL caveat extended (mixed-space
  injection gate queued for H1b).
- **Next:** Cycle 3 = **H2** (machinery-controlled re-comparison → the
  Z-of-W "changes conclusions" number). Fresh-context resume: read
  exph1_matrix/{ANALYSIS,REVIEW}.md first. Steps: (1) pre-register H2
  DESIGN.md — impose an external combination-level dedup wrapper (reject
  duplicate proposals, re-ask; where a library exposes no re-ask, document
  the fallback per library) and re-run the waste-heavy libraries
  (optuna-tpe, optuna-gp, hyperopt-tpe) plus controls on the benchmarks
  where waste was large (d3_L5, d5_L5, d6_L11, pest), 25 seeds; Z-of-W =
  on how many benchmark tasks does the library ranking (by median best /
  solve rate) change once dedup is equalized; (2) also pre-register the
  scripted mixed-space injection gate promised in H1 REVIEW (H1b item);
  (3) run smoke → full, analyze with pre-committed script, adversarial
  review, ledger, LOG, commit, push. H1b YAHPO conditional-space cells
  remain queued behind H2.

## Cycle 3 — 2026-08-21
- **Phase:** H2 machinery-controlled re-comparison — complete.
- **Did:** Pre-registered the memoization control (bit-identical
  trajectories on deterministic objectives; budget = 80 unique evals,
  400-ask horizon), G5 mixed-space injection gate PASSED (closing H1
  REVIEW's promised gap), smoke caught a third instance of the
  parents[2] path-bug class, full 275-run matrix completed with zero
  failures. Adversarial review **REFUTED the first draft**: the
  pre-committed analyze_h2.py omitted DESIGN's own solve-count tie-break,
  under-counting the headline (Z=2) and inflating d5's flip list.
  Amendment 2 fixed the metric toward its registered definition:
  **Z = 3 of 4 benchmarks change ranking** once budgets are equalized —
  optuna-tpe's apparent small-space weakness is mostly machinery (d3
  21→25/25 solves; d5 16.18/7-solves → exact optimum/22-solves), TPE
  order inverts on d6. Also: GPSampler generator saturation (30 unique in
  400 asks on K=125 — duplicates are its steady state), heterogeneous
  refund (hyperopt's strict clause FAILED: freeing its waste doesn't help
  it — on record), skopt-gp carried-row disclosure added (review
  MAJOR-3). Ledger: H2-ZOFW, H2-SAT, H2-REFUND added; H1-NULLMIX updated
  with G5 pass. The paper's three headline numbers now exist: X/N =
  3/6 libraries ≥10% waste; the audit tool detects it invisibly to
  curves; Z/W = 3/4 rankings change when machinery is equalized.
- **Next:** Cycle 4 = **H3** (theory appendix). Fresh-context resume:
  read PLAN.md phase H3 + article_loop CLAIMS.md K10/E7-E8 rows. Steps:
  (1) attempt full-text fetch of the two anchor papers (Kim et al.
  arXiv:2506.11831; B3O arXiv:2606.30228) via WebFetch — arxiv egress was
  blocked for raw downloads earlier; if still blocked, write the
  self-contained math and mark anchors snippet-verified-only, deferred to
  author (hard gate per PLAN: no appendix citation without full-text
  check — the *framing* can proceed, the citations wait); (2) Lemma A:
  oracle ceiling as a theorem for exogenous pool sequences (one-paragraph
  proof, states the adaptive case as Proposition B conjecture with E2/E3
  empirical support); (3) Proposition C: Gumbel/softmax top-1 selection
  probability, closed form, monotone in noise scale — fit to E7/E8
  guidance-dial data (K10 row); deterministic check: the fitted curve
  must reproduce E8's non-monotone residual observation or the fit's
  limits are disclosed; (4) worker≠verifier review of ALL math (a
  Sonnet 5 prover-skeptic pass), ledger, LOG, commit, push. H1b (YAHPO
  conditional spaces) and H4 (BASS cells, author machine) remain queued;
  H5 rewrite starts only after H3 lands.

## Cycle 4 — 2026-08-21
- **Phase:** H3 theory appendix — complete.
- **Did:** Anchor gate resolved honestly: arXiv still egress-blocked →
  appendix fully self-contained, Kim/B3O anchors queued as author
  verify-before-citing tasks, zero dependent claims. THEORY.md: Lemma A
  (oracle domination on exogenous pools — now a THEOREM with a coupling
  proof, and the exact boundary where it stops being one), Prop B
  (adaptive case as an explicit conjecture + counterexample showing the
  global-support assumption is necessary), Prop C (Gaussian selection
  decay proved monotone with pinned limits; Gumbel/softmax closed-form
  twin). Pre-registered fit protocol ran: the decay curve fitted on E8
  transfers to held-out E7 on func3C (R² 0.66–0.88, 4/4 cells) and
  FAILS to transfer on func2C (range compression; one held-out R² =
  −1.28) — reported as a split, func2C parameters used nowhere.
  Prover-skeptic review: **UPHELD-WITH-CORRECTIONS** — all proofs valid
  (reviewer supplied the Gumbel logit proof and independently re-fit the
  degenerate γ≈18 cell), one MAJOR citational error caught and fixed
  (Prop B's evidence had mis-attributed surrogate arms to surrogate-free
  E2; corrected to the reviewer-verified 0/2250 matched E3-vs-E2
  record), five minors applied (unique-minimum premise, post-dedup-mask
  pools, a.s.-idealization disclosure, raw-f-units for s, inlined
  counterexample). Ledger: H3-LEMA, H3-PROPC added.
- **Next:** Cycle 5 = **H5** (rewrite the paper around the new spine) —
  H4's BASS cells are author-machine work (queued in the author to-do
  list) and H1b (YAHPO conditional spaces) stays optional behind the
  rewrite. Fresh-context resume: read article_loop/CLAIMS.md H-rows,
  exph1_matrix/ANALYSIS.md, exph2_control/ANALYSIS.md,
  h3_theory/THEORY.md, and the current main.tex + writing_loop conventions
  (research/writing_loop/WRITING_PLAN.md, numbers.md pipeline,
  tools/check_article.py). Steps: (1) pre-register a REWRITE_PLAN.md
  (section map old→new: in-the-wild audit becomes the motivation +
  experiments core; the controlled BASS study becomes the mechanism
  section; abstract/title re-cut to the audited-ecosystem claim shape
  with the three headline numbers 3/6-waste, invisible-in-curves,
  Z=3/4; theory appendix from H3; every number sourced from the
  extract_numbers pipeline extended to H1/H2/H3 outputs); (2) rewrite
  section-by-section with per-section adversarial review (writing-loop
  discipline: single writer, verifying Edit tool, TODO ratchet in
  check_article.py); (3) full-pass review, checkers, ledger re-center,
  LOG, commit, push. Author to-dos to compile at H5 end: pdflatex
  compile, anchor full-text checks, E4/BASS runs, venue call.
