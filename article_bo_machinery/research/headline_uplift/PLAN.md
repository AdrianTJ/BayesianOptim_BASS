# Headline uplift loop — from solid methods paper to venue headline

**Question this plan answers:** what must change for the machinery-confound
article to be a *headline* contribution (NeurIPS main / D&B, ICML) rather
than a solid AutoML/TMLR/workshop paper.

**Research basis (2026-08-22, three fanned-out sweeps; sources in the
loop's notes when cycles run):**

1. **Success anatomy.** Every headline paper in this genre audited systems
   the authors did not build — 204 published classifiers (Recht et al.,
   ICML'19 oral), 65 independently submitted optimizers (Turner et al.),
   7 third-party solvers (MCBO, NeurIPS'23 D&B), dozens of papers' actual
   protocols (Musgrave et al., ECCV'20) — released an artifact the
   community kept using (ImageNetV2, bayesmark, HPOBench, MCBO,
   powerful-benchmarker), and compressed to one striking
   "changes-the-conclusions" number. The cautionary twin: Choi et al.'s
   optimizer study (own infrastructure, no artifact) never made a main
   proceedings despite heavy citation, and Tripp & Hernández-Lobato — our
   nearest neighbor — stayed workshop-tier. Our current draft has exactly
   their profile. The D&B rubric additionally *mandates* released code at
   submission.
2. **Feasibility of auditing the wild.** Six widely-deployed optimizers are
   pip-installable, CPU-only, and drivable through ask/tell or a wrapped
   objective in this container: Optuna (TPE + GPSampler), Hyperopt, SMAC3,
   scikit-optimize (archived but still mass-installed), Ax/BoTorch, and
   HEBO — plus FLAML (local-search contrast), Nevergrad (non-Bayesian
   control), and bayes_opt (no-categorical-support negative control).
   Four have *public GitHub issues conceding duplicate-suggestion
   behavior* (Optuna #5440/#2021/#4859/#5058; Hyperopt #608 documents
   48/500 identical trials) — free ground truth to validate our
   instrumentation before auditing libraries with no paper trail. Realistic
   higher-dimensional mixed benchmarks that run CPU-only: **YAHPO Gym**
   (mixed/conditional ML-pipeline spaces, 700+ problems, surrogate-fast),
   **JAHS-Bench-201** (14-dim mixed architecture+HP space), and the
   vendorable **COMBO pest-control** function (5^25 combinatorial, ideal
   for revisit counting).
3. **Theory upgrades within reach.** (a) *Proposition C* — the guidance-dial
   decay formalized via Gumbel/softmax selection probability (Plackett–Luce
   top-1), closed form, monotone in noise: a few lines of standard
   extreme-value theory that turns our empirical decay curve into a fitted
   law. (b) *Lemma A* — the oracle ceiling as a genuine theorem for
   exogenous (non-incumbent-conditioned) pool sequences (one-paragraph
   proof), with the adaptive case stated as an explicit Lipschitz-assumption
   conjecture (*Proposition B*) supported by our data — honest, and it
   upgrades "diagnostic yardstick" to "theorem in the special case".
   (c) Citation anchors for the regret-decomposition framing: Kim et al.
   (arXiv:2506.11831, inexact-acquisition regret) and B3O
   (arXiv:2606.30228, softmax-selection regret). All flagged: full texts
   were snippet-verified only; read before writing appendix math.

## The headline thesis (what the upgraded paper claims)

> *We audited N widely-deployed Bayesian-optimization libraries on mixed
> and categorical search spaces with a surrogate-free oracle-ceiling audit
> and combination-level revisit instrumentation. X of N silently re-spend
> ≥Y% of the evaluation budget on already-evaluated configurations, and
> machinery-controlled re-comparison changes the apparent method ranking
> on Z of W benchmark tasks. We release `bo-audit`, a tool that runs this
> audit against any ask/tell optimizer in minutes.*

(N, X, Y, Z, W are the loop's job to fill; the claim shape is fixed now so
overclaiming can't creep in later. Framing is structural — mechanisms and
defaults, never "library L is wrong"; findings validated against each
library's own issue tracker where one exists.)

## Phases

| # | Phase | Deliverable | Runs here? |
|---|---|---|---|
| H0 | **`bo-audit` tool + instrumentation validation.** A small pip-structured package (`bo_audit/`): wraps any ask/tell optimizer + objective; emits revisit counts (decoded-combination level), the machinery-disclosure table, and the oracle ceiling for the optimizer's own proposal stream where drivable. Validate the counter against the *admitted* ground truth first: reproduce Hyperopt #608-class behavior and Optuna #5440-class behavior before trusting any novel reading. | tool + validation report | yes |
| H1 | **The in-the-wild audit matrix.** 6–9 libraries × {CoCaBO Func-2C/3C, Cat-Ackley 3 sizes, pest control, 2–3 YAHPO rbv2 tasks, JAHS-Bench} × ≥25 seeds. Measured: revisit waste, solve rates, and (where the library exposes its proposal stream) machinery ceilings. Pre-registered per-library hypotheses from each library's documented mechanism. | the headline table + X/Y numbers | yes |
| H2 | **Machinery-controlled re-comparison.** Re-run the library comparison with the controls imposed (external combination-level dedup wrapper; shared init where the API allows): does the apparent ranking change? This produces Z of W — the "changes conclusions" number. | Z/W result | yes |
| H3 | **Theory appendix.** Proposition C (Gumbel/softmax decay law, fitted to E7/E8 data), Lemma A (exogenous-pool theorem), Proposition B (adaptive-case conjecture + empirical support), regret-decomposition framing with verified citations. Full-text verification of the two anchor papers is a hard gate. | appendix + fitted curve figure | yes |
| H4 | **Close the open cells.** E4/BASS on the author's machine (queued commands exist); one strong-surrogate-near-ceiling attempt (more GP restarts / larger budget on an easy mixed instance) to try to observe the generator effect on a real surrogate. | closed cells or honest nulls | partly (E4 = author) |
| H5 | **Rewrite the paper around the new spine.** In-the-wild results become Section 2's motivation and the Experiments core; the current pipeline study becomes the controlled mechanism section; abstract/title re-cut to the headline claim shape above. Full writing-loop discipline (single writer, per-section review, checks, full-pass). | submission-ready v2 | yes |

## Loop mechanics

Same discipline as the article loop, unchanged: pre-registered DESIGN.md
before every run, worker ≠ verifier on every analysis and section,
claim-ledger re-centering each cycle (new claims H1…Hn live in
`CLAIMS.md` alongside K1–K10), deterministic checks, stall counter, no
silent scope cuts, everything committed per cycle. The existing
`experiments/machinery.py` harness, checkers, and `numbers.md` pipeline
carry over.

## Risks, stated up front

- **Install/data friction:** Ax/BoTorch pulls torch (~CPU wheels fine,
  disk to watch); YAHPO/JAHS need surrogate-data downloads (GitHub-hosted,
  likely reachable; egress is the H0 smoke test). Any library that won't
  install gets logged and dropped loudly, never silently.
- **Wrapper fairness:** each library must be driven per its own docs
  (defaults, not straw men); wrapper configs are part of the disclosure
  table and reviewed adversarially.
- **The wild may be clean:** if the audit finds little waste in modern
  libraries, that is a publishable calibration result but not a headline;
  the loop reports it honestly and the paper stays at its current tier.
  Hyperopt #608 and Optuna #5440 make a total null unlikely, but the
  possibility is named now, before any data.
- **Compute:** library loops are slower than our oracle harness
  (surrogate fits per step). Budget: CPU-days spread over self-paced
  cycles; per-cell smoke tests before full fan-outs.

## Stop conditions

H1–H3 complete with the headline numbers filled (or honestly nulled), H5
review-clean → report to author with the venue call. Stall counter and
user-stop as always.
