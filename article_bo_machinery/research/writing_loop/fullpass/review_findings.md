# Phase-2 full-pass findings (round 1)

Three lenses; fix pass follows once all three report. Status legend:
open / fixed / deferred-to-user.

## Lens C — hostile referee (7 majors, 10 minors; verdict: major revision)

| # | Sev | Finding (compressed) | Status |
|---|---|---|---|
| C1 | MAJOR | Generator-confound practical relevance rests on the noise-dial proxy; no real surrogate showed the effect — say so plainly in Sec V/Discussion | fixed (Discussion answers the objection head-on) |
| C2 | MINOR | Abstract: generator result lacks the "undetected on real surrogates" clause | fixed (abstract clause added) |
| C3 | MINOR | Discussion never poses/answers "does the generator confound matter for real methods" directly | fixed (same Discussion passage) |
| C4 | MAJOR | Machinery ablations are synthetic-only; no explicit external-validity limitation | fixed (Sec VIII sentence + Discussion scope limit) |
| C5 | MAJOR | Real tasks named but no results reported — report (numbers.md has them) or drop | fixed (nlp_hpo W/T/L reported in Sec VIII; elastic net correctly re-scoped per B1) |
| C6 | MINOR | Scope paragraph silent on dimensionality/real-task generalization | fixed (dimensionality/real-task sentence in scope paragraph) |
| C7 | MAJOR | BASS is rhetorical center but never audited through the ablations — reframe as motivating case study | fixed (intro reframes BASS as case study; mechanisms credited to oracle+other families) |
| C8 | MAJOR | "Unattributable before repair" generator claim for BASS rests on reconstructed history — narrow to dedup or state actual pre-repair config with evidence | fixed (pre-repair forced-flip config cited to version history + contemporaneous A/B; audit re-establishes independently) |
| C9 | MAJOR | Protocol item 3 undersells: ceiling distance necessary but not sufficient (dial cost) | fixed (item 3: necessary-but-not-sufficient + dial cost) |
| C10 | MINOR | Replication specifics scattered (MC draws, solved threshold) | fixed (MC-draws row added to tab:disclosure) |
| C11 | MINOR | Canonicalization generalization (one-hot) unaddressed | fixed (one-hot generalization sentence) |
| C12 | MINOR | "Failure Mode II" naming asymmetry (no "Failure Mode I" header) | fixed (Sec IV retitled '...and Failure Mode I'; Sec V line-broken) |
| C13 | MAJOR | Zero figures in a paper about curve-invisibility — add ≥1 figure | fixed (fig:dedup added: superimposed curves vs 0-78 revisit audit; gap-vs-pool figure remains optional TODO) |
| C15 | MINOR | No consolidated reproducibility paragraph | fixed (reproducibility paragraph in Data Availability) |
| C16 | MINOR | Generic self-disclosure sentence — name the two refuted analyses or delete | fixed (both refuted analyses named with their corrections) |
| C18 | MINOR | No multiple-comparisons note | fixed (multiple-comparisons note in Discussion) |
| C19 | MINOR | tab:dedup missing Random+encoding cell (or a why-omitted note) | fixed (caption explains the omitted Random+encoding cell) |

## Lens A — consistency/numbers: pending
## Lens B — thesis consistency (1 blocker, 1 minor)

| # | Sev | Finding (compressed) | Status |
|---|---|---|---|
| B1 | BLOCKER | Article claims the thesis "ran two real tuning tasks"; the thesis explicitly DROPPED elastic net as uninformative (models converged immediately, Experiment.tex:318) and reports ONE real task (Conclusions.tex:15). Fix Sec VIII (and any echo) to: one reported real task (neural text classifier); elastic-net candidate dropped, runs exist in companion repo only | fixed (Sec VIII: ONE reported real task; elastic net stated as dropped-by-thesis with artifacts in repo) |
| B2 | MINOR | "Near-uninformative on the hardest categorical instance" rests on a stale PRE-FIX diagnostic (Spearman ~0.47, near-intercept, never re-measured post-fix) and the thesis itself claims only "moderate predictive correlation" on mixed — soften/mark pre-fix | fixed in ROUND 2 (round-1 'fix' was a silent str.replace no-op, caught by the round-2 verifier; now applied via verified edit) |
| B3-8 | OK | tab:thesis exact match; flip narrative matches README expectation-vs-outcome; 25-29/40 fairly quoted; machinery description consistent; "separate article" promise delivered; tone compatible | — |


## Lens A — consistency/numbers (0 blockers, 12 minors)
All numbers verified against canonical sources with zero conflicts. Minors
A1 (capped-every-method phrasing), A2 (exact-optimum precision), A3
(dedup terminology tie in tab:disclosure), A4 (keep/flip stray), A5
(Cat-Ackley forward pointer), A6 (=C12), A7 (dead sec:protocol label), A8
(abstract mantissa), A12 (families count) — ALL FIXED this round. A9/A10
(rounding artifact note; unused bib entry) — accepted as-is (harmless).
A11 (=B2) fixed.

## Round-1 fix pass: complete. Round-2 verification pending.


## Round 2 (verification): ISSUES REMAIN -> both fixed
- B2: round-1 fix had silently failed to apply (unverified str.replace);
  ledger status was therefore false. Corrected: provenance + near-intercept
  wording now verifiably in main.tex (grep-confirmed). Process lesson
  recorded: apply prose fixes via the verifying Edit tool, and never mark a
  ledger row fixed without a grep of the result.
- NEW blocker: "under an hour of CPU time" (added in round 1 for C15) was
  false by the verifier's direct timing (~74 min aggregate, E3-dominated).
  Replaced with the measured figure ("about 75 minutes of aggregate CPU
  time... oracle audits account for minutes").
- All other round-1 fixes verified genuine, including bit-identical
  superimposed curves in fig:dedup and full ref/label/environment
  integrity.

## Round 3: targeted verification of the two round-2 fixes -> pending

## Round 3: fixed-verified (B2) + partial (CPU phrase) -> the specified
one-word remedy applied ("the oracle audit itself accounts for about five
minutes", scoped to E2 per the paper's established singular usage) and
grep-verified. **CLEAN PASS declared: Phase 2 complete.**
