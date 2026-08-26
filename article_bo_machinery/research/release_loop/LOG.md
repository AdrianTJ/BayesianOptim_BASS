# Release loop log

Append-only; one entry per item, newest last; "Next:" is the
fresh-context resume instruction. Plan: `PLAN.md`. Claim ledger:
`../article_loop/CLAIMS.md` (this loop adds no claims — it changes
packaging, documentation and scope language only).

Discipline reminder for any session resuming here: a Sonnet 5 subagent
makes each edit, this session verifies independently by re-running, and
committed results / DESIGN files / paper numbers are read-only.

## Item R0 — 2026-08-26
- **Phase:** pre-registration — complete.
- **Did:** Wrote `PLAN.md` before any R1–R7 change, per the loop
  precedent that a plan is committed ahead of the work it governs.
  Recorded the pre-loop verification pass that motivates the seven
  items: every H1 headline number, the Z = 3 of 4 ranking change, and
  the figure medians re-derived with independent code and matching
  exactly; two apparent discrepancies traced to the auditor's own
  errors (wrong ranking population; a naive pigeonhole formula that
  collapses at K = 5^25) and kept on the record; bit-exact replay shown
  to fail while the reported statistics hold (median revisits 16,
  median unique 64 reproduced; 3 of 25 individual runs matching).
  Restarted the working branch from current `main` after the branch
  consolidation retired its predecessor.
- **Next:** Item R1 = the bo-audit README example. Delegate to a
  Sonnet 5 subagent: replace the "Use" snippet's mixed space (whose
  continuous `C` dimension makes exact revisits near-impossible) with a
  purely categorical space, run it, and paste the measured output.
  Out of scope for that agent: every other file, and any change to
  `core.py` counting logic. Verify by running the new snippet verbatim
  in a clean venv and diffing its stdout against the README's claimed
  output; only then commit and move to R2.

## Item R1 — 2026-08-26
- **Phase:** bo-audit README headline example — complete.
- **Did:** The "Use" example advertised `revisits: 16 / unique: 64` on a
  space carrying a continuous `C` dimension, where exact revisits are
  near-impossible; run verbatim it returned `0 / 80`. A Sonnet 5 subagent
  replaced it (README.md only, source untouched) with a purely
  categorical 5x5x5 = 125-combination space at budget 80 — the paper's
  cat-Ackley d3/L5 regime — plus a uniform-random control, with all
  documented output pasted from real measured stdout.
- **Verification refuted the first attempt (worker != verifier, working
  as intended).** The agent's first rewrite was mechanically correct —
  scope respected, output literally true and reproducible — but used a
  9-combination space at budget 80, reporting 72 revisits and
  `revisit_frac: 0.9`. The pigeonhole baseline there is 71.0, so the
  excess over unavoidable collisions was +1.0 (1.2% of budget), and
  uniform random on the same space also returns 71. That example would
  have advertised what reads as 90% waste on a space where this
  project's own metric says there is none, and where TPE is
  indistinguishable from random — a worse defect than the one being
  fixed, and precisely what a skeptical reviewer would compute first.
  Sent back with the requirement to use a regime with real headroom and
  an explicit instruction to report rather than tune the space if the
  excess came out near zero again.
- **Second attempt verified and accepted:** pigeonhole(125, 80) = 20.74
  against a measured 47 revisits (excess +26.3, 32.8% of budget) and a
  random control at 19 (excess -1.7). Every prose figure recomputed
  independently; the `(below)` cross-reference checked; scope confirmed
  as README-only; the 10-test suite still passes.
- **Gate committed:** `tools/verify_readme_example.py` extracts the
  README's own code block, runs it twice for determinism, diffs the
  documented comment against real stdout, fails when excess over
  pigeonhole is under 5% of budget, and fails when the sampler is not
  clearly above a random control. Validated against the known-bad
  version first — it passed that version's truthfulness check and
  failed its substance check, which is the behavior required. A gate
  that cannot fail is worthless.
- **Next:** Item R2 = the missing LICENSE. `README.md:179` carries a
  markdown link pointing at `LICENSE`, `bo-audit/README.md` says MIT and
  `bo-audit/pyproject.toml` sets `license = "MIT"`, but no LICENSE file
  exists anywhere in the tree, so collaborators are granted nothing.
  Delegate to a Sonnet 5 subagent: add a canonical MIT LICENSE at the
  repository root, copyright Adrian Tame Jacobo, year 2026. Out of
  scope for that agent: every other file, and any change to the license
  *identifier* in pyproject (it is already correct). Verify the text
  against the canonical MIT wording word for word, confirm the README
  link resolves to a real path, and confirm the SPDX identifier agrees;
  then commit and move to R3.

## Item R2 — 2026-08-26
- **Phase:** MIT LICENSE — complete.
- **Did:** The repository claimed MIT in three places and granted
  nothing: the root README's license link had no target,
  `bo-audit/README.md` said "MIT.", and `bo-audit/pyproject.toml` set
  `license = "MIT"`. A Sonnet 5 subagent added the canonical MIT text at
  the repository root, copyright 2026 Adrian Tame Jacobo (matching the
  `authors` field in pyproject), then added a byte-identical copy at
  `bo-audit/LICENSE`.
- **Verification, part 1 — text provenance.** The agent wrote the
  licence text *from memory* and said so. It happened to be correct, but
  memory is not an acceptable source for canonical legal text, so it was
  checked rather than believed: the canonical wording was fetched from
  the SPDX license-list-data repository and diffed word for word against
  the file, ignoring only SPDX's placeholder copyright line. Exact
  match. The agent was told to cite an authority rather than recall in
  future.
- **Verification, part 2 — a gap the brief had missed.** Building the
  wheel showed it declared `License-Expression: MIT` while containing no
  licence text at all: the Python project root is `bo-audit/`, not the
  repository root, and PEP 639 resolves licence-file patterns relative
  to the project root. The defect had simply relocated to the artifact a
  colleague would actually `pip install`. Hence the package-local copy.
- **Second round verified independently:** a fresh build of the wheel by
  this session (not the agent's) yields
  `bo_audit-0.1.0.dist-info/licenses/LICENSE`, METADATA carrying both
  `License-Expression: MIT` and `License-File: LICENSE`, and the licence
  inside the wheel byte-identical to the repository root's
  (sha256 a6b8c737...). Setuptools' PEP 639 defaults picked the file up
  on their own, so `pyproject.toml` was correctly left untouched — the
  conditional edit in the brief was not needed and was not made.
- **Note:** both LICENSE files are identical by `cmp` and by md5
  (7bee2f9f...). Duplicating the licence per distributable package is
  standard for repositories shipping a package from a subdirectory.
- **Next:** Item R3 = make the article PDF buildable from a clean clone.
  `article_bo_machinery/figures/fig_dedup_audit.pdf` is required by
  `main.tex` line 684 but is caught by the blanket `*.pdf` rule in
  `.gitignore`, whose allowlist does not include it, so a fresh clone
  following `article_bo_machinery/README.md`'s build instructions fails
  on a missing figure. The generator
  (`figures/make_fig_dedup_audit.py`) works and reproduces the caption's
  medians (0 vs 78/80) from committed E2 results. Delegate to a Sonnet 5
  subagent: add a negative-pattern exception for that one path in
  `.gitignore` so the built figure is tracked, and extend the article
  README's Build section to name the figure-generation step. Out of
  scope for that agent: the generator script itself, `main.tex`, and any
  other ignore rule. Verify by simulating a clean clone
  (`git archive`/fresh checkout into a temp dir) and confirming the
  figure is present, and by re-running the generator to confirm the
  medians still print 0 and 78. No LaTeX exists in this container, so
  the actual `pdflatex` run stays an author task and must be named as
  such rather than claimed.

## Item R3 — 2026-08-26
- **Phase:** article PDF buildable from a clean clone — complete.
- **Did:** `main.tex:684` includes `figures/fig_dedup_audit`, but the
  built figure was swallowed by the blanket `*.pdf` rule in the root
  `.gitignore` (rule at line 36) and absent from the allowlist beneath
  it, so a fresh clone following the article README's Build section
  failed on a missing figure. A Sonnet 5 subagent added one negative
  pattern for that single path inside the existing "Keep specific PDF
  assets and main outputs" block, regenerated the figure from the
  committed E2 results, and documented the generation step in the
  README's Build section.
- **Verification:** the diff is exactly one added `.gitignore` line plus
  one README paragraph — no other ignore rule, no `main.tex` change, no
  edit to the generator. The figure was regenerated independently by
  this session and printed `medians: {'combination': 0.0, 'encoding':
  78.0}`, matching the caption's claim of 78/80 under encoding dedup
  versus 0 under combination dedup. Output is a valid 1-page PDF whose
  basename matches what `main.tex` requests.
- **A verification error worth recording, since the loop records its
  own:** the first trackability check called `git check-ignore -v` and
  read its exit status as the verdict. That is wrong — `check-ignore`
  exits 0 whenever *any* rule matches, including a negation, so it
  reported "still ignored" for a file that is not ignored. Re-tested
  decisively with `git add --dry-run` (offers to add), `git status`
  (lists it as `??` untracked rather than ignored), and by reading the
  matched rule itself (line 43, `!`-prefixed). The fix was correct; the
  check was not.
  A second harness bug followed in the same item: a clean-clone check
  tested `ls "$f".pdf "$f".png "$f"`, which fails whenever *any* listed
  candidate is absent, and so reported the figure missing while the
  directory listing directly above it showed the file present. Both
  errors were in the checking code, not the change under test; both are
  recorded because a verification pass that hides its own mistakes is
  not one.
- **Clean-clone simulation (the actual acceptance test):** cloned the
  committed branch into a fresh directory. The figure is present at
  19,246 bytes, `references.bib` resolves, every `\includegraphics`
  target resolves, and running the generator *from inside the clone*
  reprints `medians: {'combination': 0.0, 'encoding': 78.0}`. Both
  LICENSE files from R2 are present in the clone as well.
- **Author task, not claimed as done:** no TeX distribution exists in
  this container, so `pdflatex` was never run here. This item makes the
  figure present and the step documented; the actual compile remains an
  author-side confirmation.
- **Next:** Item R4 = record the H6 sweep's real status.
  `research/headline_uplift/exph6_sweep/` holds a pre-registered
  16,575-run DESIGN and 10,203 committed rows with no ANALYSIS.md, GP
  arms essentially unrun, and a pre-registered hypothesis (GH5) that
  fails on the real-ML class. A reader currently finds a large tree of
  unanalyzed data that narrows the paper's headline, with nothing
  saying so. Delegate to a Sonnet 5 subagent: write a STATUS.md in that
  folder following this tree's LOG/PLAN conventions — what ran, what did
  not, which hypotheses the partial data can and cannot address, and an
  explicit statement that no paper claim currently rests on it. Out of
  scope for that agent: `results.jsonl`, `DESIGN.md`, `analyze_g.py`,
  `main.tex`, and any claim in `CLAIMS.md`. Every number in the note
  must be re-derived by this session from `results.jsonl` before the
  commit stands — do not let the agent copy figures out of this log.

## Item R4 — 2026-08-26
- **Phase:** H6 G-sweep status record — complete.
- **Did:** `exph6_sweep/` held a pre-registered DESIGN, 10,203 committed
  rows, no ANALYSIS.md, GP arms unrun and a failing pre-registered
  hypothesis, with nothing in the tree saying so. A Sonnet 5 subagent
  wrote `STATUS.md` there: coverage table, cell-level shortfall, the
  concrete finish-list, per-clause analysis of which GH hypotheses the
  partial data can and cannot address, a plain statement of the GH5
  failure, and a provenance section. `results.jsonl`, `DESIGN.md` and
  the analysis scripts were not touched.
- **Verification — ground truth derived before reading the agent's
  answer,** so the comparison was independent rather than a
  plausibility check. Re-derived from `results.jsonl` with separate
  code: 10,203 rows; per-arm 2,300/2,300/2,000/2,300/1,300 and 1/1/1
  for the GP arms; planned cells 92/92/80/92/64/77/77/77 totalling 651;
  smac 51 full, 2 partial, 11 missing; fast-arm 10,200 of 10,500
  (97.1%); GP family 3 of 5,775; overall 10,203 of 16,275 (62.7%); zero
  duplicate `(arm, benchmark, budget, seed)` keys; GH3's 66 contributing
  cells decomposing as 23 + 20 + 23 with optuna-gp contributing none.
  Every figure in STATUS.md matched.
- **A stale total in the frozen pre-registration, found and correctly
  handled.** DESIGN.md line 58 states "Total 16,575 runs", but that
  arithmetic predates its own Amendment 1, which excluded the 3 YAHPO
  benchmarks from optuna-tpe-3.6 (3 x 4 budgets x 25 seeds = 300 runs).
  The post-amendment total is 16,275. The agent found this independently
  and used the corrected denominator. DESIGN stays unedited — a
  pre-registration is frozen even where later known to be imperfect —
  and STATUS.md carries the reconciliation as a bookkeeping note.
- **Two precision defects found in review and corrected by this session
  rather than by another agent round-trip, both mechanical:** (1) the
  block labelled "reproduced verbatim" had had its `**` bold markers
  stripped, so a reader diffing it against `analyze_g.py`'s real stdout
  would have found a mismatch — restored byte-for-byte from the script's
  own output and re-checked equal; (2) the smoke-cell citation read
  "Amendment 1's Procedure step 2", conflating two separate DESIGN
  sections — Procedure step 2 names the five smoke cells, Amendment 1
  reports their timings. In a document whose entire value is provenance,
  claiming verbatim and then differing is the defect that matters most.
- **Independently confirmed the "no paper claim rests on this" statement**
  before letting it stand: grepped `main.tex` for the sweep's benchmark
  names, GH labels and run totals. The only hit was the word
  "contaminate" on line 181, a false positive from an over-loose pattern
  of mine. The paper's revisit/pigeonhole language belongs to the
  completed H1 matrix, which has its own results file.
- **Also verified against DESIGN's text:** the ">5/25 seeds is reported
  incomplete" bar STATUS.md invokes is real (DESIGN line 62), and the
  three GP runs are exactly the three GP-arm entries among Procedure
  step 2's five pre-registered smoke cells.
- **Next:** Item R5 = write the GH5 scope caveat into the article. This
  is the first item that edits `main.tex`, so the discipline tightens:
  the caveat must be scoped to what the partial sweep licenses — the
  four complete TPE-family arms — and must not imply the GP arms
  contributed, since they ran 3 of 5,775 cells. State plainly that a
  pre-registered hypothesis failed on the real-ML class and what that
  bounds. Delegate to a Sonnet 5 subagent with `main.tex` as the only
  writable file, Discussion section only, no new citations, no number
  that is not already in `STATUS.md`. Verify: `check_article.py` PASS
  with the TODO ratchet not rising, every quoted figure re-derived from
  `results.jsonl` by this session, and the surrounding Limitations prose
  re-read for contradiction with the new text.

## Item R5 — 2026-08-26
- **Phase:** GH5 scope caveat in the article — complete.
- **The judgement call that shaped this item.** The obvious way to write
  this caveat was to cite the G-sweep's failed GH5 directly. That was
  refused. This project's standing rule is that a result enters the
  paper only after worker-verifier adversarial review; the sweep is 62.7%
  complete and its analysis has had no review, so its numbers are
  inadmissible in the text however suggestive they are. The brief
  therefore forbade any reference to the sweep, its folder, or the GH
  labels, and required the caveat to rest only on evidence the paper
  already reports.
- **What made that possible:** the paper already contains the needed
  evidence at line 455 — 349 of 350 mixed-space runs registering zero
  exact revisits — but presents it purely as a null check confirming the
  counter does not overfire. The scope implication was never drawn. So
  the caveat states an implication of published, reviewed data rather
  than importing an unreviewed result.
- **Why the boundary is the honest reading, not a retreat.** Re-derived
  from `results.jsonl` before briefing: of the seven real-ML benchmarks,
  the single one with a finite (float-free) space shows median e(80) =
  +0.153 for optuna-tpe, in line with the synthetic categorical classes,
  while the six float-bearing ones record **zero** revisits in total
  across every arm and seed. The pre-registered hypothesis failed
  because six of its seven benchmarks cannot exhibit exact revisits by
  construction, not because the effect stops at real problems. The
  paper's finding is bounded by *space type*, and saying so sharpens it.
- **Did:** A Sonnet 5 subagent added three sentences to the "Scope of the
  machinery family" paragraph: the waste is a property of finite
  categorical combination spaces; continuous coordinates make exact
  revisits near-impossible by construction, as the audit's own
  349-of-350 figure shows; a practitioner on a continuous-heavy space
  should not expect this failure mode.
- **Verification:** diff confined to that one paragraph; `check_article`
  PASS with the TODO ratchet unmoved at 4/4; braces balanced (400/400);
  no undefined references; no number introduced that the paper did not
  already carry; grep confirms no sweep reference of any kind reached
  `main.tex`.
- **Three copy defects corrected by this session** rather than by
  another round-trip, all mechanical: the block was appended after the
  paragraph's closing conjecture about the *generator* restriction,
  stranding a *dedup*-scope note among generator material — relocated to
  sit directly after the ecosystem-audit sentence it qualifies, leaving
  the conjecture as the closer; "show exactly zero exact revisits"
  doubled the word "exact" — now "register none at all"; and a second
  `Sec.~\ref{sec:wild}` two sentences after the first was dropped as
  redundant.
- **Next:** Item R6 = close the environment-reproducibility gap.
  Measured earlier this session: re-running pest_control x optuna-tpe x
  25 seeds from the released package reproduces the reported statistics
  exactly (median revisits 16, median unique 64) while only 3 of 25
  individual runs replay, because the raw results record library
  versions but not their transitive dependencies, and optuna's TPE
  stream depends on numpy. Delegate to a Sonnet 5 subagent: generate a
  real environment freeze from a working environment (never
  hand-written) and add one honest sentence to Data Availability saying
  aggregates reproduce while per-seed runs are environment-sensitive.
  Out of scope: any results file, any DESIGN, and any other part of
  `main.tex`. Verify the freeze was machine-generated by checking it
  against the live environment, and re-run `check_article` with the
  ratchet held.

## Item R6 — 2026-08-26
- **Phase:** environment-reproducibility gap — complete.
- **The gap, as measured rather than assumed:** re-running
  pest_control x optuna-tpe x 25 seeds from the released package
  reproduced the reported statistics exactly (median revisits 16, median
  unique 64) while only 3 of 25 individual runs matched the committed
  per-seed rows. The objective is deterministic and unchanged since the
  original run, the runner path is identical, and optuna is the same
  4.9.0 — the divergence is transitive dependencies, since the raw
  results record each library's own version but not the numpy/scipy its
  sampler draws from.
- **Did:** A Sonnet 5 subagent produced `reference_env.txt` (machine
  generated by `pip freeze`, never typed) and added two clauses to the
  paper's Data Availability section recording that aggregates reproduce
  while per-seed replay does not.
- **The failure mode this item was most exposed to was a false
  provenance claim** — presenting today's environment as the one that
  produced the published results. The brief forbade it explicitly and
  the header delivers: a "WHAT THIS FILE IS NOT" block stating that the
  original environment's transitive versions were never recorded and
  cannot be reconstructed, that the numpy 2.4.6 shown is merely what
  resolved today, and that no numpy/scipy version is claimed for the
  original run. It also states its own coverage limit unprompted — only
  the optuna driver is installed, so it covers one arm of the audit,
  not five.
- **Verification:** the freeze was diffed against a live `pip freeze`
  captured by this session *before* the agent ran, to catch hand-editing
  — 24 packages, byte-identical, no version string altered. (The agent's
  report said "23 lines"; that was a miscount in its prose, not an error
  in the file.) `check_article` PASS with the ratchet held at 4/4,
  braces balanced, no undefined references.
- **One precision defect corrected by this session:** the paper text
  read "individual per-seed runs did not replay" when 3 of 25 in fact
  did. This paper is careful enough elsewhere to write "349 of 350"
  rather than "all", so the sentence now reads "largely did not replay
  (3 of 25 matched)". The count is a disclosure about a verification run
  rather than a scientific result, and quoting it exactly makes the
  claim checkable instead of merely directional.
- **Next:** Item R7 = repair the bo-audit package gaps, the last item and
  the one carrying the most risk, because it moves code that produced
  published numbers. Four defects: (a) `bench_by_name` falls through to
  `from machinery import ...`, so 5 of the 6 H1 benchmarks (cat_ackley
  x3, func2C, func3C) are unreachable from an installed package;
  (b) `smac_runner.py` was never copied into the package, so
  `run_smac_subprocess` resolves a path that does not exist;
  (c) `MemoizedAuditedObjective.__init__` drops the `canonicalize`
  argument, blocking conditional-space use of the budget-equalized
  control; (d) the README's test path says `bo_audit/tests` where the
  tests live at `tests/`. Any code move must be verbatim — the rule is
  that behaviour may not change — and the acceptance test is that a
  reproduced paper cell still matches at the median from a clean venv,
  not that the file merely imports.

## Item R7a — 2026-08-26
- **Phase:** vendor the H1 benchmarks into the package — complete. (R7
  is split in two: this is the part that moves code which produced
  published numbers; the three smaller package defects follow as R7b.)
- **Did:** `bench_by_name` reached cat_ackley and func2C/func3C only via
  `from machinery import ...`, a research-tree module absent from the
  distribution, so 5 of the paper's 6 audit benchmarks were unreachable
  from an installed package. A Sonnet 5 subagent extracted the minimal
  closure — `Schema`, `decode_levels`, `_rosen`, `_camel`, `_beale`,
  `_apply_fn`, `make_func2C`, `make_func3C`, `make_cat_ackley`,
  `OBJECTIVES` — verbatim into `bo_audit/benchmarks_h1.py`, leaving the
  BO harness (`run_bo`, `hybrid_candidates`, `oracle_method`, ...) out.
  `machinery.py` itself was not modified.
- **Behaviour preservation was the binding constraint,** so the wiring
  tries `machinery` first and falls back to the vendored copy only on
  ImportError. Every published run executed `machinery`; preferring it
  means reproducing a published result still runs the identical code,
  while the fallback makes the installed package self-sufficient.
- **Verification, and the reason it is trustworthy: reference values
  were captured from the original `machinery` module BEFORE the vendored
  copy existed.** SHA-256 over the objective outputs on fixed grids, all
  five benchmarks, compared afterwards against the vendored copy: bit
  identical in every case. An independent AST comparison (source segment
  per definition, not a visual diff) confirms all 10 definitions match
  their source exactly, with no extra top-level definitions smuggled in.
- **The fingerprint itself had to be fixed before it was worth
  anything.** Its first version fed raw level indices to objectives that
  take unit-cube rows (level v encodes to (v-0.5)/L), which saturates:
  all 40 cat_ackley points returned one identical value, so the check
  would have passed no matter what the copy did. Rebuilt with the
  correct encoding it spans 9-60 distinct values with real spread. Third
  time this loop that the checking code, not the change under test, was
  the broken part.
- **The parity guard was mutation-tested rather than assumed.** Two
  deliberate perturbations of the vendored copy — `100` to `100.0000001`
  in `_rosen`, and the cat_ackley grid bound `32.768` to `32.7681` —
  were each caught, and by the right tests: func2C/func3C for the first,
  the three cat_ackley sizes for the second. The file was then restored
  and confirmed byte-identical to its pre-mutation state, with the AST
  check re-run and the suite green. A guard that cannot fail would have
  been worse than no guard, because it would look like protection.
- **Clean-venv acceptance:** installed from a built wheel into a fresh
  environment with `machinery` confirmed unimportable; all six
  benchmarks resolve *and evaluate* (pest_control 25 dims, cat_ackley
  3/5/6, func2C 4, func3C 5). Suite: 16 tests pass, including the new
  parity file, which skips cleanly when the research tree is absent.
- **Next:** Item R7b = the three remaining package defects, none of
  which touch published numbers. (a) `smac_runner.py` exists only in
  `research/headline_uplift/bo_audit/`, so `run_smac_subprocess`
  resolves `Path(__file__).parent / "smac_runner.py"` to a path that
  does not exist in an installed package — copy it verbatim.
  (b) `MemoizedAuditedObjective.__init__` calls
  `super().__init__(fn, space, cont_decimals)` and silently drops the
  `canonicalize` argument, so the budget-equalized control cannot be
  used on conditional spaces — the exact case the hook was built for;
  add the passthrough and a test that fails without it. (c) the README
  states the test path as `bo_audit/tests` where the tests live at
  `tests/`. Verify by clean-venv install, full suite, and confirming
  the new memo test fails when the passthrough is reverted.
