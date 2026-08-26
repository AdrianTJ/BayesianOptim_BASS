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
