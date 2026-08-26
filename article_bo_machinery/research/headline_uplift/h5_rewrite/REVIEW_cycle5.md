# H5 Cycle 5 — section review record (abstract + Introduction)

**Verdict: UPHELD-WITH-CORRECTIONS** (worker≠verifier; every number traced
to h_numbers.md/CLAIMS.md; forbidden refuted phrasings grep-confirmed
absent; LaTeX brace/math balance checked).

## Findings and dispositions

1. **[MAJOR]** "best solve record in the entire audit" was false —
   skopt-gp out-solves optuna-gp on both mixed benchmarks (func2C 7/25 vs
   1/25; func3C 1/25 vs 0/25); the ledger scopes the claim to the
   categorical matrix. → All three occurrences changed to "the matrix's
   best categorical solve record".
2. **[MAJOR]** "several of the behaviors are acknowledged in the
   libraries' own issue trackers" — only the two TPE implementations'
   duplicate behavior has tracker citations; GPSampler waste and SMAC's
   termination have no paper trail. → Narrowed to the TPE-specific
   statement, with "behaviors with no paper trail at all" called out as
   the audit's added value.
3. **[MAJOR]** "six libraries … 1,050 runs" fails the reader's
   multiplication (6×6×25=900); the random control is the 7th arm. →
   Both occurrences now say "six libraries and a random-sampling
   control … 1,050 runs across the seven arms".
4. **[MINOR]** "bit-identical" softened to "by construction leaves …
   bit-identical" (the reviewed record verifies the checkable
   consequences, the identity itself is analytic).
5. **[MINOR]** Abstract ~356 words (slightly over AIP norm) — deferred
   to the Cycle 8 full-pass trim.
6. **[NOTE]** Verified clean: all headline numbers, "no access to library
   internals", released-tool consistency with Data Availability, style
   conventions, cite-key resolution.

## Stale-contradiction list for Cycle 7 (from the reviewer)

- Data Availability "about 75 minutes of aggregate CPU" predates H1/H2
  (~14+ core-hours) — MUST be updated in Cycle 7.
- Experiments section run counts (700/750/2450) lack H1 1050 / H2 275 /
  H0 gates — Cycle 7.
- Related Work + Setup backfill — Cycle 6 as planned; no existing text
  contradicts the new front matter (grep-checked, no stale "three
  contributions" references anywhere).

---

# H5 Cycle 6 — section review record (Related Work para, Setup wrapper para, sec:wild, sec:equalized)

**Verdict: UPHELD-WITH-CORRECTIONS** (every tab:wild cell verified
digit-for-digit against h_numbers.md, incl. rounding rule; H2 numbers,
issue citations, 349/350, 300-run zero-revisit claim all clean).

Dispositions:
1. **[MAJOR]** "validated before any library was read: … mixed-space
   duplicate-injection gates" was chronologically false (G5 ran before
   H2, after H1). → Sentence split: G1–G4 pre-library, G5 explicitly
   dated before sec:equalized.
2. **[MAJOR]** "random control sits at zero by construction" contradicted
   the table's own Random +1 cell. → "zero in expectation", with the +1
   cited as the finite-sample illustration.
3. **[MAJOR]** "re-ran the three wasteful libraries on the four
   benchmarks (275 runs)" implied 300 and conflated two carryover
   caveats. → Rewritten: TPEs on four, optuna-gp on three with the pest
   no-op carryover explained; skopt-gp ~60-unique d3 exception disclosed
   inline with its outcome-neutrality.
4. **[MINOR]** "trade places" on 11^6 overstated a tie resolving. →
   "tied as shipped, separate with Hyperopt ahead."
5. **[MINOR]** SMAC 22/25 now notes early-terminated runs are scored on
   their completed prefix.

---

# H5 Cycle 7 — section review record (Appendix A, Protocol item 6, Experiments (v), Data Availability, disclosure paragraph)

**Verdict: UPHELD-WITH-CORRECTIONS.** All recomputable numbers verified
exactly (0/2250; held-out R² 0.66–0.88; s∈[3.2,22.3]; −1.28; 5/8 ρ<1;
1050/275 runs; versions; seed ranges incl. 3001–3025 verified by min/max
over raw files; core-hours 13.21/1.45; \mathbb/\blacksquare compile via
revtex amssymb class option).

Dispositions:
1. **[MAJOR]** "the first in-the-wild analysis" mislabeled H0's refuted
   validation analysis with sec:wild's own title (H1's analysis was
   UPHELD-WITH-CORRECTIONS, not refuted). → "the audit-instrument
   validation analysis". Count "four refuted" verified correct
   (E3, E7, H0, H2).
2. **[MAJOR]** Appendix "on the same pools process" overstated E2-vs-E3
   matching (different RNG stream formulas → independently drawn pools,
   not Lemma-A coupling; also restored the specific oracle-arm
   identity). → "permissive combination-dedup arm under the same
   generator and dedup configuration … independently drawn pools, not
   Lemma-A-style coupled realizations".
3. **[MAJOR]** Oracle-section pointer "a proved monotone decay law"
   risked conflating the per-step theorem with the unproved sequential
   curve. → "proved monotone per-step selection law (the
   sequential-loop decay curve remains a fitted, not proved,
   description)".

---

# H5 Cycle 8 — FULL-PASS review record (entire main.tex, three lenses)

**Verdict: CORRECTIONS-NEEDED (minor-to-moderate) → all applied.** The
reviewer traced every quantitative claim in the whole paper (legacy and
new sections) against h_numbers.md, numbers.md, CLAIMS.md, all five
experiment ANALYSIS files, and THEORY.md: **zero fabricated, stale, or
double-counted numbers found**; all forbidden refuted phrasings absent;
all labels/refs/cites/environments balanced; no narrative seams between
old and new spines.

Dispositions:
1. **[MAJOR]** Discussion never returned to the ecosystem audit (the
   paper's headline) → snapshot-scope sentence added to "Scope of the
   machinery family" (version-pinned, six libraries, no stability claim,
   wrapper released so the measurement can be repeated), plus the
   pest-control dimensional clarification.
2. **[MINOR]** "two pre-registered hypotheses failed" undercounted
   (H2's Hyperopt strict clause is a third) → "three", with
   sec:equalized added to the citation.
3. **[MINOR]** Abstract ~375 words → four trims applied (now ≈352).
4. **[MINOR]** tab:wild 7 columns in one AIP column → converted to
   table* (double-column span).
5. **[NOTE]** bergstra2012random unused → now cited at the
   random-sampling control (checker: 0 unused-bib).
6. **[NOTE accepted, no change]** \argmin/\argmax macros and booktabs
   loaded-but-unused (cosmetic; compile untested, so no macro churn);
   class/.bst files not co-located (already AUTHOR_TODO item 1);
   K2a-fig phrasing optional tightening declined (factually exact).

Reviewer's overall assessment, quoted: "a coherent, submission-shaped
draft" modulo the author tasks (title, co-authors, compile, anchor
citations).
