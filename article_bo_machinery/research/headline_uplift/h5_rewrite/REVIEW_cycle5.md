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
