# H3 — Prover-skeptic review record

**Reviewer:** independent verifier (worker ≠ verifier), briefed to check
every proof line as a mathematician, re-derive the fit from raw CSVs, and
audit the model-vs-implementation gap against machinery.py.

## Verdict

**UPHELD-WITH-CORRECTIONS.** No mathematical claim was false: Lemma A's
coupling proof is airtight; Prop C(a)'s representation, monotonicity,
limits (1/n limit re-derived via Φ(e)~Uniform) and continuity all verify;
Prop C(b)'s scale convention and the logit-form monotonicity remark were
confirmed rigorous (the reviewer supplied the explicit two-line proof:
logit p(σ) = −log Σ_{j≥2} exp(−Δ_j/σ), decreasing in σ). The fit was
reproduced byte-identically, re-fit from scratch with independent
multistart (the γ≈18 cell confirmed as genuine cost-surface degeneracy,
not a bug), and the held-out-R² definition (excluding σ=0 on both sides)
judged correct under the registered protocol. The one MAJOR defect was
evidentiary, not mathematical.

## Findings and dispositions

1. **[MAJOR — citation falsified]** Prop B's empirical record claimed
   "E2 (oracle vs GP/TPE/BASS arms…)" — E2 is surrogate-free by its own
   DESIGN (arms are generator/dedup variants under the oracle); BASS
   appears as a run arm nowhere in E1–E5. The reviewer recomputed the
   correct record: E3 surrogate arms matched per-seed against E2's
   oracle+keep+combination-dedup arm → **0/2250 comparisons where a
   surrogate beat the oracle**. → THEORY and ANALYSIS corrected to the
   finer-grained true statement. (DESIGN.md's scope bullet carried the
   same loose phrasing; left as the pre-registration record, corrected
   here and in THEORY.)
2. **[MINOR]** "n distinct values f_1<f_2≤…" self-contradiction → premise
   restated as "unique minimum, ties among non-minimal values allowed"
   (THEORY + DESIGN echo).
3. **[MINOR]** C(c)'s σ→∞ baseline clarified to the post-dedup-mask
   candidate set (run_bo masks history-duplicates before selection,
   identically at every σ).
4. **[MINOR]** Disclosure added: C(a)'s premise is an a.s.-true
   idealization of the real generator (exact ties possible at the [0,1]
   clip boundary), not an enforced invariant.
5. **[MINOR]** "~3–22× the objective's scale" was an undefined
   normalization → s now reported in raw f-units only.
6. **[MINOR]** Dangling "described in the review file" reference →
   the reviewer's purely-local-generator counterexample inlined into
   THEORY.md Prop B.
7. **[MINOR, cosmetic]** Dead `mono` variable removed from fit_propc.py.
8. **[NOTE — verified]** All 25 numeric callouts in ANALYSIS.md, E7
   filtering, Spearman values, the func2C held-out collapse (−1.276
   reproduced by hand), Lemma A's RNG-stream separation in run_e8.py,
   and the E8 W=18/25 citation all reproduce exactly.

## What enters the ledger

H3-LEMA and H3-PROPC (in their corrected forms); the anchor to-do is
recorded as an author task, not a claim.
