# H3 — Theory appendix (pre-registered scope + fit protocol)

**Committed before the fit runs.** H3 produces THEORY.md (statements +
proofs), a fitted decay-law analysis for the guidance dial, and a
prover-skeptic adversarial review of every mathematical claim.

## Scope decided by the anchor gate

PLAN.md's hard gate: no appendix citation of Kim et al. (arXiv:2506.11831)
or B3O (arXiv:2606.30228) without a full-text check. **arXiv remains
egress-blocked in this container (re-tested this cycle)** → the appendix
is written fully self-contained; both anchors stay snippet-verified-only
and are queued for the author with exact "verify before citing" notes.
No regret-decomposition claims that depend on those papers' theorems are
made.

## Statements to prove or state (drafted in THEORY.md, reviewed adversarially)

- **Lemma A (exogenous-pool oracle bound, a theorem):** for any pool
  sequence generated independently of the selection history, the oracle
  selector's best-so-far weakly dominates every selection policy's
  best-so-far on the same pools, pointwise in t and pathwise. Proof is
  elementary; its value is delimiting *exactly when* the audit's ceiling
  is a theorem.
- **Proposition B (adaptive case, an explicitly-labeled conjecture):**
  with history-dependent generators (our hybrid generator conditions on
  the incumbent), the bound can fail in principle because policies induce
  different pool sequences; we state the conjectured domination under a
  generator-stability assumption and cite the empirical record (E2/E3:
  oracle ≥ every surrogate in every matched cell) as support, not proof.
- **Proposition C (guidance-dial decay law):**
  (a) single-draw: for selection by argmax of −f(x_i) + σ·ε_i with iid
  standard-Gaussian ε over a fixed finite pool, P(the pool argmin is
  selected) is continuous, equals 1 at σ→0⁺ (distinct values), tends to
  1/n as σ→∞, and is **monotone non-increasing in σ** (proof via the
  representation E_e[Π_j Φ(e + Δ_j/σ)]).
  (b) Gumbel twin: with iid Gumbel(σ) noise the law is exactly softmax
  P(i) ∝ exp(−f_i/σ) (Plackett–Luce top-1) — closed form, same limits,
  same monotonicity. Stated as the analytic twin; **our experiments used
  Gaussian noise** (run_e8.py line 32), never claimed otherwise.
  (c) sequential reading: as σ→∞ the dial converges to uniform-over-pool
  selection under the same generator (not to free random search) — the
  correct named baseline for the decay's far end.

## Pre-registered fit protocol (before looking at fit output)

Data: E8 (seeds 1026–1050, the pre-registered K10 grid) is the fit set;
E7's dial arm (seeds 1001–1025, same σ grid {0,1,3,10,30,100}) is the
**held-out replication set** — E8-fitted curves are scored on E7 medians
with no refitting. 8 cells: {func2C, func3C} × {keep, flip} × n_cand
{50, 1000}.

Model per cell, fit to median best_b80 over the σ grid:
  m(σ) = m₀ + (m∞ − m₀) · 1/(1 + (s/σ)^γ)   for σ > 0; m(0) = m₀
free parameters m∞, s (half-loss dial value), γ; m₀ pinned to the σ=0
median. Least squares on the 5 positive-σ medians (3 free params, 5
points — stated plainly; the replication set is the real test).

Descriptive criteria named now (no gates — H3 makes no run/no-run
decision): report per-cell fit R², held-out R² on E7, Spearman ρ of
(σ, median) per cell as the monotonicity check, and fitted s per cell.
The K10 caveat stands: E8's func3C n=50 σ=100 residual (W=18/25 vs flip)
is a *between-generator* statistic, not a violation of C(a)'s
*within-cell single-draw* monotonicity — THEORY.md must state explicitly
what C(a) does and does not predict about the sequential loop, and any
non-monotone empirical medians are disclosed, not smoothed.

## Review

A prover-skeptic (worker≠verifier) checks: every proof step, the
correctness of the monotonicity argument, the Gumbel/softmax
correspondence, the fit code against this protocol, and whether any
THEORY.md sentence overclaims (esp. Prop B's status as conjecture and
C(c)'s limit statement). Ledger entries only after that review.
