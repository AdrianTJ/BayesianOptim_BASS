# H7 Amendment 1: same-environment ordinal control (post-review)

DESIGN.md is frozen and is not edited; this file amends it. Written
2026-10-02 after the 150 nominal runs and the independent review
(`REVIEW.md`), before any run it governs.

## Why

The review replayed H1's ordinal Ax runs in H7's environment. Cat-Ackley
5^5 and 11^6 reproduce H1 exactly; pest_control, func2C and func3C do
not (6 of 6 replayed cells differ, e.g. func3C seed 3001: −0.666 in H1,
+0.042 here). H1 recorded no environment, so on those three benchmarks
the paired nominal-vs-H1 comparison mixes the ordering effect with
environment drift.

## Change

Add an **ordinal control arm** run in H7's environment and code path:
the same `run_h7.py` cell (released package, Ax 1.3.1, `Client(random_seed=seed)`,
one trial per ask, budget 80, seeds 3001–3025, the same six benchmarks,
the same caps), with exactly one difference: every categorical gets
`is_ordered=True` (Ax's default for integer choices, i.e. what H1 ran).
The flag is forced by wrapping `ax.ChoiceParameterConfig` inside the cell
(`run_h7_ordinal.py`); rows are labelled
`"... is_ordered=True on categoricals (H7 ordinal control)"`.
150 runs, written to `results_ordinal.jsonl`; failures to
`failures_ordinal.log`, committed.

## Analysis

X1–X3 are unchanged and keep their verdicts. The descriptive
nominal-vs-ordinal table is recomputed against this arm instead of H1;
the H1 comparison is kept alongside, labelled as cross-environment.
X2's comparison against the committed 18/25 is unchanged as registered;
the same-environment ordinal count is reported next to it.
