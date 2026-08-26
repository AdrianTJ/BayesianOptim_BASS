# H0 — Instrumentation validation (design, written before running)

**Purpose:** Establish that `bo_audit`'s revisit counter is correct and that
it detects the duplicate behavior the ecosystem has already admitted to,
before any novel library reading is trusted. No headline claims come from
this cycle; it is a gate.

## Ground-truth checks (counter correctness)

| # | Check | Pass criterion |
|---|---|---|
| G1 | Scripted optimizer that evaluates a fixed config exactly k=7 times, then 3 distinct configs | counter reads exactly 6 revisits, 4 unique |
| G2 | Random search, pure continuous 4-dim space, budget 200, 5 seeds | 0 revisits in every run (no false positives from continuous rounding at 6 decimals) |
| G3 | Random search, tiny categorical space (2 vars × 2 levels = 4 combos), budget 40, 20 seeds | counter's mean revisits within 2 of the analytic expectation E[revisits] = 40 − E[unique] where E[unique] = 4·(1−(3/4)^40) ≈ 4.0 → expected ≈ 36 |
| G4 | Independent recount: brute-force recount of G3's call logs by a separate script must equal the counter exactly | exact match |

## Documented-behavior checks (does the audit see what the issues admit?)

Benchmark: Cat-Ackley d=3, L=5 (125 combinations, deterministic; our
validated objective from `experiments/machinery.py`, adapted to config
dicts). Budget 80, 10 seeds (2001–2010; fresh range, no overlap with any
prior experiment), library defaults, seed as the only non-default.

| # | Library | Documented basis | Pre-registered detection criterion |
|---|---|---|---|
| D1 | hyperopt-tpe | issue #608 ("exact same point sampled in multiple trials", 48/500 on a discrete grid) | median revisits ≥ 5 of 80 |
| D2 | optuna-tpe | issues #5440/#2021 (duplicate suggestions on discrete grids) | median revisits > 0 |
| D3 | skopt-gp (one-hot GP) | no admitted issue; mechanism-based expectation (relaxation+rounding class) | reported descriptively, no threshold |
| D4 | random baseline | pigeonhole on 125 combos at budget 80 | E[revisits] = 80 − 125·(1−(124/125)^80) ≈ 20.6; observed mean within 5 |

D1/D2 are detection gates: if the counter cannot see behavior the
libraries' own trackers document, the instrumentation is wrong and H1 does
not start. D3 is exploratory. D4 doubles as a third counter-correctness
check on the real benchmark.

## Deviations/notes
- optuna-gp deferred to H1 (no admitted-issue ground truth; nothing to
  validate against here).
- SMAC3 install currently broken in-container (old build importing a
  private sklearn symbol); retry with a version pin in H1, logged not
  silent.
- Ax/BoTorch installing in background; joins in H1.
