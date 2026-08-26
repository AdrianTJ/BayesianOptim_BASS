# G-sweep status — INCOMPLETE, PAUSED

This folder holds the G-sweep, the pre-registered H6 generalization
program (see `DESIGN.md`). **The program was interrupted after the
fast-arm wave and never resumed: the GP-family arms (optuna-gp,
skopt-gp, ax) are essentially unrun, and one pre-registered hypothesis
fails outright on the data that does exist.** `results.jsonl` is a
frozen, partial experimental record, not a finished dataset. There is
no `ANALYSIS.md` and none is claimed here — this note is a status
record, not an analysis.

## What ran

Counted directly from `results.jsonl` (10,203 lines, one JSON object
per line, no duplicate `(arm, benchmark, budget, seed)` keys, no parse
errors) against the cells `DESIGN.md` and `run_g.py` actually
pre-register — i.e. respecting the smac coverage limit (16 of 23
benchmarks, classes A–D+F) and the Amendment-1 optuna-tpe-3.6 limit
(20 of 23 benchmarks, YAHPO excluded) rather than the pre-amendment
headline total DESIGN quotes:

| arm | planned cells | planned runs (25 seeds/cell) | actual runs | % complete |
|---|---|---|---|---|
| random | 92 | 2,300 | 2,300 | 100.0% |
| optuna-tpe (4.9) | 92 | 2,300 | 2,300 | 100.0% |
| optuna-tpe-3.6 | 80 | 2,000 | 2,000 | 100.0% |
| hyperopt-tpe | 92 | 2,300 | 2,300 | 100.0% |
| smac | 64 | 1,600 | 1,300 | 81.2% |
| optuna-gp | 77 | 1,925 | 1 | 0.05% |
| skopt-gp | 77 | 1,925 | 1 | 0.05% |
| ax | 77 | 1,925 | 1 | 0.05% |
| **total** | **651** | **16,275** | **10,203** | **62.7%** |

(DESIGN's own arithmetic, "Total 16,575 runs," predates Amendment 1's
YAHPO exclusion for optuna-tpe-3.6, which removes 3 benchmarks × 4
budgets × 25 seeds = 300 runs from that arm's total; 16,575 − 300 =
16,275 is the actual pre-registered total after the amendment, and is
the denominator used above. This is a bookkeeping note, not a change
to DESIGN, which is not edited by this record.)

The four TPE-family fast arms (random, optuna-tpe, optuna-tpe-3.6,
hyperopt-tpe) are complete: every one of their 356 planned cells has
its full 25 seeds. smac is 81.2% complete. The three GP-family arms
are, in effect, unrun: each has exactly one recorded run, and in each
case it is the single pre-registered timing-smoke cell from `DESIGN.md`'s
Procedure step 2 (`ax`×ml_rf_digits/B40,
`optuna-gp`×nk_n20k8/B80, `skopt-gp`×labs_n25/B80) — not the start of
a real wave.

**GP-family completion specifically: 3 of 5,775 planned runs (0.05%).**
**Fast-arm completion (the four complete arms plus smac): 10,200 of
10,500 planned runs (97.1%).**

## What did not run

Cell-level accounting (a cell is "full" at 25/25 seeds, "partial" at
1–24, "missing" at 0):

| arm | full cells | partial cells | missing cells |
|---|---|---|---|
| random / optuna-tpe / optuna-tpe-3.6 / hyperopt-tpe | 356/356 | 0 | 0 |
| smac | 51/64 | 2 | 11 |
| optuna-gp | 0/77 | 1 | 76 |
| skopt-gp | 0/77 | 1 | 76 |
| ax | 0/77 | 1 | 76 |

**The GP-family arms account for the overwhelming majority of the
shortfall:** 228 of 231 GP-arm cells (98.7%) have no data at all, and
the 3 that do have exactly 1 of 25 seeds. Against DESIGN's own
"incomplete" bar (a cell missing more than 5 of 25 seeds is reported
incomplete), all 228 fully-missing GP cells and all 3 GP smoke cells
qualify; among smac's partial cells, `contam_2p25`/B20 (1/25 seeds)
also qualifies, while `pest_control`/B160 (24/25 seeds) does not.

smac's 13 non-full cells are all in the two classes with the most
build friction historically (D and F): `contam_2p25` at B40/B80/B160
(0/25 each) and B20 (1/25); `func2C` and `func3C` at all four budgets
(0/25 each, 8 cells total). `pest_control`/B160 is missing only its
last seed (24/25).

Finishing the matrix requires, concretely:

- **The entire GP wave (G3 in the loop's terms):** 5,772 more runs
  across optuna-gp, skopt-gp, and ax — 76 of each arm's 77 cells, at
  full 25 seeds, still to run (the 77th cell in each case already has
  1 of 25).
- **A smac backfill:** 300 more runs — `contam_2p25` (24 seeds at B20,
  25 each at B40/B80/B160 = 99 runs) and `func2C`/`func3C` (25 seeds ×
  4 budgets × 2 benchmarks = 200 runs), plus the 1 missing seed on
  `pest_control`/B160.
- Then, per `DESIGN.md`'s own Procedure (step 5): `ANALYSIS.md` written
  from the completed results, worker≠verifier adversarial review, and
  only after that any update to the claims ledger or the paper.

## Hypothesis status on partial data — provisional, unreviewed

Running `python3 analyze_g.py` against the current `results.jsonl`
produces this letter-evaluation block (reproduced verbatim; a
`ConstantInputWarning` from `scipy.stats.spearmanr` on constant-input
cells prints to stderr and does not affect the printed verdicts):

```
- **GH1a** optuna-tpe e(80)>0.05 per class: ['A', 'B', 'C', 'D'] (4/5) → PASS
- **GH1b** hyperopt-tpe e(80)>0.05 per class: ['A', 'B', 'C', 'D'] (4/5) → PASS
- **GH2** ax/smac zero-revisit violations: none; skopt |e|>0.07: none → PASS
- **GH3** rho(B, e)>=0 in 66/66 = 1.00 of no-dedup cells → PASS
- **GH4** optuna-tpe-3.6 classes passing: ['A', 'B', 'C', 'D'] (4/5) → PASS (class E over ml_* only, per Amendment 1)
- **GH5** real-ML benchmarks with e(80)>=0.05 per no-dedup arm: {'optuna-tpe': 1, 'optuna-tpe-3.6': 1, 'hyperopt-tpe': 0, 'optuna-gp': 0} → FAIL
- **GH6** (descriptive) Kendall tau between B=20 and B=160 arm rankings (median best; fast arms, all covered benchmarks):
    class A: median tau +nan over 5 benchmarks
    class B: median tau +0.32 over 3 benchmarks
    class C: median tau +0.16 over 4 benchmarks
    class D: median tau +0.20 over 1 benchmarks
    class E: median tau +nan over 7 benchmarks
- **GH7** random median revisits on float-bearing spaces: violations none → PASS; no-dedup nonzero (findings, not failures): none

(distinct failed run attempts: 0; incomplete cells flagged as — above)
```

**These verdicts are provisional: they are the script's literal output
on a partial matrix, and they have not been through the
worker≠verifier adversarial review this project's process requires
before any result is treated as a finding.** They must not be read as
settled outcomes.

Which clauses the current data can actually speak to differs sharply
by which arms they name:

- **GH1, GH4, GH7 are well-supported by the current data.** They
  reference only random, optuna-tpe, hyperopt-tpe, and optuna-tpe-3.6
  — the four arms that are 100% complete. Their PASS verdicts rest on
  full pre-registered coverage, not partial data.
- **GH2 is only partly testable, and its two GP-family components are
  essentially untested.** The clause names ax, smac, and skopt-gp.
  Checked directly: smac has data in 53 of its 64 covered cells (the
  meaningfully-tested part); **ax and skopt-gp each have data in only
  1 of their 77 covered cells** — the lone smoke run. A PASS verdict
  built from "no violation found" is not informative when the arm has
  almost no observations to violate on. This is the clause DESIGN's
  own wording ties most directly to the unrun GP arms.
- **GH3 nominally covers four "no-dedup" arms (optuna-tpe,
  optuna-tpe-3.6, hyperopt-tpe, optuna-gp) but optuna-gp contributes 0
  of the 66 cells behind the 66/66 figure** — every contributing cell
  comes from the three complete TPE-family arms (optuna-tpe: 23,
  optuna-tpe-3.6: 20, hyperopt-tpe: 23). The reported PASS is real for
  the three TPE arms and says nothing about optuna-gp.
- **GH5 is well-supported despite the missing GP data**, because its
  verdict is the maximum across the four no-dedup arms, and that
  maximum (1) is already set by a complete arm (optuna-tpe); optuna-gp
  contributing 0 real-ML cells (it never ran a class-E benchmark) does
  not change the FAIL. See below.
- **GH6 is descriptive (no gate) and uses only the fast arms**, all of
  which are complete or near-complete for the benchmarks it touches;
  it is not blocked by the GP shortfall. The `nan` entries for classes
  A and E reflect tied median-best rankings at one or both budgets
  (Kendall's tau is undefined for a constant ranking), not missing
  arms.

## The GH5 result specifically

GH5 asked whether real-ML degradation from revisit waste generalizes:
at least one no-dedup arm should show median e(80) ≥ 0.05 on at least
4 of the 7 class-E (real ML) benchmarks. On the current data it does
not. The best of the four no-dedup arms — optuna-tpe — clears that bar
on only 1 of its 7 covered class-E benchmarks; optuna-tpe-3.6 clears it
on 1 of its 4 covered (YAHPO-excluded) class-E benchmarks; hyperopt-tpe
clears it on 0; optuna-gp never ran a class-E cell at all. The script
reports this as **FAIL**, and this verdict is not an artifact of
missing GP data — it is set by the fully-run TPE-family arms. Plainly:
on the real-ML benchmarks that were actually run, the excess-revisit
waste signal that GH1 found on the synthetic classes does not show up
for the no-dedup samplers, at the letter GH5 pre-registered. This
contradicts the corresponding pre-registered hypothesis on the data
collected so far.

## Relationship to the paper

`../../../main.tex` was searched for any reference to this sweep. It
was not found. A grep for the G-sweep's specific benchmark names
(`catf_*`, `nk_n20*`, `maxcut_n20`, `labs_n25`, `contam_2p25`,
`ml_rf_digits`, `ml_svm_digits`, `ml_gb_bc`, `ml_mlp_wine`, `yahpo_*`,
`func2C`/`func3C` in this context, "23 benchmarks", "GH1"–"GH7",
"16,575"/"16,275") returns nothing. The paper does discuss revisits,
the pigeonhole baseline, categorical Ackley at three sizes, and a
25-seeds/budget-80 protocol — but that text (main.tex lines ~440–537,
Table `tab:wild`) describes the separate, already-completed H1
in-the-wild audit matrix (6 benchmarks: Cat-Ackley d3/d5/d6,
pest-control, and the two CoCaBO functions; 7 arms; 25 seeds; budget
80; 1,050 runs total), which has its own results file and predates
this sweep — DESIGN.md itself frames the G-sweep as the follow-on
question of whether *that* matrix's findings generalize. **No number
in the paper currently derives from `exph6_sweep/results.jsonl` or
from `analyze_g.py`'s output.**

## Provenance

- Total-rows and per-arm/per-cell figures above: computed from
  `results.jsonl` directly against the coverage rules in `DESIGN.md`
  and the `CLASSES` / `SMAC_COVER` / `O36_SKIP` / `GP160` / `BUDGETS`
  / `SEEDS` constants in `run_g.py`.
- GH1–GH7 block: the unmodified stdout of `python3 analyze_g.py`, run
  from this folder against the committed `results.jsonl`.
- `main.tex` claim: `grep`-checked as described above.
