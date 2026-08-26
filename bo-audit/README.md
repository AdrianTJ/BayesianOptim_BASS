# bo-audit

Objective-wrapper instrumentation for Bayesian optimization: it counts
**decoded-combination revisits** — evaluations an optimizer re-spends on
configurations it has already evaluated — uniformly across any ask/tell
library, at documented defaults, without touching library internals.

Because every optimizer must call the objective, wrapping the objective is
the one vantage point shared by all of them. Each call is decoded to its
categorical combination (plus rounded continuous coordinates) and checked
against everything seen before. Convergence curves cannot see duplicate
waste; this can.

Companion tool for *"The Machinery Confound: Acquisition-Optimization
Machinery Can Dominate Surrogate Comparisons in Mixed and Categorical
Bayesian Optimization"* — the wrapper behind the six-library ecosystem
audit (1,050 runs): 3 of 6 libraries silently re-spent ≥10% of their
budget beyond unavoidable collisions; the worst re-spent a median of 53 of
80 evaluations while posting the best solve record in the matrix.

## Install

```bash
pip install bo-audit            # core instrument, stdlib-only
pip install "bo-audit[optuna]"  # + a library driver to audit
```

## Use

Wrap any `fn(config_dict) -> float`, hand the wrapper to the optimizer in
place of the objective, and read the audit off it afterwards:

```python
from bo_audit import AuditedObjective
from bo_audit.drivers import run_optuna_tpe, run_random

LEVELS = ["0", "1", "2", "3", "4"]
space = [
    ("x", "cat", LEVELS),
    ("y", "cat", LEVELS),
    ("z", "cat", LEVELS),
]

def objective_fn(config):
    i, j, k = int(config["x"]), int(config["y"]), int(config["z"])
    return (i - 2) ** 2 + (j - 2) ** 2 + (k - 2) ** 2  # single optimum at (2, 2, 2)

audited = AuditedObjective(objective_fn, space)
run_optuna_tpe(audited, space, budget=80, seed=0)
print("tpe:", audited.summary())
# tpe: {'evals': 80, 'revisits': 47, 'revisit_frac': 0.5875, 'best': 0, 'unique': 33}

audited_random = AuditedObjective(objective_fn, space)
run_random(audited_random, space, budget=80, seed=0)
print("random:", audited_random.summary())
# random: {'evals': 80, 'revisits': 19, 'revisit_frac': 0.2375, 'best': 0, 'unique': 61}
```

Raw revisit counts are only meaningful against the space's pigeonhole
baseline — the revisits forced by chance alone if evaluations landed
uniformly at random over the K = 5×5×5 = 125 combinations, `B - K·(1 -
(1 - 1/K)^B)`. At K=125, B=80 that baseline is **20.7**. The uniform-random
control above lands at 19 revisits — statistically at the baseline, i.e.
no meaningful waste. TPE lands at 47 — an excess of about 26 revisits
(~33% of the budget) above what chance alone would force, which is a real
finding about sampler behavior, not a counting artifact. Always read
`revisits` this way: against the baseline for that K and B, not as a raw
number. Revisits like this register on categorical/finite spaces where
exact repeats are possible; once a space has a continuous (`"float"`)
dimension, coordinates are keyed at 6 decimal places (below), so exact
repeats — and therefore counted revisits — are typically zero even under
heavy resampling.

The space schema is library-agnostic:

```python
[("name", "cat", [choices...]) | ("name", "float", lo, hi) | ("name", "int", lo, hi)]
```

Continuous coordinates are keyed at 6 decimal places by default
(`cont_decimals`), so near-misses are not counted as revisits; only exact
categorical-combination repeats with numerically identical continuous
parts register.

### Budget-equalized control

`MemoizedAuditedObjective` serves cached values for already-seen keys at
zero objective cost and raises `AuditStop` once a unique-evaluation budget
is spent. On deterministic objectives the sampler's trajectory is
bit-identical to uncached execution — it changes what the budget is
charged for, not what the sampler sees. That is the control under which
apparent library rankings changed on 3 of 4 benchmarks in the paper.

### Drivers

`bo_audit.drivers` runs audited sessions for optuna (TPE and GP samplers),
hyperopt, scikit-optimize, and Ax, each at documented defaults with every
non-default recorded in the returned config dict. Library imports are
lazy: the core instrument has no third-party dependencies.

### Benchmarks

`bo_audit.benchmarks` vendors the deterministic pest-control simulation
(5^25, adapted from COMBO with the determinization disclosed in the paper)
and the G-sweep families (`benchmarks_g`: categorical Rastrigin, Rosen-
brock, Michalewicz, Griewank, Schwefel, NK-landscapes, max-cut, LABS, ...).
The cat-Ackley and CoCaBO builders live with the experiment harness in the
repository (`article_bo_machinery/research/`) and are dispatched by
`bench_by_name` when that tree is on the path.

## Verification

The counting logic is covered by `tests/`:

```bash
python -m pytest tests
```

## Citation

```bibtex
@article{jacobo2026machineryconfound,
  author = {Jacobo, Adrian Tame},
  title  = {The Machinery Confound: Acquisition-Optimization Machinery Can
            Dominate Surrogate Comparisons in Mixed and Categorical
            Bayesian Optimization},
  year   = {2026},
  note   = {Preprint}
}
```

## License

MIT.
