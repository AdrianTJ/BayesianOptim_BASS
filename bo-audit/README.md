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
from bo_audit.drivers import run_optuna_tpe

space = [
    ("kernel", "cat", ["rbf", "matern"]),
    ("C", "float", 1e-3, 1e3),
]

audited = AuditedObjective(objective_fn, space)
info = run_optuna_tpe(audited, space, budget=80, seed=0)

print(audited.summary())
# {"evals": 80, "revisits": 16, "revisit_frac": 0.2, "best": ..., "unique": 64}
```

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

The counting logic is covered by `bo_audit/tests/`:

```bash
python -m pytest bo_audit/tests
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
