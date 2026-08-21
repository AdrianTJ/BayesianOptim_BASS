#!/usr/bin/env python3
"""Standalone SMAC 2.4 runner, executed inside the isolated smac venv.

Usage: smac_runner.py <benchmark> <budget> <seed>
Benchmarks: cat_ackley_d3_L5 | pest_control | func2C | func3C
Prints one JSON line: the bo-audit summary + config disclosure.

Counting happens HERE (same AuditedObjective code, imported from this
package) because the audited objective must live in the process that calls
it. Non-defaults: seed; deterministic=True in Scenario (our objectives are
deterministic — SMAC would otherwise re-evaluate incumbents by design,
which must not be confused with silent duplicate suggestions);
output_directory to a temp dir; logging quieted.
"""
import json
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))          # bo_audit
sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "article_loop" / "experiments"))
from bo_audit.core import AuditedObjective


def main():
    bench, budget, seed = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
    from bo_audit.benchmarks import bench_by_name
    fn, space = bench_by_name(bench)
    audited = AuditedObjective(fn, space)

    from ConfigSpace import ConfigurationSpace, Categorical, Float
    from smac import HyperparameterOptimizationFacade, Scenario
    cs = ConfigurationSpace(seed=seed)
    for spec in space:
        name, kind = spec[0], spec[1]
        if kind == "cat":
            cs.add(Categorical(name, spec[2]))
        else:
            cs.add(Float(name, (spec[2], spec[3])))

    def target(config, seed=0):
        return audited({k: config[k] for k in config})

    exhausted = False
    with tempfile.TemporaryDirectory() as td:
        scen = Scenario(cs, deterministic=True, n_trials=budget, seed=seed,
                        output_directory=td)
        fac = HyperparameterOptimizationFacade(scen, target, overwrite=True,
                                               logging_level=40)
        try:
            fac.optimize()
        except Exception as e:  # ConfigurationSpaceExhaustedException et al.
            # H1 Amendment 2: on small categorical spaces SMAC sometimes
            # refuses to propose further configs rather than duplicate and
            # raises mid-run; report the audit of the completed prefix plus
            # the termination reason instead of losing the run.
            if "Exhausted" not in type(e).__name__:
                raise
            exhausted = True

    import importlib.metadata as im
    out = audited.summary()
    out.update({"library": "smac", "version": im.version("smac"),
                "non_defaults": "seed; deterministic=True; temp output dir; logging quiet",
                "benchmark": bench, "seed": seed,
                "early_termination": "ConfigurationSpaceExhausted" if exhausted else None})
    print(json.dumps(out))


if __name__ == "__main__":
    main()
