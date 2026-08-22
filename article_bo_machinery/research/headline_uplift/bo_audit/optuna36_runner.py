#!/usr/bin/env python3
"""Standalone optuna 3.6 TPE runner (H6 version axis), executed inside the
optuna36 venv. Usage: optuna36_runner.py <benchmark> <budget> <seed>
Prints one JSON line: bo-audit summary + config disclosure. Counting
happens here with the same bo_audit code (venv has numpy/scipy/sklearn
1.9.0/yahpo-gym so every benchmark family evaluates identically to the
main env)."""
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))          # bo_audit
sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "article_loop" / "experiments"))


def main():
    bench, budget, seed = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
    from bo_audit.benchmarks import bench_by_name
    from bo_audit.core import AuditedObjective
    import optuna
    optuna.logging.set_verbosity(optuna.logging.WARNING)

    fn, space = bench_by_name(bench)
    audited = AuditedObjective(fn, space,
                               canonicalize=getattr(fn, "_canonicalize", None))

    def obj(trial):
        cfg = {}
        for spec in space:
            name, kind = spec[0], spec[1]
            if kind == "cat":
                cfg[name] = trial.suggest_categorical(name, spec[2])
            elif kind == "int":
                cfg[name] = trial.suggest_int(name, spec[2], spec[3])
            else:
                cfg[name] = trial.suggest_float(name, spec[2], spec[3])
        return audited(cfg)

    study = optuna.create_study(sampler=optuna.samplers.TPESampler(seed=seed))
    study.optimize(obj, n_trials=budget)

    out = audited.summary()
    out.update({"library": "optuna-tpe-3.6", "version": optuna.__version__,
                "non_defaults": "seed only", "benchmark": bench, "seed": seed})
    print(json.dumps(out))


if __name__ == "__main__":
    main()
