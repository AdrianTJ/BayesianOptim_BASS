"""H2 memoized drivers: identical library configs to H1's drivers.py,
with a 400-ask horizon and AuditStop-based early stop once 80 unique
evaluations are charged. H1 drivers stay frozen; only the stopping
mechanics differ here (recorded in non_defaults).
"""
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
from bo_audit.memo import AuditStop

HORIZON = 400


def run_optuna_tpe_memo(audited, space, budget, seed):
    import optuna
    optuna.logging.set_verbosity(optuna.logging.WARNING)

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
    try:
        study.optimize(obj, n_trials=HORIZON)
    except AuditStop:
        pass
    return {"library": "optuna-tpe", "version": optuna.__version__,
            "non_defaults": "seed; 400-ask horizon w/ memoized objective + AuditStop (H2 control)"}


def run_optuna_gp_memo(audited, space, budget, seed):
    import optuna
    optuna.logging.set_verbosity(optuna.logging.WARNING)

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

    study = optuna.create_study(sampler=optuna.samplers.GPSampler(seed=seed))
    try:
        study.optimize(obj, n_trials=HORIZON)
    except AuditStop:
        pass
    return {"library": "optuna-gp", "version": optuna.__version__,
            "non_defaults": "seed; 400-ask horizon w/ memoized objective + AuditStop (H2 control)"}


def run_hyperopt_tpe_memo(audited, space, budget, seed):
    from hyperopt import fmin, tpe, hp, Trials
    import numpy as np

    hspace = {}
    for spec in space:
        name, kind = spec[0], spec[1]
        if kind == "cat":
            hspace[name] = hp.choice(name, spec[2])
        elif kind == "int":
            hspace[name] = hp.uniformint(name, spec[2], spec[3])
        else:
            hspace[name] = hp.uniform(name, spec[2], spec[3])

    try:
        fmin(fn=audited, space=hspace, algo=tpe.suggest, max_evals=HORIZON,
             trials=Trials(), rstate=np.random.default_rng(seed),
             show_progressbar=False)
    except AuditStop:
        pass
    import hyperopt
    return {"library": "hyperopt-tpe", "version": hyperopt.__version__,
            "non_defaults": "seed; fresh Trials(); progressbar off; "
                            "400-ask horizon w/ memoized objective + AuditStop (H2 control)"}


MEMO_DRIVERS = {
    "optuna-tpe": run_optuna_tpe_memo,
    "optuna-gp": run_optuna_gp_memo,
    "hyperopt-tpe": run_hyperopt_tpe_memo,
}
