"""bo-audit drivers: run each library on (AuditedObjective, space, budget).

Each driver takes (audited, space, budget, seed) and runs the library with
its DOCUMENTED defaults (fairness rule: defaults, not straw men; any
non-default is recorded in the returned config dict). The audited objective
does all counting.
"""


def run_optuna_tpe(audited, space, budget, seed):
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
    study.optimize(obj, n_trials=budget)
    return {"library": "optuna-tpe", "version": optuna.__version__, "non_defaults": "seed only"}


def run_optuna_gp(audited, space, budget, seed):
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
    study.optimize(obj, n_trials=budget)
    return {"library": "optuna-gp", "version": optuna.__version__, "non_defaults": "seed only"}


def run_hyperopt_tpe(audited, space, budget, seed):
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

    fmin(fn=audited, space=hspace, algo=tpe.suggest, max_evals=budget,
         trials=Trials(), rstate=np.random.default_rng(seed),
         show_progressbar=False)
    import hyperopt
    return {"library": "hyperopt-tpe", "version": hyperopt.__version__, "non_defaults": "seed only"}


def run_skopt_gp(audited, space, budget, seed):
    from skopt import gp_minimize
    from skopt.space import Categorical, Integer, Real

    dims, names = [], []
    for spec in space:
        name, kind = spec[0], spec[1]
        names.append(name)
        if kind == "cat":
            dims.append(Categorical(spec[2], name=name))
        elif kind == "int":
            dims.append(Integer(spec[2], spec[3], name=name))
        else:
            dims.append(Real(spec[2], spec[3], name=name))

    def obj(x):
        return audited(dict(zip(names, x)))

    gp_minimize(obj, dims, n_calls=budget, random_state=seed)
    import skopt
    return {"library": "skopt-gp", "version": skopt.__version__, "non_defaults": "seed only"}


def run_random(audited, space, budget, seed):
    """Uniform random baseline through the identical audited objective."""
    import numpy as np
    rng = np.random.default_rng(seed)
    for _ in range(budget):
        cfg = {}
        for spec in space:
            name, kind = spec[0], spec[1]
            if kind == "cat":
                cfg[name] = spec[2][rng.integers(len(spec[2]))]
            elif kind == "int":
                cfg[name] = int(rng.integers(spec[2], spec[3] + 1))
            else:
                cfg[name] = float(rng.uniform(spec[2], spec[3]))
        audited(cfg)
    return {"library": "random", "version": "-", "non_defaults": "-"}


DRIVERS = {
    "optuna-tpe": run_optuna_tpe,
    "optuna-gp": run_optuna_gp,
    "hyperopt-tpe": run_hyperopt_tpe,
    "skopt-gp": run_skopt_gp,
    "random": run_random,
}
