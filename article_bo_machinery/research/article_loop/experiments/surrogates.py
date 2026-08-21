"""Pool-based surrogate acquisitions (GP-EI, RF-EI) and a stock-TPE runner.

Shared by E3+. The GP and RF surrogates plug into machinery.run_bo as
`acquire` closures scoring the shared candidate pool with closed-form
Expected Improvement; TPE runs optuna's own ask/tell machinery (no pool) —
deliberately, since bespoke machinery is the phenomenon under study.
"""
import warnings

import numpy as np
from scipy.stats import norm

from machinery import canonicalize, decode_levels


def _ei(mu, sd, y_best):
    """Closed-form EI for minimization (improvement = y_best - Y)."""
    sd = np.maximum(sd, 1e-12)
    z = (y_best - mu) / sd
    return (y_best - mu) * norm.cdf(z) + sd * norm.pdf(z)


def gp_ei_acquire(X_eval, y_eval, X_cand):
    from sklearn.exceptions import ConvergenceWarning
    from sklearn.gaussian_process import GaussianProcessRegressor
    from sklearn.gaussian_process.kernels import Matern, WhiteKernel

    kernel = Matern(nu=2.5, length_scale=0.2, length_scale_bounds=(1e-2, 1e1)) \
        + WhiteKernel(1e-6, (1e-10, 1e-1))
    gp = GaussianProcessRegressor(kernel=kernel, normalize_y=True,
                                  n_restarts_optimizer=1, random_state=0)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=ConvergenceWarning)
        gp.fit(X_eval, y_eval)
    mu, sd = gp.predict(X_cand, return_std=True)
    return _ei(mu, sd, y_eval.min())


def rf_ei_acquire(X_eval, y_eval, X_cand, seed=0):
    from sklearn.ensemble import RandomForestRegressor

    rf = RandomForestRegressor(n_estimators=100, min_samples_leaf=2,
                               random_state=seed, n_jobs=1)
    rf.fit(X_eval, y_eval)
    preds = np.stack([t.predict(X_cand) for t in rf.estimators_])
    return _ei(preds.mean(axis=0), preds.std(axis=0), y_eval.min())


def run_tpe(objective, budget, seed):
    """Stock optuna TPE with its own machinery; returns best curve aligned to
    run_bo's convention (index 0 = best after its n0 startup trials) plus
    decoded-combination revisit count over the post-startup trials."""
    import optuna

    optuna.logging.set_verbosity(optuna.logging.WARNING)
    schema, fn, d = objective["schema"], objective["fn"], objective["d"]
    n0 = max(2 * d + 1, 8)

    def obj(trial):
        u = np.empty(d)
        for j in range(d):
            if schema.types[j] == "cat":
                L = schema.levels[j]
                lev = trial.suggest_categorical(f"x{j}", list(range(1, L + 1)))
                u[j] = (lev - 0.5) / L
            else:
                u[j] = trial.suggest_float(f"x{j}", 0.0, 1.0)
        return float(fn(u[None, :])[0])

    sampler = optuna.samplers.TPESampler(seed=seed, n_startup_trials=n0)
    study = optuna.create_study(sampler=sampler, direction="minimize")
    study.optimize(obj, n_trials=n0 + budget)

    values = np.array([t.value for t in study.trials])
    best = np.minimum.accumulate(values)[n0 - 1:]          # length budget+1

    # Revisits: post-startup trials whose categorical combination was already
    # evaluated (meaningful as budget waste on pure-cat objectives only).
    def combo_of(trial):
        # numeric key sort ("x10" after "x2"): safe for any dimensionality
        return tuple(v for _, v in sorted(trial.params.items(),
                                          key=lambda kv: int(kv[0][1:]))
                     if isinstance(v, (int, np.integer)))

    seen, revisits = set(), 0
    for i, t in enumerate(study.trials):
        c = combo_of(t)
        if i >= n0 and schema.pure_cat and c in seen:
            revisits += 1
        seen.add(c)
    return {"best": best, "revisits": revisits}
