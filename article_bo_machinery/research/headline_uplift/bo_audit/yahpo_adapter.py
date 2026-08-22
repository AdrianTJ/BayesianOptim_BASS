"""YAHPO Gym adapter (H6 G1): real-world HPO response surfaces.

Scenarios/instances are FIXED here pre-measurement. Fidelity parameters
are pinned to their maxima (full-data, full-repl surrogate predictions).
Target: logloss (minimize) — present in both scenario families used.

Key semantics for the one conditional parameter (iaml_ranger's
num.random.splits, active only when splitrule == 'extratrees'):
the benchmark exposes fn._canonicalize, which DROPS the parameter from
the config when its parent condition is false. Runners pass this to
AuditedObjective(canonicalize=...), so two proposals differing only in
an inactive coordinate map to ONE combination key — the pre-registered
active-parameter semantics. rbv2_rpart has no conditionals (verified at
G1: 0 conditions in its opt space).
"""
from pathlib import Path

DATA = Path("/tmp/claude-0/-home-user-BayesianOptim-BASS/"
            "d8cc14fb-4c54-5880-818f-3a67a8836a1b/scratchpad/yahpo_data")
_CACHE = {}


def _bench(scenario):
    if scenario not in _CACHE:
        from yahpo_gym import local_config, benchmark_set
        local_config.init_config()
        local_config.set_data_path(str(DATA))
        _CACHE[scenario] = benchmark_set.BenchmarkSet(scenario)
    return _CACHE[scenario]


def _space_from_cs(cs, skip=()):
    space = []
    hps = list(cs.values()) if hasattr(cs, "values") else cs.get_hyperparameters()
    for h in hps:
        t = type(h).__name__
        if h.name in skip or h.name == "task_id":
            continue
        if t == "CategoricalHyperparameter":
            space.append((h.name, "cat", list(h.choices)))
        elif t == "UniformIntegerHyperparameter":
            space.append((h.name, "int", int(h.lower), int(h.upper)))
        elif t == "UniformFloatHyperparameter":
            space.append((h.name, "float", float(h.lower), float(h.upper)))
        else:
            raise ValueError(f"unhandled hp type {t} for {h.name}")
    return space


def _make(scenario, instance, fidelity, target="logloss", canonicalize=None,
          drop=()):
    b = _bench(scenario)
    cs = b.get_opt_space(drop_fidelity_params=True)
    space = _space_from_cs(cs, skip=drop)
    ti = b.config.y_names.index(target)

    def fn(cfg):
        x = {k: (int(v) if isinstance(v, bool) else v) for k, v in cfg.items()}
        if canonicalize is not None:
            x = canonicalize(x)
        x["task_id"] = instance
        x.update(fidelity)
        out = b.objective_function(x)
        return float(out[0][target]) if isinstance(out, list) else float(out[target])

    if canonicalize is not None:
        fn._canonicalize = canonicalize
    return fn, space


def _ranger_canon(cfg):
    if cfg.get("splitrule") != "extratrees":
        cfg.pop("num.random.splits", None)
    return cfg


def yahpo_rpart_41138():
    return _make("rbv2_rpart", "41138", {"trainsize": 1.0, "repl": 10})


def yahpo_rpart_40981():
    return _make("rbv2_rpart", "40981", {"trainsize": 1.0, "repl": 10})


def yahpo_ranger_1489():
    return _make("iaml_ranger", "1489", {"trainsize": 1.0},
                 canonicalize=_ranger_canon)


YAHPO_BENCH = {
    "yahpo_rpart_41138": yahpo_rpart_41138,
    "yahpo_rpart_40981": yahpo_rpart_40981,
    "yahpo_ranger_1489": yahpo_ranger_1489,
}
