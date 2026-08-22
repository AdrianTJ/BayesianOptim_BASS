"""bo-audit benchmarks that need vendoring (no pip package / no egress).

PestControl: vendored from QUVA-Lab/COMBO
(COMBO/experiments/test_functions/multiple_categorical.py, MIT-licensed,
fetched 2026-08-21), the standard 25-stage / 5-choice (5^25) categorical
benchmark. Two deliberate adaptations, both disclosed in H1's DESIGN.md:

1. torch dependency removed (pure numpy; the original only used torch for
   tensor plumbing around the identical numpy score function);
2. the objective is made DETERMINISTIC: the original draws its 100
   Monte-Carlo scenarios from the *global* numpy RNG on every call, so
   re-evaluating a configuration returns a different value — under which
   revisits could be defensible noise-averaging rather than waste. We fix
   the scenario stream with a local RandomState(0) per call (the same
   determinization used by later work that adopted this benchmark, e.g.
   BODi/Bounce-style evaluations), so f is a function and every revisit is
   unambiguously wasted budget. The simulation dynamics, priors, prices,
   tolerance-development updates, and score are otherwise verbatim.
"""
import numpy as np

PESTCONTROL_N_CHOICE = 5
PESTCONTROL_N_STAGES = 25


def _pest_spread(curr_pest_frac, spread_rate, control_rate, apply_control):
    if apply_control:
        return (1.0 - control_rate) * curr_pest_frac
    return spread_rate * (1 - curr_pest_frac) + curr_pest_frac


def _pest_control_score(x, rng):
    U = 0.1
    n_stages = x.size
    n_simulations = 100

    init_pest_frac_alpha = 1.0
    init_pest_frac_beta = 30.0
    spread_alpha = 1.0
    spread_beta = 17.0 / 3.0

    control_alpha = 1.0
    control_price_max_discount = {1: 0.2, 2: 0.3, 3: 0.3, 4: 0.0}
    tolerance_develop_rate = {1: 1.0 / 7.0, 2: 2.5 / 7.0, 3: 2.0 / 7.0, 4: 0.5 / 7.0}
    control_price = {1: 1.0, 2: 0.8, 3: 0.7, 4: 0.5}
    # below two changes over stages according to x
    control_beta = {1: 2.0 / 7.0, 2: 3.0 / 7.0, 3: 3.0 / 7.0, 4: 5.0 / 7.0}

    payed_price_sum = 0
    above_threshold = 0

    init_pest_frac = rng.beta(init_pest_frac_alpha, init_pest_frac_beta, size=(n_simulations,))
    curr_pest_frac = init_pest_frac
    for i in range(n_stages):
        spread_rate = rng.beta(spread_alpha, spread_beta, size=(n_simulations,))
        do_control = x[i] > 0
        if do_control:
            control_rate = rng.beta(control_alpha, control_beta[x[i]], size=(n_simulations,))
            next_pest_frac = _pest_spread(curr_pest_frac, spread_rate, control_rate, True)
            # tolerance has been developed for pesticide type x[i]
            control_beta[x[i]] += tolerance_develop_rate[x[i]] / float(n_stages)
            # you will get discount
            payed_price = control_price[x[i]] * (
                1.0 - control_price_max_discount[x[i]] / float(n_stages) * float(np.sum(x == x[i])))
        else:
            next_pest_frac = _pest_spread(curr_pest_frac, spread_rate, 0, False)
            payed_price = 0
        payed_price_sum += payed_price
        above_threshold += np.mean(curr_pest_frac > U)
        curr_pest_frac = next_pest_frac

    return payed_price_sum + above_threshold


def pest_control(x, scenario_seed=0):
    """Deterministic pest-control objective.

    x: length-25 sequence of ints in {0,..,4} (0 = no pesticide).
    Returns price paid + fraction of stages above the pest threshold
    (minimize). Same x -> same value (local RandomState(scenario_seed)).
    """
    x = np.asarray(x, dtype=int)
    assert x.size == PESTCONTROL_N_STAGES and x.min() >= 0 and x.max() < PESTCONTROL_N_CHOICE
    return float(_pest_control_score(x, np.random.RandomState(scenario_seed)))


def pest_space():
    """bo-audit space schema for pest control."""
    return [(f"s{j}", "cat", [0, 1, 2, 3, 4]) for j in range(PESTCONTROL_N_STAGES)]


def pest_cfg_objective(cfg):
    return pest_control([cfg[f"s{j}"] for j in range(PESTCONTROL_N_STAGES)])


def bench_by_name(name):
    """(fn(config_dict)->float, bo-audit space) for every H1 benchmark.

    Shared by the main-env run script and the venv-isolated smac_runner so
    the benchmark adapter cannot diverge between libraries. machinery.py
    (article_loop/experiments) must be importable for non-pest benchmarks;
    its objectives take unit-cube rows: cat coordinate j at level v (1..L)
    encodes to (v-0.5)/L, continuous coordinates pass through [0,1].
    """
    if name == "pest_control":
        return pest_cfg_objective, pest_space()

    from bo_audit.benchmarks_g import G_BENCH
    if name in G_BENCH:
        return G_BENCH[name]()

    if name.startswith("cat_ackley"):
        from machinery import make_cat_ackley
        bits = name.split("_")  # cat_ackley_d{d}_L{L}
        d, L = int(bits[2][1:]), int(bits[3][1:])
        obj = make_cat_ackley(d, L)
    else:
        from machinery import OBJECTIVES
        obj = OBJECTIVES[name]()
        d = obj["d"]

    schema = obj["schema"]
    space, names = [], []
    n_cat = n_cont = 0
    for j, t in enumerate(schema.types):
        if t == "cat":
            space.append((f"c{n_cat}", "cat", list(range(1, schema.levels[j] + 1))))
            n_cat += 1
        else:
            space.append((f"z{n_cont}", "float", 0.0, 1.0))
            n_cont += 1

    def fn(cfg, _obj=obj, _schema=schema):
        u, ic, iz = [], 0, 0
        for j, t in enumerate(_schema.types):
            if t == "cat":
                u.append((cfg[f"c{ic}"] - 0.5) / _schema.levels[j])
                ic += 1
            else:
                u.append(float(cfg[f"z{iz}"]))
                iz += 1
        return float(_obj["fn"](np.array([u]))[0])

    return fn, space
