"""H6 benchmark families (G0). All vendored, seeded, deterministic.

Registry G_BENCH maps name -> builder() -> (fn(config_dict)->float, space).
bench_by_name in benchmarks.py dispatches here for unknown names, so every
run script and the smac/optuna36 venv runners get these for free.

Families and instance seeds are FIXED here (committed before any
measurement, per the loop's pre-registration rule):

- catf_*: categorized classical functions. Levels map to a fixed value
  grid through a per-dimension permutation seeded per instance, so no
  library can exploit ordinal level structure; grids are chosen so the
  analytic optimum is on-grid where one exists. Minimization; optimum 0
  for rastrigin/rosen/griewank/ackley-family (schwefel/michalewicz get
  exhaustive ground truth from g0_gates.py instead).
- nk_*: Kauffman NK landscapes, N=20 binary loci, K in {2, 8} random
  neighbors, per-locus contribution tables ~ U(0,1); f = -mean
  contribution (minimize). Instance fully determined by NK_SEED.
- maxcut_n20: weighted Max-Cut on G(20, 0.5) with U(0,1) weights;
  f = -cut weight (minimize). Instance seeded.
- labs_n25: low-autocorrelation binary sequence, n=25; f = E(s)/n^2
  (normalized sidelobe energy; minimize).
- contam_2p25: contamination control (Hu et al. family, as adopted by
  BOCS/COMBO), 25 binary prevention stages, 100 Monte-Carlo scenarios
  from a LOCAL RandomState(0) per call -> deterministic, same
  determinization convention as pest_control (disclosed).
- ml_*: real hyperparameter tuning on sklearn built-in datasets (real
  ML, no egress). Deterministic: fixed StratifiedKFold splits and fixed
  estimator random_state. f = 1 - mean CV accuracy.
"""
import numpy as np

CATF_PERM_SEED = 7101   # per-instance permutation stream
NK_SEED = 7201
MAXCUT_SEED = 7301


# ---------- categorized classical functions ---------------------------------

def _catf(name, d, grid, core):
    """Build a categorized function: level j (1..L) in dim i maps to
    grid[perm_i[j-1]]; core(Z) evaluates rows of mapped values."""
    L = len(grid)
    grid = np.asarray(grid, dtype=float)
    perms = [np.random.default_rng(CATF_PERM_SEED * 100 + hash(name) % 97 + i)
             .permutation(L) for i in range(d)]
    space = [(f"c{i}", "cat", list(range(1, L + 1))) for i in range(d)]

    def fn(cfg):
        z = np.array([grid[perms[i][cfg[f"c{i}"] - 1]] for i in range(d)])
        return float(core(z))
    return fn, space


def catf_rastrigin_d4L7():
    grid = np.linspace(-5.12, 5.12, 7)          # includes 0 (L odd)
    return _catf("rastrigin", 4, grid,
                 lambda z: 10 * z.size + np.sum(z**2 - 10 * np.cos(2 * np.pi * z)))


def catf_rosen_d4L7():
    grid = [-2.0, -1.0, -0.5, 0.0, 0.5, 1.0, 2.0]   # includes 1 -> opt 0
    def core(z):
        return float(np.sum(100.0 * (z[1:] - z[:-1]**2)**2 + (1.0 - z[:-1])**2))
    return _catf("rosen", 4, grid, core)


def catf_michal_d5L9():
    grid = np.linspace(0.15, np.pi - 0.05, 9)
    m = 10
    def core(z):
        i = np.arange(1, z.size + 1)
        return float(-np.sum(np.sin(z) * np.sin(i * z**2 / np.pi) ** (2 * m)))
    return _catf("michal", 5, grid, core)


def catf_griewank_d5L7():
    grid = np.linspace(-600, 600, 7)             # includes 0
    def core(z):
        i = np.arange(1, z.size + 1)
        return float(np.sum(z**2) / 4000.0 - np.prod(np.cos(z / np.sqrt(i))) + 1.0)
    return _catf("griewank", 5, grid, core)


def catf_schwefel_d4L9():
    grid = [-500.0, -300.0, -100.0, 0.0, 100.0, 250.0, 350.0, 420.9687, 500.0]
    def core(z):
        return float(418.9829 * z.size - np.sum(z * np.sin(np.sqrt(np.abs(z)))))
    return _catf("schwefel", 4, grid, core)


# ---------- NK landscapes ----------------------------------------------------

def _nk(N, K, seed):
    rng = np.random.default_rng(seed)
    neighbors = [np.sort(rng.choice([j for j in range(N) if j != i], K, replace=False))
                 for i in range(N)]
    tables = [rng.random(2 ** (K + 1)) for _ in range(N)]
    space = [(f"b{i}", "cat", [0, 1]) for i in range(N)]

    def fn(cfg):
        x = np.array([cfg[f"b{i}"] for i in range(N)], dtype=int)
        total = 0.0
        for i in range(N):
            idx = x[i]
            for j, nb in enumerate(neighbors[i]):
                idx |= x[nb] << (j + 1)
            total += tables[i][idx]
        return float(-total / N)          # minimize negative mean fitness
    # expose instance internals for the vectorized exhaustive gate
    fn._nk = (N, K, neighbors, tables)
    return fn, space


def nk_n20k2():
    return _nk(20, 2, NK_SEED)


def nk_n20k8():
    return _nk(20, 8, NK_SEED + 1)


# ---------- weighted Max-Cut -------------------------------------------------

def maxcut_n20():
    n = 20
    rng = np.random.default_rng(MAXCUT_SEED)
    W = np.triu((rng.random((n, n)) < 0.5) * rng.random((n, n)), 1)
    W = W + W.T
    space = [(f"b{i}", "cat", [0, 1]) for i in range(n)]

    def fn(cfg):
        x = np.array([cfg[f"b{i}"] for i in range(n)], dtype=int)
        return float(-np.sum(W * (x[:, None] != x[None, :])) / 2.0)
    fn._W = W
    return fn, space


# ---------- LABS -------------------------------------------------------------

def labs_n25():
    n = 25
    space = [(f"b{i}", "cat", [0, 1]) for i in range(n)]

    def fn(cfg):
        s = np.array([2 * cfg[f"b{i}"] - 1 for i in range(n)], dtype=float)
        e = sum(float(np.dot(s[:-k], s[k:])) ** 2 for k in range(1, n))
        return e / n ** 2
    return fn, space


# ---------- contamination control -------------------------------------------

def contam_2p25():
    """25-stage contamination control (BOCS/COMBO family), determinized:
    the 100 MC scenarios come from a local RandomState(0) each call."""
    n_stages, n_sim = 25, 100
    cost_per_action = 1.0
    penalty = 1.0
    upper = 0.1          # contamination threshold
    space = [(f"b{i}", "cat", [0, 1]) for i in range(n_stages)]

    def fn(cfg):
        x = np.array([cfg[f"b{i}"] for i in range(n_stages)], dtype=int)
        rs = np.random.RandomState(0)
        z = rs.beta(1.0, 30.0, size=n_sim)             # initial contamination
        alpha = rs.beta(1.0, 17.0 / 3.0, size=(n_stages, n_sim))  # spread
        beta = rs.beta(1.0, 7.0 / 3.0, size=(n_stages, n_sim))    # restore
        above = 0.0
        for i in range(n_stages):
            if x[i]:
                z = beta[i] * (1 - z) * 0 + (1 - beta[i]) * z   # prevention
            else:
                z = alpha[i] * (1 - z) + z
            z = np.clip(z, 0.0, 1.0)
            above += np.mean(z > upper)
        return float(cost_per_action * x.sum() / n_stages + penalty * above / n_stages)
    return fn, space


# ---------- real ML: sklearn built-ins --------------------------------------

def _cv(est, X, y):
    from sklearn.model_selection import StratifiedKFold, cross_val_score
    cv = StratifiedKFold(n_splits=3, shuffle=True, random_state=11)
    return 1.0 - float(cross_val_score(est, X, y, cv=cv, n_jobs=1).mean())


def ml_rf_digits():
    from sklearn.datasets import load_digits
    from sklearn.ensemble import RandomForestClassifier
    X, y = load_digits(return_X_y=True)
    space = [("max_features", "cat", ["sqrt", "log2", "0.2", "0.5"]),
             ("criterion", "cat", ["gini", "entropy"]),
             ("min_samples_split", "int", 2, 20),
             ("max_depth", "int", 3, 30)]

    def fn(cfg):
        mf = cfg["max_features"]
        mf = float(mf) if mf.replace(".", "").isdigit() else mf
        est = RandomForestClassifier(
            n_estimators=100, max_features=mf, criterion=cfg["criterion"],
            min_samples_split=int(cfg["min_samples_split"]),
            max_depth=int(cfg["max_depth"]), random_state=17, n_jobs=1)
        return _cv(est, X, y)
    return fn, space


def ml_svm_digits():
    from sklearn.datasets import load_digits
    from sklearn.decomposition import PCA
    from sklearn.pipeline import make_pipeline
    from sklearn.preprocessing import StandardScaler
    from sklearn.svm import SVC
    X, y = load_digits(return_X_y=True)
    space = [("prep", "cat", ["none", "standardize", "pca16", "pca32"]),
             ("logC", "float", -3.0, 3.0),
             ("logGamma", "float", -5.0, 1.0)]

    def fn(cfg):
        svc = SVC(C=10 ** float(cfg["logC"]), gamma=10 ** float(cfg["logGamma"]),
                  kernel="rbf", random_state=17)
        prep = cfg["prep"]
        if prep == "standardize":
            est = make_pipeline(StandardScaler(), svc)
        elif prep.startswith("pca"):
            est = make_pipeline(PCA(n_components=int(prep[3:]), random_state=17), svc)
        else:
            est = svc
        return _cv(est, X, y)
    return fn, space


def ml_gb_bc():
    from sklearn.datasets import load_breast_cancer
    from sklearn.ensemble import GradientBoostingClassifier
    X, y = load_breast_cancer(return_X_y=True)
    space = [("max_features", "cat", ["sqrt", "log2", "1.0"]),
             ("logLR", "float", -3.0, 0.0),
             ("subsample", "float", 0.5, 1.0),
             ("max_depth", "int", 1, 6)]

    def fn(cfg):
        mf = cfg["max_features"]
        mf = float(mf) if mf.replace(".", "").isdigit() else mf
        est = GradientBoostingClassifier(
            n_estimators=50, learning_rate=10 ** float(cfg["logLR"]),
            subsample=float(cfg["subsample"]), max_depth=int(cfg["max_depth"]),
            max_features=mf, random_state=17)
        return _cv(est, X, y)
    return fn, space


def ml_mlp_wine():
    from sklearn.datasets import load_wine
    from sklearn.neural_network import MLPClassifier
    from sklearn.pipeline import make_pipeline
    from sklearn.preprocessing import StandardScaler
    X, y = load_wine(return_X_y=True)
    space = [("arch", "cat", ["16", "64", "16-16", "64-32"]),
             ("activation", "cat", ["relu", "tanh"]),
             ("logAlpha", "float", -6.0, -1.0),
             ("logLR", "float", -4.0, -1.0)]

    def fn(cfg):
        hidden = tuple(int(h) for h in cfg["arch"].split("-"))
        est = make_pipeline(StandardScaler(), MLPClassifier(
            hidden_layer_sizes=hidden, activation=cfg["activation"],
            alpha=10 ** float(cfg["logAlpha"]),
            learning_rate_init=10 ** float(cfg["logLR"]),
            max_iter=300, random_state=17))
        import warnings
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            return _cv(est, X, y)
    return fn, space


G_BENCH = {
    "catf_rastrigin_d4L7": catf_rastrigin_d4L7,
    "catf_rosen_d4L7": catf_rosen_d4L7,
    "catf_michal_d5L9": catf_michal_d5L9,
    "catf_griewank_d5L7": catf_griewank_d5L7,
    "catf_schwefel_d4L9": catf_schwefel_d4L9,
    "nk_n20k2": nk_n20k2,
    "nk_n20k8": nk_n20k8,
    "maxcut_n20": maxcut_n20,
    "labs_n25": labs_n25,
    "contam_2p25": contam_2p25,
    "ml_rf_digits": ml_rf_digits,
    "ml_svm_digits": ml_svm_digits,
    "ml_gb_bc": ml_gb_bc,
    "ml_mlp_wine": ml_mlp_wine,
}
