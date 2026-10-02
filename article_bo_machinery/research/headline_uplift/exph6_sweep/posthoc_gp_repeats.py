import sys, collections, json, logging, warnings
warnings.filterwarnings("ignore"); logging.disable(logging.WARNING)
sys.path.insert(0, ".")
sys.path.insert(0, "../article_loop/experiments")
from bo_audit.benchmarks_g import ml_gb_bc
from bo_audit.core import AuditedObjective
from bo_audit.drivers import run_optuna_gp
fn, space = ml_gb_bc()
for seed in map(int, sys.argv[1:]):
    log = []
    a = AuditedObjective(lambda c: (log.append(dict(c)), fn(c))[1], space)
    run_optuna_gp(a, space, 80, seed)
    keys = [a.key_of(c) for c in log]
    cnt = collections.Counter(keys)
    reps = {k: v for k, v in cnt.items() if v > 1}
    first = {}
    for i, k in enumerate(keys): first.setdefault(k, i)
    print(json.dumps({"seed": seed, "revisits": a.summary()["revisits"],
          "repeated": [(k, v, "first@%d" % first[k]) for k, v in reps.items()]}))
