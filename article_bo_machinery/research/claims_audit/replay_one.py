import sys, json, time, warnings, logging
warnings.filterwarnings("ignore"); logging.disable(logging.WARNING)
from pathlib import Path; sys.path.insert(0, str(Path(__file__).resolve().parents[3] / 'bo-audit'))
from bo_audit.benchmarks import bench_by_name
from bo_audit.core import AuditedObjective
from bo_audit import drivers
mode, bench, seed = sys.argv[1], sys.argv[2], int(sys.argv[3])
fn, space = bench_by_name(bench)
aud = AuditedObjective(fn, space)
if mode == 'ax-shipped':
    drivers.run_ax(aud, space, 80, seed)
elif mode == 'ax-nominal':
    from ax import Client, ChoiceParameterConfig
    c = Client(random_seed=seed)
    c.configure_experiment(parameters=[ChoiceParameterConfig(name=s[0], values=list(s[2]), parameter_type="int", is_ordered=False) for s in space], name=f"a{seed}")
    c.configure_optimization(objective="-obj")
    for _ in range(80):
        for idx, cfg in c.get_next_trials(max_trials=1).items():
            c.complete_trial(trial_index=idx, raw_data={"obj": aud(cfg)})
elif mode == 'optuna-gp':
    drivers.run_optuna_gp(aud, space, 80, seed)
keys=[k for k,_ in aud.calls]; seen=set(); first_dup=None; solve_at=None; dup_before_solve=0
for i,(k,v) in enumerate(aud.calls):
    if k in seen and first_dup is None: first_dup=i+1
    if k in seen and solve_at is None: dup_before_solve+=1
    seen.add(k)
    if v<1e-6 and solve_at is None: solve_at=i+1
out=aud.summary(); out.update(mode=mode,bench=bench,seed=seed,solve_at=solve_at,first_dup=first_dup,dup_before_solve=dup_before_solve)
print(json.dumps(out))
