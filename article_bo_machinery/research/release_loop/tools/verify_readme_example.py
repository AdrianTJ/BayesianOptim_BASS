#!/usr/bin/env python3
"""Deterministic gate for the bo-audit README's headline example.

Extracts the first python block that drives an optimizer, executes it twice,
and checks that (1) the documented output comment matches real stdout
byte-for-byte, (2) the example's excess over the pigeonhole baseline is
materially positive -- i.e. it demonstrates machinery waste rather than
unavoidable collisions -- and (3) a uniform-random control on the same space
does NOT match the sampler, so the contrast is real.
"""
import math, re, subprocess, sys, io, contextlib

README = "/home/user/BayesianOptim_BASS/bo-audit/README.md"


def pigeonhole(K, B):
    return max(0.0, B + K * math.expm1(B * math.log1p(-1.0 / K)))


def extract(py):
    blocks = re.findall(r"```python\n(.*?)```", open(README).read(), re.S)
    hits = [b for b in blocks if "run_optuna" in b or "run_hyperopt" in b]
    if not hits:
        sys.exit("FAIL: no optimizer-driving python block found in README")
    return hits[0]


def run(snippet, py):
    p = subprocess.run([py, "-c", snippet], capture_output=True, text=True)
    if p.returncode != 0:
        sys.exit(f"FAIL: snippet raised\n{p.stderr[-2000:]}")
    return p.stdout.strip()


def main():
    py = sys.argv[1]
    snip = extract(py)
    out1, out2 = run(snip, py), run(snip, py)
    print(f"run 1: {out1}")
    print(f"run 2: {out2}")
    if out1 != out2:
        sys.exit("FAIL: snippet is not deterministic across runs")

    # documented output = the last comment line inside the block
    doc = [l.strip().lstrip("#").strip()
           for l in snip.splitlines() if l.strip().startswith("#")]
    doc = [d for d in doc if "{" in d]
    if not doc:
        sys.exit("FAIL: no documented output comment ({...}) in the block")
    actual = out1.splitlines()
    if doc != actual:
        sys.exit("FAIL: documented output does not match reality\n"
                 f"  documented: {doc}\n  actual    : {actual}")
    print(f"OK: all {len(doc)} documented output line(s) match measured stdout exactly")

    # recover the space from the executed snippet to judge the demonstration
    ns = {}
    with contextlib.redirect_stdout(io.StringIO()):
        exec(compile(snip, "<readme>", "exec"), ns)
    space, summ = ns["space"], ns["audited"].summary()
    if any(s[1] != "cat" for s in space):
        print("NOTE: space has non-categorical dims; revisits are expected ~0")
    K = 1
    for s in space:
        K *= len(s[2])
    B = summ["evals"]
    pg = pigeonhole(K, B)
    excess = summ["revisits"] - pg
    print(f"K={K} combinations, budget={B}, pigeonhole={pg:.1f}, "
          f"revisits={summ['revisits']}, excess={excess:+.1f} "
          f"({excess / B * 100:+.1f}% of budget)")
    if excess < 0.05 * B:
        sys.exit("FAIL: excess over pigeonhole < 5% of budget -- this example "
                 "demonstrates unavoidable collisions, not machinery waste")
    print("OK: example demonstrates excess well above the pigeonhole baseline")

    # random control on the identical space must be clearly lower
    ctrl = (
        "from bo_audit import AuditedObjective\n"
        "from bo_audit.drivers import run_random\n"
        + snip.split("audited = AuditedObjective")[0]
        + "a = AuditedObjective(objective_fn, space)\n"
        f"run_random(a, space, {B}, 0)\nprint(a.summary()['revisits'])\n"
    )
    r = int(run(ctrl, py))
    print(f"random control revisits={r} (excess {r - pg:+.1f})")
    if summ["revisits"] - r < 0.05 * B:
        sys.exit("FAIL: sampler is not distinguishable from random on this space")
    print("OK: sampler clearly exceeds the random control")
    print("\nALL CHECKS PASS")


if __name__ == "__main__":
    main()
