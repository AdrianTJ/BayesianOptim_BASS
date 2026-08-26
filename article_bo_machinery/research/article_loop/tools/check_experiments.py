#!/usr/bin/env python3
"""Experiment-folder conventions (article_loop PLAN.md testing leg).

For every experiments/expNN_*/ folder:
  * DESIGN.md must exist (design-before-run rule)
  * if any results file (*.csv) exists, ANALYSIS.md must exist too
  * ANALYSIS.md must reference the ledger ("ledger" or a K-id) so re-center
    isn't skipped
  * python files must at least compile (py_compile)

Exit 0 = PASS, 1 = FAIL.
"""
import py_compile
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1] / "experiments"
failures = []

for exp in sorted(ROOT.glob("exp[0-9][0-9]_*")):
    if not exp.is_dir():
        continue
    design = exp / "DESIGN.md"
    analysis = exp / "ANALYSIS.md"
    if not design.exists():
        failures.append(f"{exp.name}: missing DESIGN.md (design-before-run rule)")
    if any(exp.glob("*.csv")) and not analysis.exists():
        failures.append(f"{exp.name}: results present but no ANALYSIS.md")
    if analysis.exists():
        text = analysis.read_text()
        if not re.search(r"ledger|K\d", text):
            failures.append(f"{exp.name}: ANALYSIS.md never touches the claim ledger")

for py in ROOT.rglob("*.py"):
    try:
        py_compile.compile(str(py), doraise=True)
    except py_compile.PyCompileError as e:
        failures.append(f"py_compile: {py.name}: {e.msg}")

if failures:
    print("FAIL")
    for f in failures:
        print(" -", f)
    sys.exit(1)
n_exp = len(list(ROOT.glob("exp[0-9][0-9]_*")))
print(f"PASS ({n_exp} experiment folder(s) checked)")
