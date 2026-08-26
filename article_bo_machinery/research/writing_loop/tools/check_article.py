#!/usr/bin/env python3
"""Writing-phase checks for article_bo_machinery/main.tex (WRITING_PLAN.md).

1. Citation integrity: every \\cite{...} key resolves in references.bib;
   unused bib entries are warnings.
2. TODO ratchet: the count of 'TODO' markers in main.tex must never exceed
   the recorded high-water mark (tools/article_state.json); when it drops,
   the mark is ratcheted down automatically.
3. Reference integrity: every \\ref{...} has a matching \\label{...}.
4. Environment balance: \\begin{X} counts match \\end{X}.
5. Forbidden numbers: strings listed in article_state.json's "forbidden"
   (e.g. the refuted '-0.148') must not appear in main.tex.

Exit 0 = PASS, 1 = FAIL.
"""
import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
ART = HERE.parents[2] / "main.tex"          # article_bo_machinery/main.tex
BIB = ART.parent / "references.bib"
STATE = HERE / "article_state.json"

failures, warnings = [], []
tex = ART.read_text()
state = json.loads(STATE.read_text()) if STATE.exists() else {"todo_max": 14, "forbidden": ["-0.148"]}

# 1. citations
bib_keys = set(re.findall(r"@\w+\{([^,\s]+)\s*,", BIB.read_text()))
cited = set()
for group in re.findall(r"\\cite[tp]?\{([^}]*)\}", tex):
    cited.update(k.strip() for k in group.split(","))
for k in sorted(cited - bib_keys):
    failures.append(f"\\cite{{{k}}} has no bib entry")
for k in sorted(bib_keys - cited):
    warnings.append(f"unused bib entry: {k}")

# 2. TODO ratchet
todos = len(re.findall(r"TODO", tex))
if todos > state["todo_max"]:
    failures.append(f"TODO count rose: {todos} > recorded max {state['todo_max']}")
elif todos < state["todo_max"]:
    state["todo_max"] = todos   # ratchet down

# 3. refs/labels
labels = set(re.findall(r"\\label\{([^}]*)\}", tex))
for r in set(re.findall(r"\\(?:ref|eqref)\{([^}]*)\}", tex)):
    if r not in labels:
        failures.append(f"\\ref{{{r}}} has no label")

# 4. environment balance
begins = re.findall(r"\\begin\{(\w+\*?)\}", tex)
ends = re.findall(r"\\end\{(\w+\*?)\}", tex)
for env in set(begins) | set(ends):
    if begins.count(env) != ends.count(env):
        failures.append(f"unbalanced environment: {env} "
                        f"({begins.count(env)} begin / {ends.count(env)} end)")

# 5. forbidden numbers
for s in state.get("forbidden", []):
    if s in tex:
        failures.append(f"forbidden string present: {s!r} (see CLAIMS.md)")

STATE.write_text(json.dumps(state, indent=1) + "\n")

if failures:
    print("FAIL")
    for f in failures:
        print(" -", f)
    sys.exit(1)
print(f"PASS (TODOs: {todos}, high-water {state['todo_max']}; "
      f"{len(cited)} cite keys OK; {len(warnings)} unused-bib warnings)")
for w in warnings:
    print("  warn:", w)
