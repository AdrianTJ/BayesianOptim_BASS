#!/usr/bin/env python3
"""G5: scripted duplicate injection on a MIXED cat+float space (the
true-positive gate H1's review found missing from H0). Pre-registered
expectations in DESIGN.md Q3: revisits=4, unique=6 from this exact script.
Must pass before the full H2 run launches."""
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
from bo_audit.core import AuditedObjective

space = [("c", "cat", [1, 2]), ("z", "float", 0.0, 1.0)]
a = AuditedObjective(lambda cfg: 0.0, space)

calls = [
    (1, 0.5),          # new u1
    (1, 0.5),          # true duplicate 1
    (2, 0.5),          # new u2
    (1, 0.5),          # true duplicate 2
    (2, 0.5),          # true duplicate 3
    (1, 0.501),        # near-miss 1e-3: must NOT count -> new u3
    (2, 0.3),          # new u4
    (2, 0.3 + 1e-9),   # within 6-decimal rounding: MUST count -> dup 4
    (1, 0.7),          # new u5
    (2, 0.499),        # near-miss vs 0.5: new u6
]
for c, z in calls:
    a({"c": c, "z": z})

ok = a.n_revisits == 4 and len(a.seen) == 6
print(f"[{'PASS' if ok else 'FAIL'}] G5 mixed-space injection: "
      f"revisits {a.n_revisits} (want 4), unique {len(a.seen)} (want 6)")
sys.exit(0 if ok else 1)
