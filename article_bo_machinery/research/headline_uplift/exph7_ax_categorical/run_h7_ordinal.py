#!/usr/bin/env python3
"""H7 Amendment 1: same-environment ordinal control arm.

Identical to run_h7.py except that every Ax categorical is forced to
is_ordered=True (Ax's default for integer choices, as H1 ran it), by
wrapping ax.ChoiceParameterConfig inside the cell. Resumable.

Usage: run_h7_ordinal.py [python-with-ax]
"""
import run_h7

run_h7.RESULTS = run_h7.HERE / "results_ordinal.jsonl"
run_h7.FAILURES = run_h7.HERE / "failures_ordinal.log"
run_h7.CELL = run_h7.CELL.replace(
    "from bo_audit.drivers import run_ax\n",
    "from bo_audit.drivers import run_ax\n"
    "import ax\n"
    "_CPC = ax.ChoiceParameterConfig\n"
    "def _ordinal(*a, **k):\n"
    "    k['is_ordered'] = True\n"
    "    return _CPC(*a, **k)\n"
    "ax.ChoiceParameterConfig = _ordinal\n",
).replace(
    "non_defaults=cfg[\"non_defaults\"]",
    "non_defaults=cfg[\"non_defaults\"].replace('is_ordered=False', "
    "'is_ordered=True') + ' (H7 ordinal control)'",
)
assert "_ordinal" in run_h7.CELL and "ordinal control" in run_h7.CELL

if __name__ == "__main__":
    run_h7.main()
