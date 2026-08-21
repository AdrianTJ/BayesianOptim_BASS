#!/usr/bin/env python3
"""Testing leg for the research loop (PLAN.md section 3).

Validates the conventions of article_bo_machinery/research/:
  * every .json file parses
  * every relative markdown link inside research/ resolves to a real file
  * every loop_engineering/notes/*.md has a "What this changes for us" section
  * loop_engineering/LOG.md entries are well-formed ("## Cycle N — date")
  * every .js file under research/ passes `node --check` (if node exists)

Exit code 0 = PASS, 1 = FAIL (failures listed on stdout).
"""
import json
import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]  # article_bo_machinery/research/
failures = []


def fail(msg):
    failures.append(msg)


# 1. JSON validity
for p in ROOT.rglob("*.json"):
    try:
        json.loads(p.read_text())
    except Exception as e:
        fail(f"invalid JSON: {p.relative_to(ROOT)}: {e}")

# 2. Relative markdown links resolve
link_re = re.compile(r"\[[^\]]*\]\(([^)\s]+)\)")
for p in ROOT.rglob("*.md"):
    for target in link_re.findall(p.read_text()):
        if target.startswith(("http://", "https://", "#", "mailto:")):
            continue
        t = target.split("#", 1)[0]
        if t and not (p.parent / t).exists():
            fail(f"broken link in {p.relative_to(ROOT)}: {target}")

# 3. Notes carry the required section
notes = ROOT / "loop_engineering" / "notes"
if notes.is_dir():
    for p in sorted(notes.glob("*.md")):
        if "What this changes for us" not in p.read_text():
            fail(f"note missing 'What this changes for us' section: {p.name}")

# 4. LOG.md entry shape
log = ROOT / "loop_engineering" / "LOG.md"
if log.exists():
    entries = re.findall(r"^## .*$", log.read_text(), flags=re.M)
    bad = [e for e in entries if not re.match(r"^## Cycle \d+ — \d{4}-\d{2}-\d{2}$", e)]
    for e in bad:
        fail(f"malformed LOG.md heading: {e!r}")
else:
    fail("loop_engineering/LOG.md is missing")

# 5. JS syntax
node = shutil.which("node")
if node:
    for p in ROOT.rglob("*.js"):
        r = subprocess.run([node, "--check", str(p)], capture_output=True, text=True)
        if r.returncode != 0:
            fail(f"node --check failed: {p.relative_to(ROOT)}: {r.stderr.strip()[:200]}")

if failures:
    print("FAIL")
    for f in failures:
        print(" -", f)
    sys.exit(1)
print(f"PASS ({sum(1 for _ in ROOT.rglob('*.md'))} md, "
      f"{sum(1 for _ in ROOT.rglob('*.json'))} json files checked)")
