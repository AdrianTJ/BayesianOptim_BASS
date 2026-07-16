#!/usr/bin/env python3
"""Self-test: builds synthetic PDFs and checks the pipeline routes and anchors them right.

Run this if the scripts behave oddly, or after changing them. It is fast and needs no
fixtures on disk — it generates its own.

The routing check exists because of a real bug worth remembering: get_drawings()
returns annotation appearance streams as vector paths, so a PDF full of ordinary
highlights looked exactly like flattened ink and got routed to the expensive vision
pipeline. Cheap to reintroduce, hard to notice — the output still looks plausible,
it just costs a vision pass and infers anchors it could have read exactly.

Usage:
    python selftest.py
"""
import json
import os
import subprocess
import sys
import tempfile

try:
    import pymupdf
except ImportError:
    sys.exit("pymupdf missing. Install with: pip install pymupdf")

HERE = os.path.dirname(os.path.abspath(__file__))
PY = sys.executable
RESULTS = []


def check(name, ok, detail=""):
    RESULTS.append((name, ok, detail))
    print(f"  {'PASS' if ok else 'FAIL'}  {name}" + (f"   [{detail}]" if detail and not ok else ""))


def run(script, *args):
    r = subprocess.run([PY, os.path.join(HERE, script), *args],
                       capture_output=True, text=True)
    return r.stdout + r.stderr


def make_annotated(path):
    d = pymupdf.open()
    p = d.new_page()
    p.insert_text((72, 100), "The quick brown fox jumps over the lazy dog.", fontsize=12)
    p.insert_text((72, 130), "Ergodicity is an incredibly powerful property of chains.", fontsize=12)
    p.insert_text((72, 160), "This sentence contains a speling error to be fixed.", fontsize=12)
    h = p.add_highlight_annot(p.search_for("incredibly powerful")[0])
    h.set_info(content="vague - quantify", title="Reviewer A"); h.update()
    s = p.add_strikeout_annot(p.search_for("speling")[0])
    s.set_info(content="spelling", title="Reviewer A"); s.update()
    t = p.add_text_annot((520, 128), "restructure")
    t.set_info(content="restructure this para", title="Reviewer B"); t.update()
    p.add_ink_annot([[(300, 200), (320, 210), (340, 195)]]).update()
    d.save(path)


def make_flattened(path):
    """Ink drawn straight onto the page — no annots. Mimics Apple Markup output."""
    d = pymupdf.open()
    p = d.new_page()
    p.insert_text((72, 100), "The quick brown fox jumps over the lazy dog.", fontsize=12)
    p.insert_text((72, 130), "Ergodicity is an incredibly powerful property of chains.", fontsize=12)
    red = (1.0, 0.25, 0.09)
    r = p.search_for("incredibly powerful")[0]
    p.draw_line((r.x0, r.y1 + 1), (r.x1, r.y1 + 1), color=red, width=1.5)   # underline
    for i in range(6):                                                       # margin scribble
        p.draw_line((430 + i * 4, 120 + (i % 2) * 6), (434 + i * 4, 128 - (i % 2) * 6),
                    color=red, width=1.4)
    d.save(path)


def main():
    tmp = tempfile.mkdtemp(prefix="pdfmarkup-selftest-")
    ann = os.path.join(tmp, "annotated.pdf")
    flat = os.path.join(tmp, "flattened.pdf")
    make_annotated(ann)
    make_flattened(flat)

    print("\nrouting")
    check("annotated PDF routes to A_REAL_ANNOTATIONS (not the vision pipeline)",
          "A_REAL_ANNOTATIONS" in run("triage.py", ann))
    check("flattened-ink PDF routes to B_FLATTENED_INK",
          "B_FLATTENED_INK" in run("triage.py", flat))

    print("\nannotation extraction (pipeline A)")
    aj = os.path.join(tmp, "a.json")
    run("extract_annots.py", ann, "-o", aj)
    a = json.load(open(aj))
    by = {e["subtype"]: e for e in a["annotations"]}
    check("highlight anchors exactly to the selected words",
          by.get("Highlight", {}).get("anchor") == "incredibly powerful",
          repr(by.get("Highlight", {}).get("anchor")))
    check("strikeout anchors exactly to the struck word",
          by.get("StrikeOut", {}).get("anchor") == "speling",
          repr(by.get("StrikeOut", {}).get("anchor")))
    check("reviewer's text is captured verbatim",
          by.get("Highlight", {}).get("note") == "vague - quantify")
    check("positional note is not passed off as an exact anchor",
          by.get("Text", {}).get("anchor_is_exact") is False)
    check("freehand ink is flagged for a vision pass",
          by.get("Ink", {}).get("needs_vision_pass") is True)
    check("both reviewers are distinguished", len(a["by_author"]) == 2)

    print("\nmark extraction (pipeline B)")
    mj = os.path.join(tmp, "m.json")
    run("extract_marks.py", flat, "-o", mj)
    m = json.load(open(mj))
    marks = m["pages"][0]["marks"] if m["pages"] else []
    check("ink colour auto-detected", m["ink_rgb"][0] > 0.8 and m["ink_rgb"][2] < 0.3,
          str(m["ink_rgb"]))
    check("strokes cluster into separate marks (underline + margin note)",
          len(marks) >= 2, f"{len(marks)} marks")
    check("underline anchors to the text beneath it",
          any("incredibly" in (k["text_under_mark"] or "") for k in marks))
    check("margin note is identified as marginal",
          any(k["in_margin"] for k in marks))

    print("\nrender guards")
    out = run("render.py", flat, "crop", "-o", tmp, "--page", "1", "--box", "330,1690,700,1760")
    check("pixel coords rejected with a fix-it message",
          "does not intersect" in out and "PIXEL" in out)
    check("valid point coords render",
          "p001_" in run("render.py", flat, "crop", "-o", tmp, "--page", "1",
                         "--box", "72,90,300,110"))

    failed = [r for r in RESULTS if not r[1]]
    print(f"\n{len(RESULTS) - len(failed)}/{len(RESULTS)} passed")
    if failed:
        print("FAILURES:")
        for n, _, d in failed:
            print(f"  - {n} {d}")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
