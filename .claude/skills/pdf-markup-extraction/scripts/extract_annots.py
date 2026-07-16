#!/usr/bin/env python3
"""Extract real /Annot objects and the text each one marks. No vision pass needed.

Use when triage.py says A_REAL_ANNOTATIONS. Here the reviewer's words are already
text, and for text-markup subtypes the PDF itself records exactly which glyphs were
selected — so the anchor is exact rather than inferred. Rendering pages in this case
is pure waste.

Anchoring differs by subtype, and the distinction matters:

  Highlight / Underline / StrikeOut / Squiggly
      Carry /QuadPoints: one quad per line of selected text, so a selection spanning
      a line break is recorded exactly. Intersect the quads with word boxes and the
      anchor is precise.

  Text ("sticky note") / FreeText / Square / Circle / Line / Ink
      Have only a rect, placed wherever the reviewer dropped it — usually the margin,
      pointing at something nearby. There is no recorded link to any text, so the
      anchor must be inferred from position. Treat it as a guess: this script reports
      the nearest line and marks anchor_is_exact false.

  Ink
      Freehand strokes. /Contents is almost always empty — the meaning is in the
      shape, not in text. These need a vision pass; the script flags them.

Usage:
    python extract_annots.py FILE.pdf -o annots.json
"""
import argparse
import json
import sys

try:
    import pymupdf
except ImportError:
    sys.exit("pymupdf missing. Install with: pip install pymupdf")

QUAD_SUBTYPES = {"Highlight", "Underline", "StrikeOut", "Squiggly"}
NEEDS_VISION = {"Ink", "Stamp"}


def quad_anchor(annot, words):
    """Exact anchor from /QuadPoints — the glyphs the reviewer actually selected."""
    verts = annot.vertices or []
    quads = [verts[i:i + 4] for i in range(0, len(verts), 4)] if verts else []
    picked = []
    for q in quads:
        try:
            r = pymupdf.Quad(q).rect
        except Exception:
            continue
        for w in words:
            wr = pymupdf.Rect(w[:4])
            inter = wr & r
            # majority overlap avoids grabbing neighbours the quad merely grazes
            if not inter.is_empty and abs(inter) > 0.35 * abs(wr):
                picked.append((round(w[1], 1), w[0], w[4]))
    picked.sort()
    return " ".join(p[2] for p in picked)


def near_anchor(rect, words, band=14):
    """Inferred anchor for a positional annot: the text line it sits beside."""
    mid = (rect.y0 + rect.y1) / 2
    band_words = sorted((w for w in words if abs((w[1] + w[3]) / 2 - mid) < band),
                        key=lambda w: w[0])
    under = [w[4] for w in words if pymupdf.Rect(w[:4]).intersects(rect)]
    return " ".join(under), " ".join(w[4] for w in band_words)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("pdf")
    ap.add_argument("-o", "--out", default="annots.json")
    a = ap.parse_args()

    doc = pymupdf.open(a.pdf)
    items, needs_vision = [], 0

    for i, page in enumerate(doc, 1):
        words = page.get_text("words")
        for an in page.annots() or []:
            sub = an.type[1]
            info = an.info or {}
            content = (info.get("content") or "").strip()
            rect = an.rect

            if sub in QUAD_SUBTYPES:
                anchor = quad_anchor(an, words)
                exact = bool(anchor)
                line = ""
            else:
                anchor, line = near_anchor(rect, words)
                exact = False

            entry = {
                "pdf_page": i,
                "subtype": sub,
                "author": (info.get("title") or "").strip() or None,
                "created": info.get("creationDate"),
                "modified": info.get("modDate"),
                "note": content or None,
                "anchor": anchor or None,
                "anchor_on_line": line or None,
                "anchor_is_exact": exact,
                "bbox": [round(v, 1) for v in rect],
                "needs_vision_pass": sub in NEEDS_VISION and not content,
            }
            if entry["needs_vision_pass"]:
                needs_vision += 1
            items.append(entry)

    out = {
        "source_pdf": a.pdf,
        "totals": {
            "annotations": len(items),
            "with_text": sum(1 for e in items if e["note"]),
            "exact_anchors": sum(1 for e in items if e["anchor_is_exact"]),
            "needing_vision_pass": needs_vision,
        },
        "by_subtype": {s: sum(1 for e in items if e["subtype"] == s)
                       for s in sorted({e["subtype"] for e in items})},
        "by_author": {au: sum(1 for e in items if e["author"] == au)
                      for au in sorted({e["author"] for e in items if e["author"]})},
        "annotations": items,
    }
    with open(a.out, "w") as f:
        json.dump(out, f, indent=2, ensure_ascii=False)

    t = out["totals"]
    print(f"{t['annotations']} annots | {t['with_text']} carry text | "
          f"{t['exact_anchors']} exact anchors -> {a.out}")
    print(f"by subtype: {out['by_subtype']}")
    if out["by_author"]:
        print(f"by author : {out['by_author']}  <- more than one means more than one reviewer")
    if needs_vision:
        print(f"\n{needs_vision} annots are freehand/stamp with no text — render those pages "
              f"and read them:\n  python render.py {a.pdf} pages -o pages/ --pages "
              f"{','.join(str(e['pdf_page']) for e in items if e['needs_vision_pass'])[:60]}")


if __name__ == "__main__":
    main()
