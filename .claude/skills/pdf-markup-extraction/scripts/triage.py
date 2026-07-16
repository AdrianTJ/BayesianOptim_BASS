#!/usr/bin/env python3
"""Classify a marked-up PDF and recommend an extraction pipeline.

Run this FIRST, before opening the PDF in a viewer or reading any pages.
Marked-up PDFs come in three shapes that need completely different handling,
and they are indistinguishable by eye. Guessing wastes an expensive vision pass.

Usage:
    python triage.py FILE.pdf [--json]
"""
import argparse
import json
import sys
from collections import Counter, defaultdict

try:
    import pymupdf
except ImportError:
    sys.exit("pymupdf missing. Install with: pip install pymupdf")


def is_ink(rgb, sat=0.22, dark=0.30):
    """True if a colour reads as deliberate ink rather than body text or rules.

    Reviewers pick a colour that stands out from black type, so we look for
    channel spread (saturation) and enough brightness to not be near-black.
    """
    if not rgb or len(rgb) != 3:
        return False
    r, g, b = rgb
    return (max(rgb) - min(rgb)) > sat and max(rgb) > dark


def key(rgb, places=1):
    return tuple(round(c, places) for c in rgb)


def annot_rects(page):
    return [pymupdf.Rect(a.rect) for a in (page.annots() or [])]


def belongs_to_annot(rect, boxes, frac=0.5):
    """True if a drawing is an annotation's own appearance rather than loose ink.

    get_drawings() returns the vector content of annotation appearance streams too —
    a highlight IS a coloured rectangle. Counting those as flattened ink misroutes a
    perfectly readable annotated PDF into the expensive vision pipeline, so discount
    anything sitting inside an annotation's rect.
    """
    if not boxes:
        return False
    area = abs(rect)
    if area <= 0:
        return any(rect in b for b in boxes)
    return any(abs(rect & b) > frac * area for b in boxes)


def analyse(path):
    doc = pymupdf.open(path)
    out = {
        "file": path,
        "pages": len(doc),
        "producer": doc.metadata.get("producer"),
        "creator": doc.metadata.get("creator"),
        "created": doc.metadata.get("creationDate"),
        "encrypted": doc.is_encrypted,
    }

    annots = Counter()
    annot_pages = set()
    ink_colours = Counter()
    ink_pages = defaultdict(int)
    text_colours = Counter()
    text_colour_pages = defaultdict(set)
    text_chars = 0
    raster_pages = 0
    big_images = 0

    for i, page in enumerate(doc, 1):
        for a in page.annots():
            annots[a.type[1]] += 1
            annot_pages.add(i)

        txt = page.get_text().strip()
        text_chars += len(txt)

        # a page that is one big image with no text is a scan
        imgs = page.get_images(full=True)
        page_area = abs(page.rect)
        covering = False
        for im in imgs:
            big_images += 1
            try:
                for r in page.get_image_rects(im[0]):
                    if abs(r) > 0.6 * page_area:
                        covering = True
            except Exception:
                pass
        if covering and len(txt) < 60:
            raster_pages += 1

        abox = annot_rects(page)
        for d in page.get_drawings():
            if belongs_to_annot(pymupdf.Rect(d["rect"]), abox):
                continue
            for attr in ("color", "stroke", "fill"):
                c = d.get(attr)
                if is_ink(c):
                    ink_colours[key(c)] += 1
                    ink_pages[key(c)] += 1
                    break

        for blk in page.get_text("dict")["blocks"]:
            for line in blk.get("lines", []):
                for span in line["spans"]:
                    if not span["text"].strip():
                        continue
                    c = span["color"]
                    rgb = (((c >> 16) & 255) / 255, ((c >> 8) & 255) / 255, (c & 255) / 255)
                    if is_ink(rgb):
                        text_colours[key(rgb)] += 1
                        text_colour_pages[key(rgb)].add(i)

    out["annotations"] = {
        "total": sum(annots.values()),
        "by_subtype": dict(annots),
        "on_pages": sorted(annot_pages)[:40],
    }
    out["ink_vector_strokes"] = {
        "total": sum(ink_colours.values()),
        "by_colour": [
            {"rgb": list(c), "strokes": n, "hex": "#%02x%02x%02x" % tuple(int(v * 255) for v in c)}
            for c, n in ink_colours.most_common(6)
        ],
    }
    out["coloured_typeset_text"] = {
        "total_spans": sum(text_colours.values()),
        "by_colour": [
            {"rgb": list(c), "spans": n, "pages": sorted(text_colour_pages[c])[:20]}
            for c, n in text_colours.most_common(4)
        ],
    }
    out["text_layer"] = {
        "total_chars": text_chars,
        "chars_per_page": round(text_chars / max(1, len(doc)), 1),
        "has_real_text": text_chars > 200 * len(doc) * 0.2,
    }
    out["raster"] = {"full_page_image_pages": raster_pages, "total_images": big_images}

    # ---- decide the pipeline ----
    has_annots = out["annotations"]["total"] > 0
    has_ink = out["ink_vector_strokes"]["total"] > 0
    has_text = out["text_layer"]["has_real_text"]
    scanned = raster_pages > 0.5 * len(doc)

    if scanned and not has_text:
        pipeline, why = "C_SCANNED", (
            "Pages are full-page images with no text layer. Both the markup AND the "
            "underlying document must be read visually. No anchoring is possible — "
            "you cannot pair a mark to text that has no coordinates. Expect the lowest "
            "confidence of the three cases and say so in the output."
        )
    elif has_annots and not has_ink:
        pipeline, why = "A_REAL_ANNOTATIONS", (
            "Real /Annot objects are present. Read annot.info['content'] and annot.rect "
            "directly — for Highlight/Underline/StrikeOut subtypes the /QuadPoints already "
            "give you the exact marked text. This is the cheap case: NO vision pass needed "
            "for typed notes. Only fall back to rendering for /Ink subtypes, which are "
            "freehand strokes with no text."
        )
    elif has_ink and has_text:
        pipeline, why = "B_FLATTENED_INK", (
            "Coloured vector strokes with no (or few) annotation objects, over a real text "
            "layer. This is Apple Markup / Preview 'print to PDF' output: the ink layer was "
            "merged into page graphics, so there is nothing to read programmatically. "
            "Cluster the strokes, anchor them to the text underneath, and transcribe the "
            "handwriting visually. This is the expensive case but also the richest."
        )
    elif has_annots and has_ink:
        pipeline, why = "MIXED", (
            "Both live annotations and flattened ink. Handle A first (free), then B for "
            "whatever strokes remain unaccounted for."
        )
    else:
        pipeline, why = "NONE_DETECTED", (
            "No annotations, no coloured ink, no scan signature. Either the markup is in "
            "the same colour as body text, or there is no markup. Try lowering --sat, or "
            "render a page you know is marked and look at it before going further."
        )

    out["verdict"] = {"pipeline": pipeline, "reasoning": why}

    if out["coloured_typeset_text"]["total_spans"]:
        out["verdict"]["note_coloured_text"] = (
            "Coloured TYPESET text found. This is not reviewer ink — it is authored in the "
            "source (e.g. \\textcolor{red}{...}) and is greppable directly. Keep it in a "
            "separate section of the output; conflating the author's own TODOs with a "
            "reviewer's corrections misattributes both."
        )
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("pdf")
    ap.add_argument("--json", action="store_true", help="machine-readable output")
    a = ap.parse_args()

    r = analyse(a.pdf)
    if a.json:
        print(json.dumps(r, indent=2, default=str))
        return

    v = r["verdict"]
    print(f"\n{r['file']}  ({r['pages']} pages)")
    print(f"producer : {r['producer']}")
    print(f"\n  annotations        : {r['annotations']['total']}  {r['annotations']['by_subtype'] or ''}")
    print(f"  ink strokes        : {r['ink_vector_strokes']['total']}")
    for c in r["ink_vector_strokes"]["by_colour"]:
        print(f"      {c['hex']}  rgb{tuple(c['rgb'])}  x{c['strokes']}")
    print(f"  coloured typeset   : {r['coloured_typeset_text']['total_spans']} spans")
    print(f"  text layer         : {r['text_layer']['chars_per_page']} chars/page  "
          f"(real text: {r['text_layer']['has_real_text']})")
    print(f"  full-page rasters  : {r['raster']['full_page_image_pages']}")
    print(f"\n  ==> PIPELINE: {v['pipeline']}")
    print(f"      {v['reasoning']}")
    if "note_coloured_text" in v:
        print(f"\n  ==> ALSO: {v['note_coloured_text']}")
    print()


if __name__ == "__main__":
    main()
