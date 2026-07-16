#!/usr/bin/env python3
"""Cluster coloured ink strokes into marks and anchor each one to the text underneath.

This is the bridge that makes flattened markup usable. The ink itself carries no
text, but it has coordinates — and so does every word in the text layer. Pairing
them turns "there is a squiggle on page 13" into "there is a squiggle on the words
'vectorized version of this equation'", which is what makes the output actionable
rather than merely descriptive.

Emits marks.json: one entry per cluster, with bbox, the words it overlaps, the
words on its line, and whether it sits in the margin. You then read the rendered
page and fill in what the ink SAYS.

Usage:
    python extract_marks.py FILE.pdf -o marks.json
    python extract_marks.py FILE.pdf --colour '#ff4017' --gap 14
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
    if not rgb or len(rgb) != 3:
        return False
    return (max(rgb) - min(rgb)) > sat and max(rgb) > dark


def near(a, b, tol=0.18):
    return all(abs(x - y) <= tol for x, y in zip(a, b))


def hex_to_rgb(h):
    h = h.lstrip("#")
    return tuple(int(h[i:i + 2], 16) / 255 for i in (0, 2, 4))


def dominant_colour(doc, sat, dark):
    """Pick the ink colour by stroke count. Reviewers use one pen."""
    tally = defaultdict(int)
    for page in doc:
        for d in page.get_drawings():
            for attr in ("color", "stroke", "fill"):
                c = d.get(attr)
                if is_ink(c, sat, dark):
                    tally[tuple(round(v, 1) for v in c)] += 1
                    break
    if not tally:
        return None
    return max(tally.items(), key=lambda kv: kv[1])[0]


def belongs_to_annot(rect, boxes, frac=0.5):
    """True if a drawing is an annotation's own appearance rather than loose ink.

    get_drawings() also returns the vector content of annotation appearance streams —
    a highlight IS a coloured rectangle. Clustering those would invent 'marks' that
    are really live annotations, better read with extract_annots.py.
    """
    if not boxes:
        return False
    area = abs(rect)
    if area <= 0:
        return any(rect in b for b in boxes)
    return any(abs(rect & b) > frac * area for b in boxes)


def ink_rects(page, target, sat, dark, tol):
    out = []
    abox = [pymupdf.Rect(a.rect) for a in (page.annots() or [])]
    for d in page.get_drawings():
        if belongs_to_annot(pymupdf.Rect(d["rect"]), abox):
            continue
        col = None
        for attr in ("color", "stroke", "fill"):
            c = d.get(attr)
            if is_ink(c, sat, dark):
                col = c
                break
        if col is None:
            continue
        if target and not near(col, target, tol):
            continue
        r = pymupdf.Rect(d["rect"])
        if r.is_infinite:
            continue
        # A perfectly horizontal or vertical stroke has zero height/width, which makes
        # its Rect "empty" — and an underline is exactly that. Dropping empties would
        # silently discard the single most common markup gesture there is. Hand-drawn
        # ink is usually wavy enough to have real extent, so this hides well: it only
        # bites on ruler-straight lines, which is precisely what a drawing tool emits.
        # Give degenerate rects a hairline extent instead.
        if r.width <= 0 or r.height <= 0:
            r = pymupdf.Rect(r.x0 - 0.25, r.y0 - 0.25, r.x1 + 0.25, r.y1 + 0.25)
        # a stroke spanning nearly the whole page is a border, not a mark
        if r.width > 0.92 * page.rect.width and r.height > 0.92 * page.rect.height:
            continue
        out.append(r)
    return out


def cluster(rects, gap):
    """Merge strokes whose inflated boxes touch.

    A handwritten word is many strokes; a margin note is many words. One physical
    'mark' is whatever is spatially contiguous, so grow each box by `gap` and union
    anything that overlaps. Tune gap: too small splits words into letters, too large
    swallows the whole margin into one blob.
    """
    boxes = [pymupdf.Rect(r) for r in rects]
    changed = True
    while changed:
        changed = False
        merged = []
        while boxes:
            b = boxes.pop()
            touching = [o for o in boxes
                        if pymupdf.Rect(b.x0 - gap, b.y0 - gap, b.x1 + gap, b.y1 + gap).intersects(o)]
            if touching:
                for o in touching:
                    boxes.remove(o)
                    b = b | o
                changed = True
            merged.append(b)
        boxes = merged
    return sorted(boxes, key=lambda r: (round(r.y0 / 12), r.x0))


def body_span(words):
    if not words:
        return 0, 0
    return min(w[0] for w in words), max(w[2] for w in words)


def label_candidates(page, band=0.15):
    """Numeric tokens in the header/footer bands — possible printed page numbers.

    Naively grabbing the first digit on the page is wrong in a way that is easy to
    miss: on a chapter opening, "Chapter 1" yields 1 while the printed folio is 3.
    Restricting to the margin bands and cross-checking the offset across the whole
    document (see resolve_labels) avoids inventing plausible-but-wrong page numbers.
    """
    h = page.rect.height
    out = []
    for w in page.get_text("words"):
        x0, y0, x1, y1, txt = w[0], w[1], w[2], w[3], w[4]
        if not (txt.isdigit() and len(txt) <= 4):
            continue
        mid = (y0 + y1) / 2
        if mid < band * h or mid > (1 - band) * h:
            out.append(int(txt))
    return out


def resolve_labels(doc):
    """Map pdf page -> printed page by finding the dominant (pdf_page - printed) offset.

    A real folio sequence advances in lockstep with the PDF, so the correct offset is
    the one that the most pages agree on. Stray numbers (chapter numbers, years,
    figure captions) do not agree with each other and fall away.
    """
    votes = Counter()
    per_page = {}
    for i, page in enumerate(doc, 1):
        cands = label_candidates(page)
        per_page[i] = cands
        for c in cands:
            if 0 <= i - c < 40:
                votes[i - c] += 1
    if not votes:
        return {i: None for i in per_page}
    offset, agree = votes.most_common(1)[0]
    if agree < max(3, 0.15 * len(doc)):
        return {i: None for i in per_page}
    labels = {}
    for i, cands in per_page.items():
        want = i - offset
        labels[i] = want if want in cands else (want if want > 0 else None)
    return labels


def coloured_text(page, sat, dark):
    """Typeset coloured text: authored in the source, NOT reviewer ink."""
    out = []
    for blk in page.get_text("dict")["blocks"]:
        for line in blk.get("lines", []):
            buf, box = [], None
            for span in line["spans"]:
                c = span["color"]
                rgb = (((c >> 16) & 255) / 255, ((c >> 8) & 255) / 255, (c & 255) / 255)
                if is_ink(rgb, sat, dark) and span["text"].strip():
                    buf.append(span["text"])
                    r = pymupdf.Rect(span["bbox"])
                    box = r if box is None else box | r
            if buf:
                out.append({"text": " ".join(buf).strip(),
                            "bbox": [round(v, 1) for v in box]})
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("pdf")
    ap.add_argument("-o", "--out", default="marks.json")
    ap.add_argument("--colour", help="target ink as hex, e.g. '#ff4017'. Default: auto-detect")
    ap.add_argument("--gap", type=float, default=14.0, help="stroke merge distance in pt (default 14)")
    ap.add_argument("--sat", type=float, default=0.22)
    ap.add_argument("--dark", type=float, default=0.30)
    ap.add_argument("--tol", type=float, default=0.18, help="colour match tolerance")
    ap.add_argument("--anchor-pad", type=float, default=3.0,
                    help="pt of tolerance when matching a mark to the text it marks; "
                         "catches underlines that sit just below the glyphs (default 3)")
    a = ap.parse_args()

    doc = pymupdf.open(a.pdf)
    labels = resolve_labels(doc)
    target = hex_to_rgb(a.colour) if a.colour else dominant_colour(doc, a.sat, a.dark)
    if target is None:
        sys.exit("No ink-coloured strokes found. Run triage.py; you may not have a "
                 "flattened-ink PDF, or the ink may be darker than --dark.")

    pages, total_marks, total_strokes = [], 0, 0
    for i, page in enumerate(doc, 1):
        rects = ink_rects(page, target, a.sat, a.dark, a.tol)
        ctext = coloured_text(page, a.sat, a.dark)
        if not rects and not ctext:
            continue

        words = page.get_text("words")
        x0b, x1b = body_span(words)
        marks = []
        for c in cluster(rects, a.gap):
            if c.width < 1.5 and c.height < 1.5:
                continue
            # Underlines sit just BELOW the glyphs they mark, so a strict intersection
            # returns nothing for the commonest gesture of all. Probe with a small pad
            # to catch text the mark abuts rather than overlaps. Keep the pad well under
            # the line spacing (~12-14pt) so it does not reach the neighbouring line.
            probe = pymupdf.Rect(c.x0 - a.anchor_pad, c.y0 - a.anchor_pad,
                                 c.x1 + a.anchor_pad, c.y1 + a.anchor_pad)
            under = [w[4] for w in words if pymupdf.Rect(w[:4]).intersects(probe)]
            mid = (c.y0 + c.y1) / 2
            band = sorted((w for w in words if abs((w[1] + w[3]) / 2 - mid) < 14),
                          key=lambda w: w[0])
            marks.append({
                "bbox": [round(v, 1) for v in c],
                "width": round(c.width, 1),
                "height": round(c.height, 1),
                "in_margin": bool(c.x0 > x1b - 5 or c.x1 < x0b + 5),
                "text_under_mark": " ".join(under),
                "text_on_line": " ".join(w[4] for w in band),
                "note": None,          # <- you fill this in from the render
                "confidence": None,    # <- high | medium | low
            })
        total_marks += len(marks)
        total_strokes += len(rects)
        pages.append({
            "pdf_page": i,
            "printed_page": labels.get(i),
            "strokes": len(rects),
            "marks": marks,
            "coloured_typeset_text": ctext,
        })

    doc_out = {
        "source_pdf": a.pdf,
        "ink_rgb": [round(v, 3) for v in target],
        "ink_hex": "#%02x%02x%02x" % tuple(int(v * 255) for v in target),
        "cluster_gap_pt": a.gap,
        "totals": {
            "pdf_pages": len(doc),
            "annotated_pages": len(pages),
            "marks": total_marks,
            "strokes": total_strokes,
        },
        "pages": pages,
    }
    with open(a.out, "w") as f:
        json.dump(doc_out, f, indent=2, ensure_ascii=False)

    print(f"ink {doc_out['ink_hex']} | {len(pages)} annotated pages | "
          f"{total_marks} marks from {total_strokes} strokes -> {a.out}")
    ct = sum(len(p["coloured_typeset_text"]) for p in pages)
    if ct:
        print(f"note: {ct} coloured TYPESET text runs found — authored in source, not ink. "
              f"Keep separate.")
    if total_marks and total_strokes / total_marks > 60:
        print(f"hint: {total_strokes/total_marks:.0f} strokes per mark looks high — "
              f"try a smaller --gap to split merged notes apart.")


if __name__ == "__main__":
    main()
