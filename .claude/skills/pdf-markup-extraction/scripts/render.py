#!/usr/bin/env python3
"""Render pages for reading, and zoomed crops for marks you cannot make out.

Two modes:

  pages  — one PNG per page, sized for a vision pass. 170 dpi is the sweet spot:
           legible for most handwriting, cheap enough to read 50 of them.
  crop   — a tight, high-dpi window on one mark. Reach for this the moment a mark
           is ambiguous, rather than guessing. Guessing on handwriting is how
           fabricated corrections get into the output.

Coordinates are PDF POINTS (72/inch), not screen pixels. A 595x842pt A4 page
rendered at 170dpi is 1406x1988px — passing pixel coords to --box silently
produces garbage or an error. Copy bboxes straight from marks.json and you are
always in points. This script rejects out-of-range boxes rather than failing
cryptically deeper in.

Usage:
    python render.py FILE.pdf pages -o out/ --pages 6,7,13-19
    python render.py FILE.pdf crop  -o out/ --page 13 --box 120,545,220,585 --tag weird
"""
import argparse
import os
import sys

try:
    import pymupdf
except ImportError:
    sys.exit("pymupdf missing. Install with: pip install pymupdf")


def parse_pages(spec, n):
    if not spec:
        return list(range(1, n + 1))
    out = []
    for part in spec.split(","):
        part = part.strip()
        if "-" in part:
            a, b = part.split("-")
            out.extend(range(int(a), int(b) + 1))
        elif part:
            out.append(int(part))
    return [p for p in out if 1 <= p <= n]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("pdf")
    ap.add_argument("mode", choices=["pages", "crop"])
    ap.add_argument("-o", "--out", default="render")
    ap.add_argument("--pages", help="e.g. 6,7,13-19 (pages mode; default all)")
    ap.add_argument("--dpi", type=int, help="default 170 for pages, 440 for crop")
    ap.add_argument("--page", type=int, help="page number (crop mode)")
    ap.add_argument("--box", help="x0,y0,x1,y1 in PDF POINTS (crop mode)")
    ap.add_argument("--pad", type=float, default=6.0, help="pt of breathing room around --box")
    ap.add_argument("--tag", default="crop", help="filename suffix (crop mode)")
    a = ap.parse_args()

    doc = pymupdf.open(a.pdf)
    os.makedirs(a.out, exist_ok=True)

    if a.mode == "pages":
        dpi = a.dpi or 170
        for n in parse_pages(a.pages, len(doc)):
            pix = doc[n - 1].get_pixmap(dpi=dpi)
            f = os.path.join(a.out, f"p{n:03d}.png")
            pix.save(f)
            print(f"{f}  {pix.width}x{pix.height}")
        return

    # crop
    if a.page is None or not a.box:
        sys.exit("crop mode needs --page and --box x0,y0,x1,y1 (in points)")
    dpi = a.dpi or 440
    page = doc[a.page - 1]
    try:
        x0, y0, x1, y1 = (float(v) for v in a.box.split(","))
    except ValueError:
        sys.exit("--box must be four comma-separated numbers: x0,y0,x1,y1")

    box = pymupdf.Rect(x0 - a.pad, y0 - a.pad, x1 + a.pad, y1 + a.pad)
    clip = box & page.rect
    if clip.is_empty:
        sys.exit(
            f"--box {a.box} does not intersect page {a.page} (page is "
            f"{page.rect.width:.0f}x{page.rect.height:.0f} pt).\n"
            f"Most likely you passed PIXEL coords from a render. Divide by dpi/72 "
            f"(e.g. at 170dpi, divide by {170/72:.3f}) — or just copy the bbox from marks.json."
        )
    pix = page.get_pixmap(dpi=dpi, clip=clip)
    f = os.path.join(a.out, f"p{a.page:03d}_{a.tag}.png")
    pix.save(f)
    print(f"{f}  {pix.width}x{pix.height}  clip={[round(v,1) for v in clip]}")


if __name__ == "__main__":
    main()
