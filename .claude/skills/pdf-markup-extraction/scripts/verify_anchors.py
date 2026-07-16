#!/usr/bin/env python3
"""Check each anchor against the live source tree, so stale corrections are visible.

Markup is a photograph of a document at one moment. By the time anyone acts on it,
the source has usually moved on — text reworded, sections deleted, some corrections
already applied. Handing back a correction list that silently includes items the
author fixed months ago wastes their time and erodes trust in the whole artefact.

This does a normalised substring search for a distinctive run of words from each
anchor. A hit means the marked text still exists verbatim and the correction is
probably still live. A miss means the text changed — which is a signal to CHECK,
not proof the work is done. Reserve it for anchors that are genuine quotes; for
anchors that are descriptions ("Chapter 3 title"), a miss means nothing.

Usage:
    python verify_anchors.py corrections.json --source src/ --field anchor
    python verify_anchors.py corrections.json --source src/ --ext .tex .md --write
"""
import argparse
import glob
import json
import os
import re
import sys
import unicodedata


def norm(s):
    """Flatten markup and punctuation so a LaTeX/Markdown source can match prose."""
    s = unicodedata.normalize("NFKD", s)
    s = re.sub(r"\\[a-zA-Z]+\*?", " ", s)      # \textit, \cite ...
    s = re.sub(r"[{}$\\_^~&%#]", " ", s)       # markup punctuation
    s = re.sub(r"[^a-z0-9 ]", " ", s.lower())
    return re.sub(r"\s+", " ", s).strip()


def load_sources(root, exts):
    corpus = {}
    for ext in exts:
        for f in glob.glob(os.path.join(root, "**", "*" + ext), recursive=True):
            try:
                with open(f, encoding="utf-8", errors="ignore") as fh:
                    corpus[os.path.relpath(f, root)] = norm(fh.read())
            except OSError:
                pass
    return corpus


def find(anchor, corpus, probes=(10, 7, 5)):
    """Longest distinctive run wins. Slide the window so a mangled head or tail
    (very common — anchors get clipped at line breaks) does not veto a real match."""
    if not anchor:
        return None
    words = norm(anchor).split()
    for n in probes:
        if len(words) < n:
            continue
        for start in range(0, max(1, len(words) - n + 1)):
            probe = " ".join(words[start:start + n])
            for fn, txt in corpus.items():
                if probe in txt:
                    return fn
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("corrections", help="JSON with a list of items (or {'corrections': [...]})")
    ap.add_argument("--source", required=True, help="root of the source tree")
    ap.add_argument("--ext", nargs="+", default=[".tex", ".md", ".txt", ".rst"])
    ap.add_argument("--field", default="anchor")
    ap.add_argument("--verbatim-field", default="anchor_is_verbatim",
                    help="bool field; items where this is false are skipped as not-quotes")
    ap.add_argument("--write", action="store_true", help="write results back into the file")
    a = ap.parse_args()

    doc = json.load(open(a.corrections))
    items = doc["corrections"] if isinstance(doc, dict) and "corrections" in doc else doc
    if not isinstance(items, list):
        sys.exit("expected a list of correction objects")

    corpus = load_sources(a.source, a.ext)
    if not corpus:
        sys.exit(f"no {'/'.join(a.ext)} files under {a.source}")

    hits = misses = skipped = 0
    for it in items:
        if it.get(a.verbatim_field) is False:
            it["anchor_found_in"] = None
            it["anchor_check"] = "skipped (anchor is descriptive, not a quote)"
            skipped += 1
            continue
        f = find(it.get(a.field), corpus)
        it["anchor_found_in"] = f
        it["anchor_check"] = "found" if f else "NOT FOUND — text may have been reworded or the fix already applied"
        hits += bool(f)
        misses += (not f)

    print(f"source files : {len(corpus)}")
    print(f"found        : {hits}")
    print(f"not found    : {misses}   <- review these; they may already be done")
    print(f"skipped      : {skipped}  (descriptive anchors)")

    if a.write:
        json.dump(doc, open(a.corrections, "w"), indent=2, ensure_ascii=False)
        print(f"\nwrote results back into {a.corrections}")
    else:
        print("\n(dry run — pass --write to save)")


if __name__ == "__main__":
    main()
