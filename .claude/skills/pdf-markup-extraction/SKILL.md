---
name: pdf-markup-extraction
description: Extract handwritten or annotated corrections from a marked-up PDF into structured, machine-actionable JSON — each mark paired with the exact text it points at. Use this whenever a PDF carries review marks of any kind: an advisor's or supervisor's comments on a thesis or paper, reviewer/editor markup, red-pen corrections, iPad/Apple Pencil annotations, highlights and margin notes, or a scanned page someone wrote on. Trigger it for phrasings like "extract the notes/comments/corrections from this PDF", "my professor marked up my draft", "turn these annotations into a list", "what did the reviewer write", "I got feedback as a PDF", or any request to work through markup systematically rather than by hand. Also use it when someone wants to apply PDF feedback to source files (LaTeX, Markdown, docs), since the anchoring step is what makes that possible. Reach for this even if the user does not say "annotation" or "extract" — if there are marks on a PDF and they want to do something with them, this is the skill.
---

# Extracting markup from PDFs

A marked-up PDF is a document plus somebody's thinking about that document. The
thinking is the valuable part and it is trapped: it exists as ink, or as objects
a viewer renders but never surfaces as a list. Getting it out by hand means
scrolling, squinting, and retyping — which is why people put it off for months.

The goal is not a transcript. A transcript ("page 13 says 'unclear'") is barely
better than the PDF. The goal is a record where every mark is **paired with the
exact text it points at**, so the result can drive edits rather than just be read.

## Start with triage, always

Marked-up PDFs come in three shapes. They look identical in a viewer and need
completely different pipelines. Guessing wrong costs an expensive vision pass and
sends you down the wrong road, so spend one cheap call finding out:

```bash
python scripts/triage.py FILE.pdf
```

It reports the annotation objects, ink strokes and their colours, coloured typeset
text, the text layer, and which pipeline applies:

| Verdict | What you have | What to do |
|---|---|---|
| `A_REAL_ANNOTATIONS` | Live `/Annot` objects | Read them directly. **No vision pass needed.** |
| `B_FLATTENED_INK` | Coloured vector strokes, no annots | Cluster → anchor → transcribe visually |
| `C_SCANNED` | Full-page rasters, no text layer | Read both layers visually; no anchoring possible |
| `MIXED` | Both | Do A first (free), then B for the rest |

### A — real annotations

The cheap case. The reviewer's words are already text, and for `Highlight` /
`Underline` / `StrikeOut` / `Squiggly` the `/QuadPoints` array records exactly which
glyphs were selected — so the anchor is *exact*, not inferred.

```bash
python scripts/extract_annots.py FILE.pdf -o annots.json
```

Do not render pages in this case; it is pure waste. The exception is `/Ink`
(freehand) and `/Stamp`, which carry no text — the script flags those and tells you
which pages to render.

Two things the output gives you for free: `anchor_is_exact` distinguishes a
QuadPoints anchor from one merely inferred from a margin note's position, and
`by_author` reveals when a document has been through more than one reviewer, whose
notes may well disagree.

### B — flattened ink

The common case for anything annotated on an iPad or exported from Preview: the
markup layer gets merged into page graphics, so `/Annot` is empty even though the
page is covered in red. There is nothing to read programmatically — the ink is
just curves. But the ink has coordinates, and so does every word underneath, and
that is the whole opening.

```bash
python scripts/extract_marks.py FILE.pdf -o marks.json
```

This auto-detects the ink colour, merges strokes into contiguous marks, and pairs
each with `text_under_mark` (what the mark sits on) and `text_on_line` (context for
margin notes). Then render and read:

```bash
python scripts/render.py FILE.pdf pages -o pages/ --pages 6,7,13-19
```

170 dpi is the default because it is legible for most handwriting and cheap enough
to read fifty of. Work through the pages in order, filling in the `note` and
`confidence` fields for each mark. `marks.json` tells you how many marks to expect
on each page — if you can only find three and it says seven, look again before
moving on.

### C — scanned

Both the markup and the document are pixels. You must read everything visually and
you cannot anchor, because the underlying text has no coordinates to pair with.
Quote the surrounding printed text as the anchor instead, and say plainly in the
output that anchors are approximate. This case has the lowest confidence of the
three and the output should admit it.

## Transcribing handwriting: the part that needs discipline

This is the only step a script cannot do, and it is where the errors live. In
studies of vision models reading handwriting, **the large majority of downstream
errors trace to transcription**, and — this is the dangerous part — the failures
skew toward *confident plausible invention* rather than visible confusion. A model
that cannot read a word tends to produce a sensible-looking guess, not an error.

That failure mode is unusually costly here. A fabricated correction is worse than
a missing one: a gap gets noticed, but an invented instruction gets applied, and it
arrives wearing the reviewer's authority. The person acting on your output cannot
tell your guess from their supervisor's actual words.

So the working rule is: **record what you can read, flag what you cannot, and never
smooth over the difference.**

- Set `confidence` on every mark: `high` (unambiguous), `medium` (legible, some
  doubt), `low` (guessing).
- Add `needs_review` with a note on *what specifically* is uncertain whenever you
  are not confident. "Reads 'Needed?' but the final glyph could be '!'" lets the
  user resolve it in two seconds. "Unclear" does not.
- Zoom before you guess. It is one cheap call:
  ```bash
  python scripts/render.py FILE.pdf crop -o crops/ --page 13 --box 105,555,235,590 --tag weird
  ```
  Copy `--box` straight from `marks.json` — those are PDF points. Pixel coords from
  a render will not work (the script will tell you so).
- If it is still illegible at 440 dpi, say so. Leave the note empty, describe what
  you can see (a circle, an arrow and where it points), and mark it `low`. An honest
  gap is a useful output. An invented reading is a landmine.

**Cross-reference before you settle.** The same hand writes the same word the same
way. A mark that is ambiguous alone is often obvious once you have seen it
elsewhere, and an unlabelled mark can be resolved by a related mark on another
page. Read the whole document before finalising the uncertain ones, and revise
earlier confidence upward when later pages settle a question. This is the main
reason to transcribe sequentially in one context rather than fanning pages out to
parallel workers.

## Separate the reviewer's marks from the author's own

`triage.py` flags coloured *typeset* text — text that is red (or blue, or whatever)
in the **source**, e.g. `\textcolor{red}{TODO: fix this}`. It looks identical to ink
on screen and is completely different in kind: it is the author talking to
themselves, not the reviewer talking to the author.

Keep it in its own section of the output. Conflating them misattributes both — it
puts the author's uncertainties in the reviewer's mouth and buries the reviewer's
actual instructions. It is also directly greppable in the source, so it needs no
transcription at all.

Watch for reviewers *answering* these notes; that pairing is often the most useful
content in the document, and it is invisible unless you are tracking both.

## Check the markup against the live source

Markup is a photograph of a document at one moment. By the time anyone acts on it,
the source has usually moved on — text reworded, sections rewritten, some
corrections already applied. Handing back a list that silently includes months-old
resolved items wastes the user's time and makes them distrust the whole artefact.

Establish the gap up front (`git log` for when the PDF landed vs. now), then:

```bash
python scripts/verify_anchors.py corrections.json --source src/ --ext .tex --write
```

A hit means the marked text still exists verbatim; a miss means it changed — which
is a prompt to **check**, not proof the work is done. Only run it on anchors that
are genuine quotes; set `anchor_is_verbatim: false` on descriptive anchors ("the
Chapter 3 title") so they are skipped rather than reported as spurious misses.

## Output shape

One JSON file. Structure it so a script can consume it and a human can read it:

```jsonc
{
  "source": { /* file, pages, reviewer, markup tool, dates, ink colour */ },
  "extraction": {
    "method": "...",
    "caveat": "Handwriting has no text encoding; every note is a legibility
               judgement. Check confidence and needs_review before acting.",
    "staleness_warning": "..."
  },
  "field_guide": { /* what each field means — the file should explain itself */ },
  "counts": { /* by kind, by confidence, needs_review */ },
  "author_todos": [ /* coloured typeset text — the author's own, kept separate */ ],
  "corrections": [
    {
      "id": "C001",
      "pdf_page": 13,
      "printed_page": 9,
      "kind": "replace",
      "anchor": "the vectorized version of this equation",
      "note": "is already vectorized",     // what the ink SAYS (transcription)
      "action": "...",                     // what to DO (your interpretation)
      "confidence": "high",
      "needs_review": "...",               // only when uncertain
      "anchor_is_verbatim": true,
      "anchor_found_in": "TeX_files/GP.tex"
    }
  ]
}
```

Keep `note` and `action` separate. `note` is evidence — what is physically written.
`action` is inference — what you think it means. Fusing them destroys the user's
ability to audit your reading, which is the one thing that makes an uncertain
transcript trustworthy.

Useful `kind` values: `replace`, `insert`, `delete`, `typo`, `punctuation`,
`capitalization`, `typography`, `math_correction`, `notation`, `citation`,
`bibliography`, `figure`, `rewrite`, `restructure`, `structural_note`, `query`,
`agreement`. Add others as the document demands.

**`query` deserves its own treatment.** A large share of real markup is an underline
plus "?" — the reviewer flagging something without prescribing a fix. These are not
mechanically applicable and should not be presented as if they were. Counting them
separately tells the user what they can automate versus what needs their judgement.

## Reporting back

Lead with what the markup *says*, not what you did. The user wants their reviewer's
verdict; the pipeline is plumbing.

Then give them the numbers that determine what they can do next: how many
corrections, how many are mechanically applicable versus judgement calls, how many
need review, and how many anchors have gone stale. Quote the substantive notes
verbatim — a supervisor's "it took 26 pages to see where you are going" lands in a
way that "structural feedback on chapter ordering" never will.

Say plainly what you could not read.

## Scripts

| Script | Does |
|---|---|
| `scripts/triage.py` | Classify the PDF, pick the pipeline. **Run first.** |
| `scripts/extract_annots.py` | Pipeline A: read real annots + exact QuadPoints anchors |
| `scripts/extract_marks.py` | Pipeline B: cluster ink into marks, anchor each to its text |
| `scripts/render.py` | `pages` for reading; `crop` for zooming a doubtful mark |
| `scripts/verify_anchors.py` | Check anchors against the live source tree |
| `scripts/selftest.py` | Builds synthetic PDFs and asserts routing + anchoring. Run if anything looks off. |

All need `pymupdf` (`pip install pymupdf`). If the environment is externally managed,
a throwaway venv is fine — nothing here needs to persist.

Tuning worth knowing:

- `--gap` (default 14pt) sets how far apart strokes can be and still count as one
  mark. Too small splits a word into letters; too large swallows a whole margin into
  one blob. The script warns when the strokes-per-mark ratio looks suspicious.
- `--anchor-pad` (default 3pt) is the tolerance for matching a mark to its text. It
  exists because an underline sits *below* the glyphs it marks and would otherwise
  anchor to nothing. Keep it well under the line spacing or marks will grab the
  neighbouring line.
- `--colour` overrides auto-detection. Run once per colour when a reviewer used two
  pens — a second colour usually means a second meaning, or a second reviewer.

If a count looks wrong, compare `triage.py`'s stroke total against
`extract_marks.py`'s: a gap means strokes are being filtered, and that is worth
understanding rather than shrugging at.
