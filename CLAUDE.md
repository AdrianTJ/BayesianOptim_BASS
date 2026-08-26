# Working conventions

Guidance for anyone — human or agent — making changes in this repository.

## Branches

Branch names carry a type prefix describing the kind of work:

| prefix | for |
|---|---|
| `feat/` | new capability, packaging, or release work |
| `bug/` | fixes to broken behaviour |
| `research/` | experiment programs and their records |
| `docs/` | documentation-only changes |

**Do not use a `claude/` prefix.** Branches are named for the work, not
for who or what produced them. Several `claude/*` branches exist in the
history from before this rule; they are not a precedent to follow.

Develop on a branch, never directly on `main`.

## Research records are append-only

Everything under `article_bo_machinery/research/` is a provenance trail,
and the article's methodology depends on it being trustworthy:

- **`DESIGN.md` files are frozen once committed.** They are
  pre-registrations, committed before the runs they govern, and git
  history is the evidence of that ordering. If a DESIGN is later found
  to be imperfect, record the correction in the analysis or a status
  note — do not edit the DESIGN.
- **`LOG.md` files are append-only.** Add a new entry; do not rewrite
  earlier ones. An entry that was accurate when written stays as it is,
  even when the world has moved on. A log naming a since-renamed branch
  is correct history, not a stale reference.
- **Committed `results.jsonl` / `results.csv` are read-only.** No edit
  to raw results is ever part of a fix.
- **A result becomes a finding only after adversarial review** by
  someone other than whoever produced it. Unreviewed output does not
  enter `main.tex`, however suggestive it looks.

## Checks before committing

Three deterministic checkers gate the article and its research tree:

```bash
python3 article_bo_machinery/research/writing_loop/tools/check_article.py
python3 article_bo_machinery/research/tools/check_research.py
python3 article_bo_machinery/research/article_loop/tools/check_experiments.py
```

`check_article.py` enforces a TODO ratchet: the count of `% TODO`
markers in `main.tex` may fall but never rise.

The package has its own suite, run from the `bo-audit/` directory:

```bash
cd bo-audit && python3 -m pytest tests
```

`tests/test_benchmarks_h1_parity.py` guards the benchmark code vendored
into `bo_audit/benchmarks_h1.py` against its source in
`research/article_loop/experiments/machinery.py`. It **skips** when
`machinery` cannot be imported, and a skip means the guard did not run —
read the skip message, which reports whether the research tree was found
and quotes the real import error. Importing `machinery` needs `scipy`.

## Building the article

```bash
cd article_bo_machinery
pdflatex main && bibtex main && pdflatex main && pdflatex main
```

The article's one figure is committed, so no generation step is needed.
To regenerate it from the committed results, run
`figures/make_fig_dedup_audit.py` (needs matplotlib and numpy). The
REVTeX 4.1 class comes from a full TeX distribution, not from this
repository.
