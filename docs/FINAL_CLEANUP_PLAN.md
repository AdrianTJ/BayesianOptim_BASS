# Final-version cleanup plan

Execute after the review of PR #12 is done and the thesis is being finalized.
Nothing here blocks review; it is the checklist for turning the working repo
into the final public version.

## 1. Delete working files that served their purpose

- [ ] `run_on_ec2.sh` (repo root). One-shot EC2 provisioning runner; the run
      it existed for is done and its outputs live in `final_results/`.
      Reproducibility stays covered by `code_files/run_all_final.sh` and
      `RUNNING.md`. Also update `final_results/README.md`, which mentions the
      script ("or remotely via `run_on_ec2.sh`"), and check `RUNNING.md` for
      the EC2 recipe section.
- [ ] `written_files/tesis_escrito/PASS2_INSTRUCTIONS.md`. The worklist is
      fully executed (see its status header); git history preserves it.
- [ ] `docs/FINAL_CLEANUP_PLAN.md` (this file), last, once everything above
      is done. `docs/AI_DISCLOSURE.md` stays; it backs the thesis disclosure
      section.

## 2. Local-only clutter (already gitignored, nothing tracked)

`.gitignore` already covers `.DS_Store` (root rule plus `**/.DS_Store`) and
`.Rhistory` (`**/.Rhistory`), and `git ls-files` confirms no copy of either
is tracked. They only exist on local disks. To clear them locally:

```bash
find . -name .DS_Store -delete
rm -f .Rhistory
```

LaTeX build artifacts in `written_files/tesis_escrito/` (aux, log, toc, bbl,
etc.) are likewise ignored and untracked; `main.pdf` is the one deliberate
exception and stays tracked. Optional local clean: `latexmk -c` in that
directory.

## 3. Thesis final-version debt (author tasks, tracked from PASS2 Step 4)

- [ ] Replace the two `missing.png` stand-ins in `BASS.tex` (theoretical
      basis diagram; decision-tree diagram).
- [ ] Resolve the two `% AUTHOR NOTE` comments in `BASS.tex`.
- [ ] Set `\date{}` in `main.tex`.
- [ ] Add the disclosure section to the thesis (source text in
      `docs/AI_DISCLOSURE.md`).
- [ ] Turn the traceability markup black: redefine `\green`/`\blue` to `#1`
      in `main.tex` (or strip the wrappers), then rebuild and commit
      `main.pdf`.

## 4. Repository tidy after merge

- [ ] Merge PR #12, then delete the merged remote branches
      (`claude/bass-bo-benchmark-review-hnxikv` and any other stale
      `claude/*` or feature branches).
- [ ] Decide the fate of PR #13 (the machinery-confound article scaffold);
      it is separate from the thesis.
- [ ] Optional: review whether `class_presentation/` belongs in the final
      public repo.
- [ ] `final_results/elastic_net/` stays: it is unused by the thesis but is
      part of the run's provenance.

## 5. Final verification (run after all of the above)

```bash
cd written_files/tesis_escrito
pdflatex -interaction=nonstopmode main.tex && bibtex main && \
  pdflatex -interaction=nonstopmode main.tex && pdflatex -interaction=nonstopmode main.tex
grep -E "^!|Citation.*undefined|Reference.*undefined" main.log   # must be empty
grep -rn "PASS2\|AUTHOR NOTE\|missing.png\|INSERT DATE" TeX_files/ main.tex  # must be empty
```
