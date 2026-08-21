# Loop & graph engineering — research/writing/testing loop plan

**Topic:** Agentic loop engineering and workflow-graph design — how to build
long-running research, writing, and testing loops (like the ones driving this
repository's article work): loop architectures, orchestration graphs, state
across iterations, quality control, and cost engineering.

**Purpose:** Build a referenceable, markdown-only body of knowledge about how
we run this project's own automation, and continuously improve that automation
using what we learn. Everything the loop produces — research notes, decisions,
computed checks — is written to markdown in this folder and committed, so it
can be referenced later.

---

## Loop shape

One **cycle** = research → write → test → log → commit/push. Cycles run
unattended on this branch (`claude/machinery-confound-article`), self-paced
(20–30 min idle cadence, immediately when a cycle's work is ready to continue).
The supervising session (Fable 5) reviews and synthesizes; fan-out subagents
(Sonnet 5) do searches and drafts, mirroring the split used for the novelty
check.

### 1. Research leg
Take the next open question from the backlog below. Fan out web searches if
the question needs external sources; otherwise mine our own artifacts (the
novelty-check workflow, its journal, this repo's history) as primary evidence.
Write findings to `notes/NN_<slug>.md` — one file per question, with sources
cited, and a "what this changes for us" section.

### 2. Writing leg
Apply at most one improvement per cycle to our own loop/workflow practice,
derived from the research leg: e.g. a reusable workflow-script template, a
checklist, a convention added to `research/README.md`. Improvements are
documents in this folder, not changes to the article or the thesis code —
those stay out of scope for the unattended loop.

### 3. Testing leg
Every cycle, run the repo-health checks and record the outcome in the cycle's
log entry (environment has no R or LaTeX; checks are Python/Node-based):
- `python3 tools/check_research.py` — validates research-folder conventions:
  every `sources.json` parses, every markdown link target inside
  `article_bo_machinery/research/` resolves, every `notes/` file has the
  required sections, LOG.md entries are well-formed.
- `node --check` on any `.js` workflow script under `research/`.
- `git status --porcelain` must be clean after the commit (nothing untracked
  left behind).

### 4. Log + commit
Append one entry to `LOG.md` per cycle (template below). Commit everything
touched, push to `origin/claude/machinery-confound-article`. A cycle with no
findings still logs (one line: "no change"), but does not commit.

## Research backlog

Ordered; the loop takes the topmost unchecked item. Add new questions to the
bottom as they arise — the loop may append, never delete.

- [x] **Q1. Loop architectures.** *(done — `notes/01_loop_architectures.md`)* Single-agent ReAct-style loops vs.
  orchestrator/worker fan-out; when deterministic control flow (scripted
  pipelines) beats model-driven control flow; stop conditions and
  loop-until-dry patterns. Sources: agent-engineering literature + our own
  novelty-check run as a case study.
- [ ] **Q2. Workflow graphs.** Pipeline vs. barrier semantics; where
  synchronization is genuinely needed (dedup, early-exit, cross-item
  comparison); adversarial-verification stages and judge panels as graph
  motifs; cost of barriers in wall-clock.
- [ ] **Q3. State and memory across iterations.** Append-only logs and
  journals as loop state; resumability and caching of completed nodes;
  idempotent cycles; markdown as the durable, human-auditable state store
  (what we're doing here — what do others do, what breaks at scale?).
- [ ] **Q4. Quality-control loops.** Critic/verifier patterns, N-vote
  refutation, perspective-diverse vs. redundant verification; convergence
  criteria (K consecutive dry rounds); how to keep verifiers from
  rubber-stamping. Compare with what the novelty check did.
- [ ] **Q5. Testing loops for research artifacts.** CI-style continuous
  checks for papers and research folders (link integrity, reference
  consistency, compile checks where toolchains exist); what "green" means
  for a document rather than a program.
- [ ] **Q6. Cost and model-tier engineering.** Assigning cheap models to
  mechanical stages and strong models to review/synthesis (our Sonnet-5
  searchers / Fable-5 reviewer split); token-budget-driven loop scaling;
  when a bigger fan-out stops paying.
- [ ] **Q7. Synthesis.** Fold Q1–Q6 into a single reference document
  (`LOOP_ENGINEERING.md`): our house patterns for research/writing/testing
  loops, each with rationale and a pointer to the note that established it.

## Output conventions

- Everything in markdown, in this folder; raw data (JSON, scripts) sits next
  to the note that interprets it. No binaries.
- Every note starts with: date, question, method (who searched, who
  reviewed), then findings with sources, then **What this changes for us**.
- Every cycle's log entry names the files it touched.
- Committed every cycle; the git history is part of the record.

## LOG.md entry template

```markdown
## Cycle N — YYYY-MM-DD
- **Question:** Qk (short name)
- **Did:** one-paragraph summary of research/write/test legs
- **Files:** paths touched
- **Tests:** check_research.py PASS/FAIL (+ details if FAIL)
- **Next:** what the next cycle should pick up
```

## Stop conditions

The loop ends when Q7's synthesis document exists and a full cycle produces
no new backlog items — or when the user says stop. If the environment loses
network access for search-driven questions, the loop falls back to
internal-evidence questions (Q3, Q4 case studies) rather than idling.

**Stall counter** (added Cycle 1, from Magentic-One's progress-ledger
pattern — see `notes/01_loop_architectures.md`): a cycle that ends with no
committed change is a stall. Two consecutive stalls stop the loop and report
to the user, rather than burning further cycles.
