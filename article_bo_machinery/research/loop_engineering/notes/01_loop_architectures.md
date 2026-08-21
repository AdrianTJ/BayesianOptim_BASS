# Q1 — Loop architectures

**Date:** 2026-08-21 · **Status:** DONE (two Sonnet 5 research agents,
~128k tokens / 53 tool calls total; reviewed and synthesized by Fable 5)

**Question:** Single-agent ReAct-style loops vs. orchestrator/worker fan-out;
when deterministic control flow (scripted pipelines) beats model-driven
control flow; stop conditions and loop-until-dry patterns.

**Method:** Two Sonnet 5 web-research agents (one on architecture families,
one on stop conditions/pacing), findings reviewed and synthesized by the
supervising Fable 5 session; plus an internal case study of this repository's
own novelty-check run (2026-08-21).

---

## Internal case study: the novelty-check run

What we already know from our own artifacts (`../../novelty_check/`,
`workflow_script.js`, `sources.json`):

- **Hybrid control flow worked.** The run scouted inline first (the
  supervising session read the repo and the article to define the five search
  angles and the C1–C3 threat definitions), then handed a fully deterministic
  script the fan-out: 5 search agents → barrier (URL dedup) → 5 assess agents
  → per-chunk adversarial verifiers. The model decided *what* to research;
  the script decided *how the graph executes*. No agent ever chose the next
  stage — which made the run auditable after the fact from one journal.
- **The single barrier was earned.** URL dedup genuinely needed all search
  results at once; everything downstream was pipelined per-chunk so
  verification of chunk 1 ran while chunk 3 was still being assessed.
- **Tiered models paid off.** 15 Sonnet agents did ~908k tokens of search,
  fetch, and refutation work; the (more expensive) supervising model spent
  its tokens only on scoping, reviewing verdicts, auditing the dropped-source
  list, and writing the report. The two places the supervisor added clear
  value: catching two mechanism-adjacent sources the cap had dropped, and
  weighing verifier evidence quality (code-grep vs. abstract-only).
- **Failure mode observed:** the 15-source cap silently dropped 24 candidates;
  only the supervisor's manual audit caught that two of them mattered. A
  scripted "no silent caps — log what was dropped" rule (which the script did
  follow, via `dropped_beyond_cap`) is what made that audit possible.

## External findings: architecture families

Reviewed synthesis of the Sonnet 5 agent's sweep (28 tool calls). Egress
policy blocked direct fetches of most publisher pages (anthropic.com, zenml,
bytebytego); only platform.claude.com was directly readable, so most claims
below rest on search-result snippets — usable for our purposes, but re-verify
before quoting anywhere public.

**The two poles.** Anthropic's canonical distinction: *workflows* orchestrate
models through predefined code paths (predictable, auditable); *agents* let
the model direct its own process (flexible, expensive to debug). Guidance:
start with the simplest workflow, add model-driven control only when the task
shape can't be known in advance
([Building Effective AI Agents](https://www.anthropic.com/engineering/building-effective-agents)).
Single-agent ReAct loops degrade over long horizons through three documented
mechanisms: *context rot* (recall degrades as tokens accumulate, before hard
limits), *progress stalls* (repeating equivalent actions), and *error
compounding* (a wrong assertion becomes ground truth for later steps; success
on 4+ hour autonomous tasks falls below 10% in one study).

**Fan-out economics.** Anthropic's production research system (lead agent →
3–5 parallel subagents with separately-scoped contexts → separate citation
pass) beat single-agent Opus by ~90% on their internal research eval at ~15×
the token cost. The repeated lesson: parallelism pays for **read-heavy,
breadth-first, independently decomposable** work; it fails on write-heavy,
shared-mutable-state work — one role-specialized coding topology spent more
tokens coordinating than working. Vague fan-out instructions cause duplicated
effort; explicit task boundaries per subagent are what prevent overlap.

**The hybrid middle.** The pattern that keeps recurring (LangGraph's
controlled cyclic graphs; "scout inline, then scripted fan-out"): use model
judgment for discovery and scoping, then hand a deterministic script the
execution graph — resumable, inspectable, with a separate verifier stage
unbiased by the implementation.

**Long-running mechanics.** The strongest production pattern for multi-session
work is *fresh context per session + persistent artifacts*: each session
starts with zero memory and reconstructs state from a progress file and git
history rather than carrying a long context forward
([Effective harnesses for long-running agents](https://www.anthropic.com/engineering/effective-harnesses-for-long-running-agents)).
Checkpoint every step so a crash at iteration 5 preserves 1–4; re-invocation
comes from the harness (cron, events, or a clamped self-scheduled wakeup),
not from the loop holding itself open.

## External findings: stop conditions and pacing

Reviewed synthesis of the Sonnet 5 agent's sweep (25 tool calls). The agent
flagged that several publisher pages (anthropic.com, a16z.com, mindstudio.ai)
were egress-blocked, so those claims rest on search-index snippets, not
full-page reads — re-confirm before quoting verbatim anywhere public.

**Stop conditions used by real systems.** Every mature framework pairs a
*semantic* stop with a *structural* ceiling: LangChain `AgentExecutor` has
`max_iterations`/`max_execution_time` plus an `early_stopping_method` that can
ask the model for one best-effort answer when the budget is hit
([docs](https://reference.langchain.com/python/langchain-classic/agents/agent/AgentExecutor/early_stopping_method));
the OpenAI Agents SDK defaults to `max_turns=10` and defines "done" purely
structurally — typed final output with no further tool calls
([docs](https://openai.github.io/openai-agents-python/running_agents/));
Google ADK's `LoopAgent` refuses to self-terminate — a sub-agent must signal
`escalate=True` under a hard `max_iterations` ceiling
([docs](https://google.github.io/adk-docs/agents/workflow-agents/loop-agents/)).
Deep-research systems combine "can a sufficiently confident answer be
synthesized?" with a max-rounds ceiling. Anthropic's long-running-agent
guidance treats hitting a budget as a *normal* stop: persist state plus a
"next resume instruction" so a fresh session continues cleanly.

**Pathologies.** The AutoGPT lineage's documented killers are recursive
self-verification ("not thorough enough → check again, forever"), inability
to notice repeated actions, and natural-language completion judgments that
default to "more work needed"
([vectara/awesome-agent-failures](https://github.com/vectara/awesome-agent-failures/blob/main/docs/case-studies/autogpt-planning-failures.md)).
Microsoft's Magentic-One formalizes the countermeasure as a **progress
ledger**: every round updates task state and a stall counter; more than ~2
non-progressing rounds forces a reset-and-replan
([Microsoft Research](https://www.microsoft.com/en-us/research/articles/magentic-one-a-generalist-multi-agent-system-for-solving-complex-tasks/)).
Exit checks get reward-hacked when the worker grades itself — the fix is a
separate verifier, exactly the split our novelty check used.

**Pacing.** The production pattern is hybrid: event-driven wakeups as the
fast path, reconciliation polling as the safety net, exponential backoff on
quiet sources with a reset-on-change, and idempotent handlers at every
external write. Returns on extra iterations are logarithmic and can go
negative (one benchmark: 1→10 samples +4.4 points, 10→20 +0.2; correction
loops plateau by ~attempt 5), so a diminishing-returns threshold over a
trailing window (e.g. improvement < ε across 3 cycles) is a better exit than
a fixed count.

## What this changes for us

1. **Our architecture is already the recommended one** — hybrid
   scout-then-script with fresh context per cycle and durable markdown/git
   state. The novelty check independently converged on the pattern the
   literature recommends; keep it, and keep the deterministic script (not the
   model) owning the execution graph on fan-outs.
2. **Adopt a stall counter (Magentic-One's progress ledger).** Our PLAN.md
   stop condition ("Q7 done + no new backlog items") had no guard against
   non-progressing cycles. Applied this cycle: PLAN.md now stops the loop
   after 2 consecutive cycles that produce no committed change, mirroring the
   ~2-round stall threshold and the diminishing-returns evidence.
3. **Keep the worker/verifier split.** Reward-hacked exit checks come from
   self-grading; our Sonnet-searches/Fable-review split is the documented
   mitigation. Never let a cycle's producing agent also be its acceptance
   check.
4. **Explicit task boundaries per subagent.** The duplicated-work failure
   (vague "research X" prompts) is why this cycle's two agents got disjoint
   briefs (architectures vs. stop conditions). Convention going forward:
   every fan-out prompt states what is *out* of its scope.
5. **LOG.md is our progress file.** The fresh-context-per-session pattern
   works only if the persisted artifacts are good enough to reconstruct
   state; each LOG entry's "Next:" line is the "next resume instruction" and
   should be written so a zero-context session could continue from it alone.
