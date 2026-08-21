export const meta = {
  name: 'novelty-check-machinery-confound',
  description: 'Prior-work sweep: is the machinery-confound article novel?',
  whenToUse: 'Novelty check for the BO machinery-confound article',
  phases: [
    { title: 'Search', detail: '5 angles, one web-search agent each', model: 'sonnet' },
    { title: 'Assess', detail: 'fetch top sources, rate novelty threat', model: 'sonnet' },
    { title: 'Verify', detail: 'adversarial check of flagged threats', model: 'sonnet' },
  ],
}

// ---------------------------------------------------------------------------
// Context shared with every agent: what the article claims as new.
// ---------------------------------------------------------------------------
const ARTICLE = `
CONTEXT: We are checking the novelty of a draft article, working title "The
Machinery Confound: Acquisition-Optimization Machinery Can Dominate Surrogate
Comparisons in Mixed and Categorical Bayesian Optimization". Its three claimed
contributions are:

C1 (oracle-ceiling audit): Replace the acquisition function's score with the
TRUE objective value inside a pool/candidate-based BO loop, so the loop always
picks the best admissible candidate in the pool. The resulting best-so-far
curve upper-bounds what ANY surrogate could achieve with that
candidate-generation machinery. Comparing this ceiling across machinery
variants (A/B on the candidate generator, dedup rule, etc.) isolates machinery
effects cheaply, with no surrogate fits. Proposed as a general diagnostic for
BO pipelines.

C2 (machinery dominates surrogate comparisons): Two demonstrated failure
modes in mixed/categorical BO: (a) a candidate generator whose local proposals
always force a categorical flip (never refining continuous coordinates while
keeping the incumbent's categorical combination) caps even a perfect surrogate
far from the optimum; (b) duplicate detection at the encoding level rather
than the decoded-categorical-combination level silently re-spends a majority
of the evaluation budget re-evaluating known combinations. Both are invisible
in standard convergence plots and get misattributed to the surrogate.

C3 (machinery-controlled comparison protocol): For surrogate comparisons in
mixed/categorical spaces: share one schema-aware candidate generator across
all surrogates, deduplicate at the decoded-combination level, pair seeds via
shared initial designs, report per-seed win/tie/loss with Wilcoxon signed-rank
tests, run the oracle-ceiling audit per machinery variant before attributing
anything to a surrogate, and scale benchmark instances so at least one is
solvable within budget.

A source THREATENS novelty only if it already does substantially the same
thing: e.g. it replaces the acquisition with the true objective (or an oracle)
to bound/audit the search machinery; or it empirically demonstrates that the
acquisition-OPTIMIZATION machinery (candidate pools, local search, dedup)
confounds surrogate comparisons in mixed/categorical spaces; or it proposes a
comparable machinery-controlled benchmarking protocol. Papers that merely (i)
study acquisition-function maximization quality in continuous spaces, (ii)
propose new mixed-space BO methods with bespoke machinery, or (iii) critique
BO benchmarking generally (baselines, seeds, budgets) are RELATED WORK, not
novelty threats -- classify them as low/none but still record them, they are
useful for the related-work section.
`

const ANGLES = [
  { key: 'acq-opt', query: 'importance of acquisition function optimization inner loop in Bayesian optimization: multi-start vs random candidates, candidate pool size effects, "maximizing acquisition functions" (Wilson et al.), papers arguing the acquisition optimizer matters as much as the surrogate' },
  { key: 'benchmarking', query: 'critiques of Bayesian optimization / HPO benchmarking and reproducibility: Eggensperger, Turner NeurIPS 2020 black-box challenge, pitfalls in comparing HPO methods, confounds in surrogate/method comparisons, benchmarking protocol papers (HPOBench, Bencher, etc.)' },
  { key: 'mixed-cat', query: 'mixed and categorical Bayesian optimization methods and their acquisition-optimization machinery: SMAC local search neighborhoods, TPE, CoCaBO, Casmopolitan, COMBO, BODi, Bounce, MerCBO -- any paper that ABLATES or isolates the candidate-generation/local-search machinery separately from the surrogate' },
  { key: 'oracle', query: 'oracle or perfect-surrogate upper bounds in Bayesian optimization or model-based optimization: replacing the acquisition or surrogate with the true objective, "oracle baseline", regret decomposition into surrogate error vs acquisition-optimization error, diagnosing BO pipeline components' },
  { key: 'dedup', query: 'duplicate or repeated evaluations in discrete/combinatorial/categorical Bayesian optimization: wasted budget re-evaluating the same configuration, deduplication of candidates, rounding continuous relaxations to repeated integer/categorical points (e.g. Garrido-Merchan Hernandez-Lobato)' },
]

// ---------------------------------------------------------------------------
const SEARCH_SCHEMA = {
  type: 'object', required: ['results'],
  properties: { results: { type: 'array', items: {
    type: 'object', required: ['url', 'title', 'relevance'],
    properties: {
      url: { type: 'string' }, title: { type: 'string' },
      venue_year: { type: 'string' },
      relevance: { type: 'string', description: 'one sentence: why this might threaten or inform the article novelty' },
      suspected_threat: { type: 'boolean', description: 'true if this looks like it might already do what the article claims (C1-C3)' },
    } } } },
}

const ASSESS_SCHEMA = {
  type: 'object', required: ['sources'],
  properties: { sources: { type: 'array', items: {
    type: 'object', required: ['url', 'accessible', 'threat', 'summary'],
    properties: {
      url: { type: 'string' }, title: { type: 'string' },
      accessible: { type: 'boolean' },
      summary: { type: 'string', description: '2-4 sentences: what the source actually does' },
      threat: { type: 'string', enum: ['high', 'medium', 'low', 'none'] },
      threat_reason: { type: 'string', description: 'which claimed contribution (C1/C2/C3) it touches and how' },
      key_quotes: { type: 'array', items: { type: 'string' }, description: 'verbatim quotes supporting the threat rating' },
    } } } },
}

const VERIFY_SCHEMA = {
  type: 'object', required: ['url', 'verdict', 'reasoning'],
  properties: {
    url: { type: 'string' },
    verdict: { type: 'string', enum: ['confirmed_threat', 'downgraded', 'inaccessible'] },
    reasoning: { type: 'string' },
    decisive_evidence: { type: 'string', description: 'verbatim quote or concrete section reference that decides it' },
    what_the_article_must_cite_or_differentiate: { type: 'string' },
  },
}

// ---------------------------------------------------------------------------
phase('Search')
log('Fanning out 5 search angles (Sonnet agents)')
const searchResults = await parallel(ANGLES.map(a => () =>
  agent(`${ARTICLE}
You are a literature-search agent. Search the web (use the WebSearch tool,
multiple queries; also try arXiv, OpenReview, Google Scholar-indexed pages)
for the angle below. Return the 6-10 most relevant sources as structured
results. Prefer primary sources (arXiv abstract pages, proceedings pages)
over blogs. Do NOT fetch full papers; titles+abstracts are enough at this
stage. Flag suspected_threat=true generously -- a later stage double-checks.

ANGLE (${a.key}): ${a.query}`,
    { label: `search:${a.key}`, phase: 'Search', schema: SEARCH_SCHEMA, model: 'sonnet' })
))

// Barrier justified: URL-dedup needs all search results at once.
const seen = new Set()
const candidates = []
for (const r of searchResults.filter(Boolean)) {
  for (const s of r.results || []) {
    const k = (s.url || '').replace(/^https?:\/\//, '').replace(/\/$/, '').toLowerCase()
    if (!k || seen.has(k)) continue
    seen.add(k)
    candidates.push(s)
  }
}
log(`${candidates.length} unique sources found; assessing suspected threats first`)

// Rank: suspected threats first, cap at 15 sources, chunk into 5 groups of 3.
candidates.sort((x, y) => (y.suspected_threat === true) - (x.suspected_threat === true))
const top = candidates.slice(0, 15)
const chunks = []
for (let i = 0; i < top.length; i += 3) chunks.push(top.slice(i, i + 3))

// ---------------------------------------------------------------------------
// Pipeline: each chunk is assessed, and its flagged sources verified, without
// waiting for other chunks.
// ---------------------------------------------------------------------------
const assessed = []
const verdicts = []
await pipeline(
  chunks,
  (chunk, _item, i) => agent(`${ARTICLE}
You are a source-assessment agent. For EACH of the sources below: fetch it
(WebFetch the URL; for arXiv prefer the /abs/ page, fetch the full text or
HTML version only if the abstract is ambiguous), summarize what it actually
does, and rate the novelty threat to the article per the definition above.
Rate 'high' only if the source substantially pre-empts C1, C2, or C3; use
'medium' for partial overlap that the article must explicitly differentiate;
'low'/'none' for ordinary related work. Include verbatim key_quotes for any
high/medium rating. If a URL is unreachable, mark accessible=false and rate
from the title alone with threat based on your best judgment.

SOURCES:
${chunk.map(s => `- ${s.title} :: ${s.url} :: ${s.relevance}`).join('\n')}`,
    { label: `assess:${i + 1}`, phase: 'Assess', schema: ASSESS_SCHEMA, model: 'sonnet' }),
  async (result, _item, i) => {
    if (!result) return null
    assessed.push(...(result.sources || []))
    const flagged = (result.sources || []).filter(s => s.threat === 'high' || s.threat === 'medium')
    const vs = await parallel(flagged.map(s => () =>
      agent(`${ARTICLE}
You are an adversarial verifier. A first-pass agent rated this source a
'${s.threat}' novelty threat, saying: "${s.threat_reason}". Its summary:
"${s.summary}". Quotes: ${JSON.stringify(s.key_quotes || [])}.

Your job: try to REFUTE the threat rating. Fetch the source yourself
(WebFetch ${s.url}; for arXiv also try the full text) and check whether it
truly pre-empts the article's C1/C2/C3 as defined above, or whether the
overlap is superficial (different question, continuous-only, no oracle-style
audit, no machinery-controlled protocol). Return 'confirmed_threat' only if
the source genuinely does substantially the same thing; 'downgraded' if the
overlap is partial or superficial (explain exactly what differs); and in
either case say what the article must cite or differentiate.`,
      { label: `verify:${(s.title || s.url).slice(0, 40)}`, phase: 'Verify', schema: VERIFY_SCHEMA, model: 'sonnet' })
    ))
    verdicts.push(...vs.filter(Boolean))
    return true
  }
)

log(`Assessment done: ${assessed.length} sources assessed, ${verdicts.length} flagged sources adversarially verified`)
return {
  angle_count: ANGLES.length,
  unique_candidates: candidates.length,
  assessed_count: assessed.length,
  dropped_beyond_cap: Math.max(0, candidates.length - 15),
  all_candidates: candidates,
  assessed,
  verdicts,
}