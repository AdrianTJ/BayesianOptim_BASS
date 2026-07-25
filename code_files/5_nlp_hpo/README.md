# Real-task NLP hyperparameter-optimization objective

The one benchmark in this project backed by a real machine-learning task rather
than a synthetic surface. Each evaluation trains a small text classifier from
scratch on AG News and returns its validation error, which is minimised.

## Setup

The Python environment is not tracked in git (`.venv/` is ignored), so create it
once before running the benchmark:

```bash
cd code_files/5_nlp_hpo
uv sync          # pinned by uv.lock: python 3.11 + torch 2.13
```

The first evaluation downloads the AG News CSVs and caches the tokenised
tensors to `data_cache.pt`; every later run reads that cache and needs no
network. Individual `(configuration, validation error)` pairs are cached to
`eval_cache.json`, so repeated configurations cost only interpreter startup.
Both caches are ignored by git and regenerate on demand.

## Running

One evaluation by hand, either by naming the hyperparameters directly or by
passing level indices (the form the R pipeline uses):

```bash
uv run train_nlp.py --config '{"arch":"cnn","optimizer":"adam","activation":"relu","pooling":"max","lr":0.003,"embed_dim":128,"dropout":0.3,"batch_size":128}'
uv run train_nlp.py --levels 1,1,0,1,1,2,1,1
```

Either prints a single line, `RESULT <validation error>`.

The full comparison, from the repository root:

```bash
Rscript code_files/run_benchmark.R --objective=nlp_hpo \
  --budget=60 --reps=25 --with_tpe=true --workers=1
```

`--workers=1` is deliberate: the objective is served through the shared on-disk
cache, so the run is kept in a single process rather than raced across workers.
Expect the first full run to take several hours, dominated by the surrogate fits
rather than by training, and much less on any later run that reuses the cache.

## Design notes

The search space is entirely categorical (eight hyperparameters, 14,580
configurations) and deliberately mixes genuinely unordered choices
(architecture, optimiser, activation, pooling) with discretised-ordered ones
(learning rate, embedding dimension, dropout, batch size). See the experimental
chapter of the thesis for why that mix is the point of the benchmark.

The objective is deterministic in the configuration: the training seed is fixed
inside the trainer, so the validation error is a fixed function of the
configuration, exactly as the synthetic benchmarks are fixed functions of their
inputs. That is what makes the cache sound and the paired statistics valid.
`train_nlp.py` owns the mapping from level indices to hyperparameter values, so
the R and Python sides cannot disagree about which configuration is which.
