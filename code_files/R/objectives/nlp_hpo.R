# =============================================================================
# nlp_hpo.R  --  Real hyperparameter-optimization objective (NLP text classifier)
# =============================================================================
# The only benchmark in this work backed by a real machine-learning task rather
# than a synthetic surface. Each evaluation trains a small text classifier from
# scratch on AG News (see code_files/5_nlp_hpo/train_nlp.py) and returns its
# validation error, which is MINIMISED.
#
# Every one of the eight hyperparameters is a discrete choice, so the whole
# search space is categorical. Four are genuinely UNORDERED (architecture,
# optimizer, activation, pooling) and four are discretized-ORDERED (learning
# rate, embedding dimension, dropout, batch size). This mix is deliberate: it is
# what a real HPO space looks like, and it is honest about where a categorical
# surrogate can and cannot have an edge (the unordered coordinates are the ones a
# GP's ordered relaxation cannot represent faithfully).
#
# The objective is DETERMINISTIC in the configuration: the training seed is
# fixed inside the Python trainer, so f(config) is a fixed function over the
# categorical grid, exactly like Branin is a fixed function over its box. The
# trainer caches every (config -> error) to disk, so revisited configurations
# (common under categorical BO) and repeated seeds cost only interpreter startup.
# =============================================================================

# Value mapping lives in train_nlp.py (single source of truth); R only needs the
# number of levels per hyperparameter, in the trainer's HP_ORDER.
NLP_HPO_LEVELS <- c(
  arch       = 3L,   # meanpool, cnn, bilstm            (unordered)
  optimizer  = 4L,   # sgd, adam, rmsprop, adamw        (unordered)
  activation = 3L,   # relu, tanh, gelu                 (unordered)
  pooling    = 3L,   # mean, max, last                  (unordered)
  lr         = 5L,   # 1e-3 .. 1e-1                      (ordered)
  embed_dim  = 3L,   # 32, 64, 128                      (ordered)
  dropout    = 3L,   # 0.0, 0.3, 0.6                    (ordered)
  batch_size = 3L    # 32, 128, 512                     (ordered)
)

# Absolute paths so the objective works from any worker's cwd. Override the base
# with NLP_HPO_HOME if the repo lives elsewhere.
nlp_hpo_home <- function() {
  env <- Sys.getenv("NLP_HPO_HOME", unset = "")
  if (nzchar(env)) return(env)
  # source_library() records the code_files/ root; fall back to the working
  # directory only if the library was loaded some other way.
  file.path(getOption("bass.code_root", "code_files"), "5_nlp_hpo")
}

# In-process memo: under plan(sequential) the whole experiment runs in one R
# session, so repeated configurations (very common across methods and seeds on a
# categorical space) are served from this environment without ever launching the
# Python trainer. The trainer keeps its own on-disk cache too, so genuinely new
# configs are computed once and reused across separate runs.
NLP_HPO_MEMO <- new.env(parent = emptyenv())

#' One evaluation: decode a unit-cube row to level indices, train, return error.
nlp_hpo_eval_row <- function(u) {
  L   <- NLP_HPO_LEVELS
  # decode_levels returns 1..L; the trainer wants 0-based indices in HP_ORDER.
  lev <- vapply(seq_along(L), function(j) decode_levels(u[j], L[[j]]) - 1L, integer(1))
  key <- paste(lev, collapse = ",")
  if (!is.null(NLP_HPO_MEMO[[key]])) return(NLP_HPO_MEMO[[key]])
  home <- nlp_hpo_home()
  py   <- file.path(home, ".venv", "bin", "python")
  script <- file.path(home, "train_nlp.py")
  out <- system2(py, c(script, "--levels", paste(lev, collapse = ",")),
                 stdout = TRUE, stderr = FALSE)
  line <- grep("^RESULT ", out, value = TRUE)
  if (length(line) != 1L) {
    stop("nlp_hpo: trainer produced no RESULT line for levels ",
         paste(lev, collapse = ","))
  }
  val <- as.numeric(sub("^RESULT ", "", line))
  NLP_HPO_MEMO[[key]] <- val
  val
}

#' Build the NLP HPO objective: list(name, d, fn, schema).
make_nlp_hpo <- function() {
  d      <- length(NLP_HPO_LEVELS)
  schema <- list(types  = rep("cat", d),
                 levels = as.integer(NLP_HPO_LEVELS))
  fn <- function(X) {
    X <- as.matrix(X)
    if (is.null(nrow(X))) X <- matrix(X, nrow = 1)
    apply(X, 1, nlp_hpo_eval_row)
  }
  list(name = "nlp_hpo", d = d, fn = fn, schema = schema)
}
