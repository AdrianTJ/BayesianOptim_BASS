# =============================================================================
# bootstrap.R  --  Load the whole library with one call
# =============================================================================
# Since this is a plain "source the files" library (no package build step), we
# need a tiny loader that pulls in every piece in the right order. Both the
# entry-point runner and the test suite call source_library() so they never have
# to know the internal file list.
# =============================================================================

#' Source every library file under `lib_dir`.
#'
#' Loads the top-level modules first, then the objective definitions, then the
#' objective loader. `bootstrap.R` itself is skipped.
#'
#' @param lib_dir Path to the R/ library directory.
#' @return Invisibly NULL; called for its side effect of defining functions.
source_library <- function(lib_dir) {
  # Top-level modules, in dependency order (helpers before the loop that uses
  # them; objectives loader last because it references the objective functions).
  core <- c(
    "config.R",
    "candidates.R",
    "acquisition.R",
    "surrogates.R",
    "tpe.R",
    "bo_loop.R",
    "experiment.R"
  )
  for (f in core) source(file.path(lib_dir, f))

  # Objective definitions (utils first so the benchmarks can use them), then the
  # name -> objective loader.
  obj_dir <- file.path(lib_dir, "objectives")
  source(file.path(obj_dir, "objective_utils.R"))
  for (f in c("branin.R", "rastrigin.R", "synthetic.R", "categorical.R",
               "nlp_hpo.R")) {
    source(file.path(obj_dir, f))
  }
  source(file.path(lib_dir, "objectives.R"))

  invisible(NULL)
}
