# Loaded automatically by testthat before any test file runs.
# Finds the R/ library by walking up from the current directory until it sees
# R/bootstrap.R, then sources the whole library so the tests can call any
# function directly. This keeps the tests independent of where they are launched.

find_lib_dir <- function(start = getwd()) {
  dir <- normalizePath(start)
  repeat {
    candidate <- file.path(dir, "R", "bootstrap.R")
    if (file.exists(candidate)) return(file.path(dir, "R"))
    parent <- dirname(dir)
    if (parent == dir) stop("Could not locate the R/ library directory.")
    dir <- parent
  }
}

lib_dir <- find_lib_dir()
source(file.path(lib_dir, "bootstrap.R"))
source_library(lib_dir)
