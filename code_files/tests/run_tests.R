#!/usr/bin/env Rscript

# Convenience runner for the unit-test suite.
#   Rscript code_files/tests/run_tests.R
# (The library itself is sourced by testthat via helper-setup.R.)

library(testthat)

this_file <- sub("^--file=", "",
                 grep("^--file=", commandArgs(FALSE), value = TRUE))
suite_dir <- file.path(
  if (length(this_file)) dirname(normalizePath(this_file)) else getwd(),
  "testthat"
)

testthat::test_dir(suite_dir, stop_on_failure = TRUE)
