#!/usr/bin/env Rscript

options(warn = 1)

if (!requireNamespace("lintr", quietly = TRUE)) {
  stop("Package 'lintr' is required. Install it with renv::install(\"lintr\").", call. = FALSE)
}

modern_r_targets <- function() {
  files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  keep_patterns <- c(
    "/mod_",
    "/data_input_utils\\.R$",
    "/shared_state\\.R$",
    "/status_utils\\.R$",
    "/plot_options_service\\.R$"
  )

  files[vapply(
    files,
    function(path) any(vapply(keep_patterns, grepl, logical(1), x = path)),
    logical(1)
  )]
}

test_targets <- function() {
  list.files("tests/testthat", pattern = "\\.R$", full.names = TRUE)
}

lint_targets <- unique(c(modern_r_targets(), test_targets()))

linters <- lintr::linters_with_defaults(
  object_name_linter = NULL,
  object_usage_linter = NULL,
  indentation_linter = NULL,
  line_length_linter = NULL,
  brace_linter = NULL,
  paren_body_linter = NULL,
  object_length_linter = NULL
)

lints <- unlist(
  lapply(lint_targets, function(path) lintr::lint(filename = path, linters = linters)),
  recursive = FALSE
)

if (length(lints) > 0) {
  print(lints)
  stop(sprintf("Lint failed: %d issue(s) found.", length(lints)), call. = FALSE)
}

cat(sprintf("Lint passed: %d file(s) scanned.\n", length(lint_targets)))
