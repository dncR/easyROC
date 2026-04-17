#!/usr/bin/env Rscript

options(warn = 1)

scope_files <- c(
  "app.R",
  "ui.R",
  "server.R",
  list.files("R", pattern = "\\.R$", full.names = TRUE)
)

scope_files <- scope_files[file.exists(scope_files)]

rules <- list(
  list(
    id = "SC-001",
    pattern = ":::",
    message = "Non-exported namespace usage (:::) is not allowed in application code."
  ),
  list(
    id = "SC-002",
    pattern = "library\\((pROC|plyr|OptimalCutpoints)\\)",
    message = "Direct runtime library() calls for core analysis packages are not allowed."
  ),
  list(
    id = "SC-003",
    pattern = "setwd\\(",
    message = "setwd() is not allowed in production app code."
  )
)

violations <- character(0)

for (file in scope_files) {
  lines <- readLines(file, warn = FALSE)
  for (rule in rules) {
    idx <- grep(rule$pattern, lines, perl = TRUE)
    if (length(idx) == 0) {
      next
    }
    details <- sprintf("%s:%d [%s] %s", file, idx, rule$id, rule$message)
    violations <- c(violations, details)
  }
}

if (length(violations) > 0) {
  cat("Static checks failed:\n")
  cat(paste0("- ", violations, collapse = "\n"), "\n", sep = "")
  quit(status = 1)
}

cat(sprintf("Static checks passed: %d file(s) scanned.\n", length(scope_files)))
