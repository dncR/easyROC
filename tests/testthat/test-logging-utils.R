source(file.path(repo_root, "R", "logging_utils.R"))

set_env_temp <- function(name, value = NULL) {
  old <- Sys.getenv(name, unset = NA_character_)
  if (is.null(value)) {
    Sys.unsetenv(name)
  } else {
    do.call(Sys.setenv, stats::setNames(list(value), name))
  }
  old
}

restore_env <- function(name, old) {
  if (is.na(old)) {
    Sys.unsetenv(name)
  } else {
    do.call(Sys.setenv, stats::setNames(list(old), name))
  }
}

test_that("easyroc_format_log_line builds structured key-value output", {
  line <- easyroc_format_log_line(
    level = "info",
    event = "upload_parsed",
    context = list(module = "mod_data_upload", rows = 42)
  )

  expect_match(line, "level=INFO")
  expect_match(line, "event=upload_parsed")
  expect_match(line, "module=mod_data_upload")
  expect_match(line, "rows=42")
})

test_that("easyroc_should_log respects EASYROC_LOG_LEVEL threshold", {
  old <- set_env_temp("EASYROC_LOG_LEVEL", "WARN")
  on.exit(restore_env("EASYROC_LOG_LEVEL", old), add = TRUE)

  expect_false(easyroc_should_log("INFO"))
  expect_true(easyroc_should_log("WARN"))
  expect_true(easyroc_should_log("ERROR"))
})

test_that("easyroc_log writes to EASYROC_LOG_FILE when configured", {
  tmp <- tempfile("easyroc-log-", fileext = ".log")
  old_level <- set_env_temp("EASYROC_LOG_LEVEL", "INFO")
  old_file <- set_env_temp("EASYROC_LOG_FILE", tmp)
  on.exit({
    restore_env("EASYROC_LOG_LEVEL", old_level)
    restore_env("EASYROC_LOG_FILE", old_file)
    unlink(tmp)
  }, add = TRUE)

  easyroc_log(
    level = "INFO",
    event = "session_started",
    context = list(module = "server", session = "abc123")
  )

  lines <- readLines(tmp, warn = FALSE)
  expect_true(length(lines) >= 1)
  expect_match(lines[[1]], "event=session_started")
  expect_match(lines[[1]], "session=abc123")
})
