#!/usr/bin/env Rscript

options(warn = 1)

trim <- function(x) {
  sub("^[[:space:]]+|[[:space:]]+$", "", x)
}

check_required_files <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop(
      paste("Missing required file(s):", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
}

probe_http <- function(host = "127.0.0.1", port = 3838L, timeout = 2) {
  con <- socketConnection(
    host = host,
    port = as.integer(port),
    open = "r+b",
    blocking = TRUE,
    timeout = timeout
  )
  on.exit(close(con), add = TRUE)

  request <- c(
    "GET / HTTP/1.1",
    sprintf("Host: %s", host),
    "Connection: close",
    "",
    ""
  )

  writeBin(charToRaw(paste(request, collapse = "\r\n")), con)
  flush(con)

  status_line <- readLines(con, n = 1, warn = FALSE)
  if (length(status_line) == 0) {
    stop("No HTTP response received from Shiny app.", call. = FALSE)
  }

  code <- suppressWarnings(as.integer(strsplit(trim(status_line), " +")[[1]][2]))
  if (is.na(code) || code < 200 || code >= 500) {
    stop(
      sprintf("Unexpected HTTP status from Shiny app: %s", status_line),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

check_runtime_health <- function() {
  host <- Sys.getenv("EASYROC_HEALTHCHECK_HOST", "127.0.0.1")
  port <- suppressWarnings(as.integer(Sys.getenv("SHINY_PORT", "3838")))
  if (is.na(port) || port <= 0) {
    stop("SHINY_PORT is missing or invalid.", call. = FALSE)
  }

  probe_http(host = host, port = port, timeout = 2)
}

mode <- tolower(if (length(commandArgs(trailingOnly = TRUE)) > 0) {
  commandArgs(trailingOnly = TRUE)[1]
} else {
  "readiness"
})

if (!(mode %in% c("readiness", "liveness"))) {
  stop("Mode must be either 'readiness' or 'liveness'.", call. = FALSE)
}

if (mode == "readiness") {
  check_required_files(c("app.R", "ui.R", "server.R", "R/logging_utils.R"))
}

check_runtime_health()
cat(sprintf("easyROC %s check passed.\n", mode))

