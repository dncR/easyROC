easyroc_log_levels <- c("DEBUG", "INFO", "WARN", "ERROR")

easyroc_coerce_log_level <- function(level) {
  normalized <- toupper(as.character(level %||% "INFO"))
  if (!(normalized %in% easyroc_log_levels)) {
    return("INFO")
  }
  normalized
}

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) {
    return(y)
  }
  x
}

easyroc_current_log_level <- function() {
  easyroc_coerce_log_level(Sys.getenv("EASYROC_LOG_LEVEL", "INFO"))
}

easyroc_should_log <- function(level) {
  target <- easyroc_coerce_log_level(level)
  configured <- easyroc_current_log_level()
  match(target, easyroc_log_levels) >= match(configured, easyroc_log_levels)
}

easyroc_escape_log_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return("\"\"")
  }

  scalar <- paste(as.character(value), collapse = ",")
  scalar <- gsub("[\r\n\t]+", " ", scalar)

  if (grepl("[[:space:]=\"']", scalar)) {
    escaped <- gsub("\"", "\\\\\"", scalar)
    return(paste0("\"", escaped, "\""))
  }

  scalar
}

easyroc_normalize_log_context <- function(context) {
  if (is.null(context)) {
    return(list())
  }
  if (!is.list(context)) {
    return(list(value = as.character(context)))
  }

  out <- list()
  for (name in names(context)) {
    key <- if (is.null(name) || name == "") "value" else name
    value <- context[[name]]
    if (inherits(value, "condition")) {
      value <- conditionMessage(value)
    }
    out[[key]] <- value
  }
  out
}

easyroc_format_log_line <- function(level, event, context = list()) {
  event_value <- as.character(event %||% "unknown_event")
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  parts <- c(
    paste0("ts=", timestamp),
    paste0("level=", easyroc_coerce_log_level(level)),
    paste0("event=", easyroc_escape_log_value(event_value))
  )

  normalized_context <- easyroc_normalize_log_context(context)
  if (length(normalized_context) > 0) {
    for (key in names(normalized_context)) {
      parts <- c(parts, paste0(key, "=", easyroc_escape_log_value(normalized_context[[key]])))
    }
  }

  paste(parts, collapse = " ")
}

easyroc_emit_log <- function(line) {
  cat(line, "\n", file = stderr(), sep = "")

  log_file <- Sys.getenv("EASYROC_LOG_FILE", "")
  if (nzchar(log_file)) {
    tryCatch(
      cat(line, "\n", file = log_file, append = TRUE, sep = ""),
      error = function(e) {
        cat(
          "ts=",
          format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          " level=WARN event=\"log_file_write_failed\" reason=",
          easyroc_escape_log_value(conditionMessage(e)),
          "\n",
          file = stderr(),
          sep = ""
        )
      }
    )
  }

  invisible(line)
}

easyroc_log <- function(level = "INFO", event = "event", context = list()) {
  if (!easyroc_should_log(level)) {
    return(invisible(NULL))
  }

  line <- easyroc_format_log_line(level = level, event = event, context = context)
  easyroc_emit_log(line)
}

