createSharedState <- function() {
  list(
    data = shiny::reactiveVal(NULL),
    status_var = shiny::reactiveVal(NULL),
    event_value = shiny::reactiveVal(NULL),
    upload_error = shiny::reactiveVal(NULL)
  )
}

validateSharedState <- function(shared_state) {
  required_fields <- c("data", "status_var", "event_value", "upload_error")

  if (!is.list(shared_state) || !all(required_fields %in% names(shared_state))) {
    stop("shared_state must include: data, status_var, event_value, upload_error", call. = FALSE)
  }

  for (field in required_fields) {
    if (!is.function(shared_state[[field]])) {
      stop(paste0("shared_state$", field, " must be a reactiveVal function"), call. = FALSE)
    }
  }

  invisible(shared_state)
}
