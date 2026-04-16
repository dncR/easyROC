create_download_handler_spec <- function(filename, content, content_type = NULL) {
  list(
    filename = filename,
    content = content,
    content_type = content_type
  )
}

register_download_handlers <- function(output, specs) {
  if (!is.list(specs) || length(specs) == 0) {
    stop("specs must be a non-empty list of download handler specs", call. = FALSE)
  }

  for (id in names(specs)) {
    spec <- specs[[id]]

    if (!is.list(spec) || is.null(spec$filename) || is.null(spec$content)) {
      stop(paste0("Invalid download spec for id: ", id), call. = FALSE)
    }

    if (is.null(spec$content_type)) {
      output[[id]] <- shiny::downloadHandler(
        filename = spec$filename,
        content = spec$content
      )
    } else {
      output[[id]] <- shiny::downloadHandler(
        filename = spec$filename,
        content = spec$content,
        contentType = spec$content_type
      )
    }
  }
}
