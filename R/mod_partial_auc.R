compute_partial_auc_result <- function(data, marker_names, status_var, lowhigh,
                                       point_a, point_b, sens_spec) {
  if (is.null(data) || is.null(status_var) || status_var == "" ||
      is.null(marker_names) || length(marker_names) == 0) {
    return(NULL)
  }

  pAUC(
    data = data,
    range = c(point_a, point_b),
    criteria = sens_spec,
    correct = TRUE,
    percent = FALSE,
    markers = marker_names,
    status = status_var,
    direction = ifelse(lowhigh, "<", ">")
  )
}

mod_partial_auc_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::div(id = ns("root")))
}

mod_partial_auc_server <- function(id, shared_state = NULL, root_input = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(root_input)) {
      stop("root_input must be provided to mod_partial_auc_server", call. = FALSE)
    }

    if (is.null(shared_state)) {
      shared_state <- createSharedState()
    }
    validateSharedState(shared_state)

    is_active <- shiny::reactive({
      !is.null(root_input$markerInput) && root_input$tabs1 == "ROC curve"
    })

    pauc_result <- shiny::reactive({
      if (!is_active()) {
        return(NULL)
      }

      compute_partial_auc_result(
        data = shared_state$data(),
        marker_names = root_input$markerInput,
        status_var = shared_state$status_var(),
        lowhigh = root_input$lowhigh,
        point_a = root_input$pointA,
        point_b = root_input$pointB,
        sens_spec = root_input$sensSpec
      )
    })

    list(
      is_active = is_active,
      pauc_result = pauc_result
    )
  })
}
