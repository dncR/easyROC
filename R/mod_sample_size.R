mod_sample_size_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(id = ns("root"))
  )
}

compute_sample_size_result <- function(root_input) {
  method <- as.character(root_input$sampleSizeMethod)

  if (method == "1") {
    return(SampleSizeSingleTest(
      alpha = root_input$alpha1,
      power = root_input$power1,
      auc = root_input$auc,
      ratio = root_input$ratio
    ))
  }

  if (method == "2") {
    return(SampleSizeTwoTests(
      alpha = root_input$alpha2,
      power = root_input$power2,
      auc01 = root_input$auc01,
      auc02 = root_input$auc02,
      auc11 = root_input$auc11,
      auc12 = root_input$auc12,
      ratio = root_input$ratio2
    ))
  }

  if (method == "3") {
    return(SampleSizeStandardvsNew(
      alpha = root_input$alpha3,
      power = root_input$power3,
      aucs = root_input$aucs,
      aucn = root_input$aucn,
      sd = root_input$sd,
      ratio = root_input$ratio3
    ))
  }

  NULL
}

compute_sample_size_lines <- function(root_input) {
  utils::capture.output(compute_sample_size_result(root_input))
}

mod_sample_size_server <- function(id, shared_state = NULL, root_input = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(root_input)) {
      stop("root_input must be provided to mod_sample_size_server", call. = FALSE)
    }

    is_active <- shiny::reactive({
      root_input$tabs1 == "Sample size"
    })

    sample_size_result <- shiny::reactive({
      if (!is_active()) {
        return(NULL)
      }
      compute_sample_size_result(root_input = root_input)
    })

    sample_size_lines <- shiny::reactive({
      if (!is_active()) {
        return(NULL)
      }
      compute_sample_size_lines(root_input = root_input)
    })

    list(
      shared_state = shared_state,
      is_active = is_active,
      sample_size_result = sample_size_result,
      sample_size_lines = sample_size_lines
    )
  })
}
