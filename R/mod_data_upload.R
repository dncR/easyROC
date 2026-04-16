mod_data_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(id = ns("root"))
  )
}

mod_data_upload_server <- function(id, shared_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    list(
      shared_state = shared_state
    )
  })
}
