library(shiny)

load_source_value <- function(path) {
  source(path, local = TRUE)$value
}

ui <- load_source_value("ui.R")
server <- load_source_value("server.R")

shinyApp(ui = ui, server = server)
