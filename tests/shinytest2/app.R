library(shiny)

load_source_value <- function(path) {
  source(path, local = TRUE)$value
}

repo_root <- normalizePath(file.path(getwd(), "..", ".."), mustWork = TRUE)
setwd(repo_root)
options(shiny.testmode = TRUE)

ui <- load_source_value(file.path(repo_root, "ui.R"))
server_base <- load_source_value(file.path(repo_root, "server.R"))

server <- function(input, output, session) {
  setwd(repo_root)
  server_base(input, output, session)
}

shinyApp(ui = ui, server = server)
