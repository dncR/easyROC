mod_data_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::radioButtons(
      ns("dataInput"),
      "",
      list("Load example data" = 1, "Upload a file" = 2),
      selected = 1
    ),

    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == '1'", ns("dataInput")),
      shiny::h5(shiny::tags$b("Datasets:")),
      shiny::radioButtons(
        ns("sampleData"),
        "",
        list("Mayo data (n=312, p=4)" = 1, "PBC data set (n=418, p=20)" = 2),
        selected = 1
      ),
      shiny::tags$p(shiny::tags$b("n:"), " number of observations"),
      shiny::HTML("<p><b>p</b>: number of variables</p>")
    ),

    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == '2'", ns("dataInput")),
      shiny::HTML("<br>"),
      shiny::h5("Upload a delimited text file (max. 30MB): "),
      shiny::fileInput(ns("upload"), "", multiple = FALSE),
      shiny::radioButtons(
        ns("fileSepDF"),
        "Delimiter:",
        list("Comma" = 1, "Tab" = 2, "Semicolon" = 3, "Space" = 4),
        selected = 2
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] != '1'", ns("fileSepDF")),
        shiny::checkboxInput(ns("decimal"), "Use comma as decimal", value = FALSE)
      ),
      shiny::HTML("<br>"),
      shiny::HTML("<p>You can upload your data separated by comma, tab, semicolon or space.</p>"),
      shiny::HTML("<p><b>Note</b>: First row must be the header including the variable names.</p>"),
      shiny::uiOutput(ns("uploadValidationMessage"))
    ),

    shiny::HTML("<br>"),
    shiny::selectizeInput(ns("statusVar"), "Select status variable", choices = NULL, multiple = FALSE),
    shiny::selectizeInput(ns("valueStatus"), "Select category for cases", choices = NULL, multiple = FALSE)
  )
}

mod_data_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    uploadError <- shiny::reactiveVal(NULL)

    dataM <- shiny::reactive({
      data <- NULL

      if (input$dataInput == 1) {
        if (input$sampleData == 1) {
          data <- utils::read.table("data/mayo.txt", header = TRUE)
        } else if (input$sampleData == 2) {
          data <- utils::read.table("data/pbc.txt", header = TRUE)
        }
        uploadError(NULL)
      } else if (input$dataInput == 2) {
        inFile <- input$upload
        mySep <- switch(input$fileSepDF, "1" = ",", "2" = "\t", "3" = ";", "4" = "")

        if (is.null(inFile)) {
          uploadError(NULL)
          return(NULL)
        }

        parsed <- readDelimitedUpload(
          filePath = inFile$datapath,
          fileSize = inFile$size,
          sep = mySep,
          decimalComma = isTRUE(input$decimal)
        )

        if (!is.null(parsed$error)) {
          uploadError(parsed$error)
          return(NULL)
        }

        uploadError(NULL)
        data <- parsed$data
      }

      data
    })

    output$uploadValidationMessage <- shiny::renderUI({
      msg <- uploadError()
      if (is.null(msg) || msg == "") {
        return(NULL)
      }
      shiny::tags$p(style = "color:#b22222; font-weight:600; margin-top:8px;", msg)
    })

    shiny::observe({
      data_tmp <- dataM()
      if (!is.null(data_tmp)) {
        shiny::updateSelectInput(
          session = session,
          inputId = "statusVar",
          choices = colnames(data_tmp),
          selected = colnames(data_tmp)[1]
        )
      } else {
        shiny::updateSelectInput(
          session = session,
          inputId = "statusVar",
          choices = "",
          selected = ""
        )
      }
    })

    shiny::observe({
      data_tmp <- dataM()
      current_status <- input$statusVar
      if (!is.null(data_tmp) && !is.null(current_status) && current_status != "") {
        idx <- which(colnames(data_tmp) %in% current_status)
        categories <- levels(as.factor(as.character(data_tmp[, idx])))
        shiny::updateSelectizeInput(
          session = session,
          inputId = "valueStatus",
          choices = categories,
          selected = NULL
        )
      } else {
        shiny::updateSelectizeInput(
          session = session,
          inputId = "valueStatus",
          choices = "",
          selected = ""
        )
      }
    })

    list(
      data = dataM,
      status_var = shiny::reactive(input$statusVar),
      event_value = shiny::reactive(input$valueStatus)
    )
  })
}
