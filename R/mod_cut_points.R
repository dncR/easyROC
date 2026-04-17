if (!exists("resolveTagHealthy", mode = "function")) {
  source("R/status_utils.R")
}
if (!exists("rocdata", mode = "function")) {
  source("R/rocdata.R")
}
if (!exists("mROC", mode = "function")) {
  source("R/mROC.R")
}

compute_cutoff_direction <- function(lowhigh) {
  ifelse(isTRUE(lowhigh), "<", ">")
}

compute_cutoff_control <- function(root_input) {
  method <- root_input$cutOffMethods

  if (method == "Youden") {
    return(OptimalCutpoints::control.cutpoints(
      CFP = root_input$CFP_Youden,
      CFN = root_input$CFN_Youden,
      generalized.Youden = root_input$generalized_Youden,
      costs.benefits.Youden = root_input$costs_benefits_Youden
    ))
  }
  if (method == "CB") {
    return(OptimalCutpoints::control.cutpoints(costs.ratio = root_input$costs_ratio))
  }
  if (method == "MCT") {
    return(OptimalCutpoints::control.cutpoints(CFP = root_input$CFP_MCT, CFN = root_input$CFN_MCT))
  }
  if (method == "MinValueSp") {
    return(OptimalCutpoints::control.cutpoints(valueSp = root_input$valueSp_MVSp))
  }
  if (method == "MinValueSe") {
    return(OptimalCutpoints::control.cutpoints(valueSe = root_input$valueSe_MVSe))
  }
  if (method == "ValueSe") {
    return(OptimalCutpoints::control.cutpoints(valueSe = root_input$valueSe_VSe))
  }
  if (method == "ValueSp") {
    return(OptimalCutpoints::control.cutpoints(valueSp = root_input$valueSp_VSp))
  }
  if (method == "MinValueSpSe") {
    return(OptimalCutpoints::control.cutpoints(
      valueSp = root_input$valueSp_MVSpSe,
      valueSe = root_input$valueSe_MVSpSe,
      maxSp = root_input$maxSp_MVSpSe
    ))
  }
  if (method == "MaxKappa") {
    return(OptimalCutpoints::control.cutpoints(
      CFP = root_input$CFP_MK,
      CFN = root_input$CFN_MK,
      weighted.Kappa = root_input$weighted_Kappa
    ))
  }
  if (method == "MaxEfficiency") {
    return(OptimalCutpoints::control.cutpoints(
      costs.benefits.Efficiency = root_input$costs_benefits_Efficiency,
      standard.deviation.accuracy = root_input$standard_deviation_accuracy
    ))
  }
  if (method == "MinValueNPV") {
    return(OptimalCutpoints::control.cutpoints(valueNPV = root_input$valueNPV_MVNPV))
  }
  if (method == "MinValuePPV") {
    return(OptimalCutpoints::control.cutpoints(valuePPV = root_input$valuePPV_MVPPV))
  }
  if (method == "ValueNPV") {
    return(OptimalCutpoints::control.cutpoints(valueNPV = root_input$valueNPV_VNPV))
  }
  if (method == "ValuePPV") {
    return(OptimalCutpoints::control.cutpoints(valuePPV = root_input$valuePPV_VPPV))
  }
  if (method == "MinValueNPVPPV") {
    return(OptimalCutpoints::control.cutpoints(
      valueNPV = root_input$valueNPV_MVNPVPPV,
      valuePPV = root_input$valuePPV_MVNPVPPV,
      maxNPV = root_input$maxNPV_MVNPVPPV
    ))
  }
  if (method == "ValueDLR.Negative") {
    return(OptimalCutpoints::control.cutpoints(valueDLR.Negative = root_input$valueDLR_Negative))
  }
  if (method == "ValueDLR.Positive") {
    return(OptimalCutpoints::control.cutpoints(valueDLR.Positive = root_input$valueDLR_Positive))
  }
  if (method == "MinPvalue") {
    return(OptimalCutpoints::control.cutpoints(adjusted.pvalue = root_input$adjusted_pvalue))
  }

  OptimalCutpoints::control.cutpoints()
}

resolve_cutoff_tag_healthy <- function(data, status_var, event_value) {
  if (is.null(data) || is.null(status_var) || status_var == "" ||
      is.null(event_value) || event_value == "") {
    return(NULL)
  }

  resolveTagHealthy(statusValues = data[, status_var], eventValue = event_value)
}

compute_optimal_cutpoint <- function(data, status_var, marker_name, event_value,
                                     cut_method, lowhigh, control_opts) {
  if (is.null(data) || is.null(status_var) || status_var == "" ||
      is.null(marker_name) || marker_name == "" ||
      is.null(cut_method) || cut_method == "" ||
      is.null(event_value) || event_value == "") {
    return(NULL)
  }

  tag_healthy <- resolve_cutoff_tag_healthy(
    data = data,
    status_var = status_var,
    event_value = event_value
  )
  if (is.null(tag_healthy)) {
    return(NULL)
  }

  OptimalCutpoints::optimal.cutpoints(
    X = marker_name,
    status = status_var,
    tag.healthy = tag_healthy,
    methods = cut_method,
    data = data,
    direction = compute_cutoff_direction(lowhigh),
    pop.prev = NULL,
    categorical.cov = NULL,
    control = control_opts,
    ci.fit = TRUE,
    conf.level = 0.95,
    trace = FALSE
  )
}

compute_cutoff_roc_coordinates <- function(data, status_var, marker_names, event_value, lowhigh) {
  if (is.null(data) || is.null(status_var) || status_var == "" ||
      is.null(marker_names) || length(marker_names) == 0 ||
      is.null(event_value) || event_value == "") {
    return(NULL)
  }

  mROC(
    data = data,
    statusName = status_var,
    markerName = marker_names,
    event = event_value,
    diseaseHigher = lowhigh
  )$plotdata
}

mod_cut_points_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::div(id = ns("root")))
}

mod_cut_points_server <- function(id, shared_state = NULL, root_input = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(root_input)) {
      stop("root_input must be provided to mod_cut_points_server", call. = FALSE)
    }

    if (is.null(shared_state)) {
      shared_state <- createSharedState()
    }
    validateSharedState(shared_state)

    is_active <- shiny::reactive({
      !is.null(root_input$markerInput) && root_input$tabs1 == "Cut points"
    })

    control_opts <- shiny::reactive({
      compute_cutoff_control(root_input = root_input)
    })

    optimal_cutpoint <- shiny::reactive({
      if (!is_active()) {
        return(NULL)
      }

      compute_optimal_cutpoint(
        data = shared_state$data(),
        status_var = shared_state$status_var(),
        marker_name = root_input$cutoffMarker,
        event_value = shared_state$event_value(),
        cut_method = root_input$cutOffMethods,
        lowhigh = root_input$lowhigh,
        control_opts = control_opts()
      )
    })

    cutoff_roc_coordinates <- shiny::reactive({
      if (!is_active()) {
        return(NULL)
      }

      compute_cutoff_roc_coordinates(
        data = shared_state$data(),
        status_var = shared_state$status_var(),
        marker_names = root_input$markerInput,
        event_value = shared_state$event_value(),
        lowhigh = root_input$lowhigh
      )
    })

    list(
      is_active = is_active,
      control_opts = control_opts,
      optimal_cutpoint = optimal_cutpoint,
      cutoff_roc_coordinates = cutoff_roc_coordinates
    )
  })
}
