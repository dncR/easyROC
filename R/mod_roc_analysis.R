compute_roc_statistics <- function(data, status_var, marker_names, event_value, lowhigh,
                                   roc_estimation_type, conf_int, std_err, advanced,
                                   alpha, conf_int_parametric, alpha_parametric) {
  if (is.null(data) || is.null(status_var) || status_var == "" ||
      is.null(marker_names) || length(marker_names) == 0 ||
      is.null(event_value) || event_value == "") {
    return(NULL)
  }

  if (roc_estimation_type == "nonParametricROC") {
    return(
      mROC(
        data = data,
        statusName = status_var,
        markerName = marker_names,
        event = event_value,
        diseaseHigher = lowhigh,
        ci.method = conf_int,
        se.method = std_err,
        advanced = advanced,
        alpha = alpha
      )$stats
    )
  }

  tmp <- lapply(marker_names, function(marker) {
    parametricROC(
      data = data,
      marker = marker,
      status = status_var,
      event = event_value,
      returnROCdata = TRUE,
      higherValuesPositives = lowhigh,
      confidence.level = 1 - alpha_parametric,
      plot = FALSE,
      exact = isTRUE(conf_int_parametric == "Exact")
    )$stats
  })
  names(tmp) <- marker_names

  tmp <- plyr::ldply(tmp, rbind)[, -1, drop = FALSE]
  tmp <- tmp[, -c(2:5), drop = FALSE]
  colnames(tmp) <- c("Marker", "AUC", "SE.AUC", "LowerLimit", "UpperLimit (*)", "z", "p-value")
  tmp
}

compute_roc_plot_bundle <- function(data, status_var, marker_names, event_value, lowhigh,
                                    roc_estimation_type, alpha_parametric) {
  if (is.null(data) || is.null(status_var) || status_var == "" ||
      is.null(marker_names) || length(marker_names) == 0 ||
      is.null(event_value) || event_value == "") {
    return(NULL)
  }

  if (roc_estimation_type == "nonParametricROC") {
    return(
      mROC(
        data = data,
        statusName = status_var,
        markerName = marker_names,
        event = event_value,
        diseaseHigher = lowhigh
      )
    )
  }

  tmp <- lapply(marker_names, function(marker) {
    plot_data <- parametricROC(
      data = data,
      marker = marker,
      status = status_var,
      event = event_value,
      returnROCdata = TRUE,
      higherValuesPositives = lowhigh,
      confidence.level = 1 - alpha_parametric,
      plot = FALSE,
      exact = FALSE
    )$plotdata

    plot_data <- round(plot_data, 4)
    plot_data <- plot_data[order(plot_data$FPR, plot_data$TPR), ]

    if (lowhigh) {
      plot_data <- rbind(data.frame(Cutpoint = Inf, FPR = 0, TPR = 0), plot_data)
      plot_data <- rbind(plot_data, data.frame(Cutpoint = -Inf, FPR = 1, TPR = 1))
    } else {
      plot_data <- rbind(data.frame(Cutpoint = -Inf, FPR = 0, TPR = 0), plot_data)
      plot_data <- rbind(plot_data, data.frame(Cutpoint = Inf, FPR = 1, TPR = 1))
    }

    plot_data
  })
  names(tmp) <- marker_names

  tmp <- plyr::ldply(tmp, rbind)
  colnames(tmp)[1] <- "Marker"
  tmp <- dplyr::arrange(tmp, Marker, Cutpoint)
  list(plotdata = as.data.frame(tmp))
}

compute_roc_comparisons <- function(data, status_var, marker_names, event_value, lowhigh,
                                    conf_int, std_err, advanced, alpha, multiple_method) {
  if (is.null(data) || is.null(status_var) || status_var == "" ||
      is.null(marker_names) || length(marker_names) < 2 ||
      is.null(event_value) || event_value == "") {
    return(NULL)
  }

  combs <- data.frame(combn(length(marker_names), 2))
  n_col <- ifelse(length(marker_names) > 2, 9, 8)
  comparisons <- data.frame(matrix(NA, nrow = ncol(combs), ncol = n_col))

  if (n_col == 9) {
    colnames(comparisons) <- c(
      "Marker1 (I)", "Marker2 (J)", "AUC(I)", "AUC(J)", "|I - J|",
      "SE(|I - J|)", "z", "p-value", "p-value (adj.)"
    )
  } else {
    colnames(comparisons) <- c(
      "Marker1 (I)", "Marker2 (J)", "AUC(I)", "AUC(J)", "|I - J|",
      "SE(|I - J|)", "z", "p-value"
    )
  }

  comparisons[, 1] <- marker_names[as.numeric(combs[1, ])]
  comparisons[, 2] <- marker_names[as.numeric(combs[2, ])]

  stats <- mROC(
    data = data,
    statusName = status_var,
    markerName = marker_names,
    event = event_value,
    diseaseHigher = lowhigh,
    ci.method = conf_int,
    se.method = std_err,
    advanced = advanced,
    alpha = alpha
  )$stats
  stats_tmp <- stats[, 1:3]

  comparisons[, 3] <- stats_tmp[as.numeric(combs[1, ]), 2]
  comparisons[, 4] <- stats_tmp[as.numeric(combs[2, ]), 2]
  comparisons[, 5] <- abs(comparisons[, 4] - comparisons[, 3])
  comparisons[, 6] <- sqrt(stats_tmp[as.numeric(combs[1, ]), 3]^2 + stats_tmp[as.numeric(combs[2, ]), 3]^2)
  comparisons[, 7] <- comparisons[, 5] / comparisons[, 6]
  comparisons[, 8] <- 2 * (1 - pnorm(comparisons[, 7]))

  if (n_col == 9) {
    comparisons[, 9] <- p.adjust(comparisons[, 8], method = multiple_method)
  }

  comparisons[, -c(1, 2)] <- round(comparisons[, -c(1, 2)], 4)
  comparisons
}

mod_roc_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::div(id = ns("root")))
}

mod_roc_analysis_server <- function(id, shared_state = NULL, root_input = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(root_input)) {
      stop("root_input must be provided to mod_roc_analysis_server", call. = FALSE)
    }

    if (is.null(shared_state)) {
      shared_state <- createSharedState()
    }
    validateSharedState(shared_state)

    is_active <- shiny::reactive({
      !is.null(root_input$markerInput) && root_input$tabs1 == "ROC curve"
    })

    roc_statistics <- shiny::reactive({
      if (!is_active()) {
        return(NULL)
      }

      compute_roc_statistics(
        data = shared_state$data(),
        status_var = shared_state$status_var(),
        marker_names = root_input$markerInput,
        event_value = shared_state$event_value(),
        lowhigh = root_input$lowhigh,
        roc_estimation_type = root_input$rocEstimationType,
        conf_int = root_input$ConfInt,
        std_err = root_input$StdErr,
        advanced = root_input$advanced,
        alpha = root_input$alpha,
        conf_int_parametric = root_input$ConfIntParametric,
        alpha_parametric = root_input$alphaParametric
      )
    })

    roc_bundle <- shiny::reactive({
      if (!is_active()) {
        return(NULL)
      }

      compute_roc_plot_bundle(
        data = shared_state$data(),
        status_var = shared_state$status_var(),
        marker_names = root_input$markerInput,
        event_value = shared_state$event_value(),
        lowhigh = root_input$lowhigh,
        roc_estimation_type = root_input$rocEstimationType,
        alpha_parametric = root_input$alphaParametric
      )
    })

    roc_coordinates <- shiny::reactive({
      out <- roc_bundle()
      if (is.null(out)) {
        return(NULL)
      }
      out$plotdata
    })

    roc_comparisons <- shiny::reactive({
      if (!is_active()) {
        return(NULL)
      }

      compute_roc_comparisons(
        data = shared_state$data(),
        status_var = shared_state$status_var(),
        marker_names = root_input$markerInput,
        event_value = shared_state$event_value(),
        lowhigh = root_input$lowhigh,
        conf_int = root_input$ConfInt,
        std_err = root_input$StdErr,
        advanced = root_input$advanced,
        alpha = root_input$alpha,
        multiple_method = root_input$MultipleCompMethod
      )
    })

    list(
      is_active = is_active,
      roc_statistics = roc_statistics,
      roc_bundle = roc_bundle,
      roc_coordinates = roc_coordinates,
      roc_comparisons = roc_comparisons
    )
  })
}
