read_baseline_tsv_cut <- function(name) {
  utils::read.delim(
    file.path(repo_root, "docs", "baseline", "reference_outputs", name),
    check.names = FALSE
  )
}

test_that("compute_cutoff_direction maps lowhigh flag to expected symbols", {
  expect_equal(compute_cutoff_direction(TRUE), "<")
  expect_equal(compute_cutoff_direction(FALSE), ">")
  expect_equal(compute_cutoff_direction(NULL), ">")
})

test_that("compute_cutoff_control builds control object for known and default methods", {
  skip_if_not_installed("OptimalCutpoints")

  root_input <- list(
    cutOffMethods = "Youden",
    CFP_Youden = 1,
    CFN_Youden = 1,
    generalized_Youden = FALSE,
    costs_benefits_Youden = FALSE
  )
  ctrl_youden <- compute_cutoff_control(root_input)
  expect_true(is.list(ctrl_youden))

  root_input$cutOffMethods <- "UnknownMethod"
  ctrl_default <- compute_cutoff_control(root_input)
  expect_true(is.list(ctrl_default))
})

test_that("compute_optimal_cutpoint returns baseline-equivalent Youden output", {
  skip_if_not_installed("OptimalCutpoints")
  skip_if_not_installed("pROC")

  mayo <- utils::read.table(file.path(repo_root, "data", "mayo.txt"), header = TRUE)

  res <- suppressWarnings(
    compute_optimal_cutpoint(
      data = mayo,
      status_var = "censor",
      marker_name = "mayoscore4",
      event_value = 1,
      cut_method = "Youden",
      lowhigh = TRUE,
      control_opts = OptimalCutpoints::control.cutpoints()
    )
  )

  out <- printCutOff2(res)
  names(out)[1] <- "Metric"
  out$Value <- as.numeric(out$Value)
  out$Lower_Limit <- as.numeric(out$Lower_Limit)
  out$Upper_Limit <- as.numeric(out$Upper_Limit)

  baseline <- read_baseline_tsv_cut("mayo_cutoff_youden.tsv")
  names(baseline)[1] <- "Metric"
  baseline$Value <- as.numeric(baseline$Value)
  baseline$Lower_Limit <- as.numeric(baseline$Lower_Limit)
  baseline$Upper_Limit <- as.numeric(baseline$Upper_Limit)

  expect_equal(out$Metric, baseline$Metric)
  expect_equal(out$Value, baseline$Value, tolerance = 1e-6)
  expect_equal(out$Lower_Limit, baseline$Lower_Limit, tolerance = 1e-6)
  expect_equal(out$Upper_Limit, baseline$Upper_Limit, tolerance = 1e-6)
})

test_that("compute_optimal_cutpoint returns NULL for missing critical inputs", {
  out <- compute_optimal_cutpoint(
    data = NULL,
    status_var = "status",
    marker_name = "marker",
    event_value = 1,
    cut_method = "Youden",
    lowhigh = TRUE,
    control_opts = list()
  )

  expect_null(out)
})
