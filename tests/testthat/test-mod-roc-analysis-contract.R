test_that("compute_roc_statistics returns expected shape for both estimation types", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("plyr")
  skip_if_not_installed("dplyr")

  mayo <- utils::read.table(file.path(repo_root, "data", "mayo.txt"), header = TRUE)

  nonparam <- suppressWarnings(
    compute_roc_statistics(
      data = mayo,
      status_var = "censor",
      marker_names = c("mayoscore4", "mayoscore5"),
      event_value = 1,
      lowhigh = TRUE,
      roc_estimation_type = "nonParametricROC",
      conf_int = "DeLong",
      std_err = "DeLong",
      advanced = TRUE,
      alpha = 0.05,
      conf_int_parametric = "asymptotic",
      alpha_parametric = 0.05
    )
  )

  expect_s3_class(nonparam, "data.frame")
  expect_equal(nrow(nonparam), 2)
  expect_true(all(c("Marker", "AUC", "SE.AUC") %in% colnames(nonparam)))

  param <- suppressWarnings(
    compute_roc_statistics(
      data = mayo,
      status_var = "censor",
      marker_names = c("mayoscore4", "mayoscore5"),
      event_value = 1,
      lowhigh = TRUE,
      roc_estimation_type = "parametricROC",
      conf_int = "DeLong",
      std_err = "DeLong",
      advanced = TRUE,
      alpha = 0.05,
      conf_int_parametric = "asymptotic",
      alpha_parametric = 0.05
    )
  )

  expect_s3_class(param, "data.frame")
  expect_equal(nrow(param), 2)
  expect_true(all(c("Marker", "AUC", "SE.AUC") %in% colnames(param)))
})

test_that("compute_roc_plot_bundle and comparisons keep ROC core outputs available", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("plyr")
  skip_if_not_installed("dplyr")

  mayo <- utils::read.table(file.path(repo_root, "data", "mayo.txt"), header = TRUE)

  bundle <- suppressWarnings(
    compute_roc_plot_bundle(
      data = mayo,
      status_var = "censor",
      marker_names = c("mayoscore4", "mayoscore5"),
      event_value = 1,
      lowhigh = TRUE,
      roc_estimation_type = "nonParametricROC",
      alpha_parametric = 0.05
    )
  )

  expect_true(is.list(bundle))
  expect_true("plotdata" %in% names(bundle))
  expect_s3_class(bundle$plotdata, "data.frame")
  expect_true(all(c("Marker", "FPR", "TPR") %in% colnames(bundle$plotdata)))

  comparisons <- suppressWarnings(
    compute_roc_comparisons(
      data = mayo,
      status_var = "censor",
      marker_names = c("mayoscore4", "mayoscore5"),
      event_value = 1,
      lowhigh = TRUE,
      conf_int = "DeLong",
      std_err = "DeLong",
      advanced = TRUE,
      alpha = 0.05,
      multiple_method = "bonferroni"
    )
  )

  expect_s3_class(comparisons, "data.frame")
  expect_equal(nrow(comparisons), 1)
  expect_true(all(c("Marker1 (I)", "Marker2 (J)", "p-value") %in% colnames(comparisons)))
})
