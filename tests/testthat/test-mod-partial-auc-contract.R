read_baseline_tsv_local <- function(name) {
  utils::read.delim(
    file.path(repo_root, "docs", "baseline", "reference_outputs", name),
    check.names = FALSE
  )
}

test_that("compute_partial_auc_result returns expected marker-level output", {
  skip_if_not_installed("pROC")

  mayo <- utils::read.table(file.path(repo_root, "data", "mayo.txt"), header = TRUE)

  out <- compute_partial_auc_result(
    data = mayo,
    marker_names = c("mayoscore4", "mayoscore5"),
    status_var = "censor",
    lowhigh = TRUE,
    point_a = 0.5,
    point_b = 1,
    sens_spec = "Sensitivity"
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_true(all(c("Marker", "Partial AUC") %in% colnames(out)))
})

test_that("compute_partial_auc_result stays aligned with baseline partial AUC", {
  skip_if_not_installed("pROC")

  mayo <- utils::read.table(file.path(repo_root, "data", "mayo.txt"), header = TRUE)
  baseline <- read_baseline_tsv_local("mayo_pauc.tsv")

  out <- compute_partial_auc_result(
    data = mayo,
    marker_names = c("mayoscore4", "mayoscore5"),
    status_var = "censor",
    lowhigh = TRUE,
    point_a = 0.5,
    point_b = 1,
    sens_spec = "Sensitivity"
  )

  expect_equal(out$Marker, baseline$Marker)
  expect_equal(out$`Partial AUC`, baseline$`Partial AUC`, tolerance = 1e-6)
})

test_that("compute_partial_auc_result returns NULL when required inputs are missing", {
  out <- compute_partial_auc_result(
    data = NULL,
    marker_names = c("m1"),
    status_var = "status",
    lowhigh = TRUE,
    point_a = 0.5,
    point_b = 1,
    sens_spec = "Sensitivity"
  )
  expect_null(out)
})
