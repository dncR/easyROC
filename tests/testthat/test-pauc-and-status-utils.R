test_that("pAUC validates inputs and returns expected shape", {
  skip_if_not_installed("pROC")

  dat <- data.frame(
    status = c(0, 1, 0, 1, 0, 1),
    m1 = c(0.1, 0.9, 0.2, 0.8, 0.3, 0.7),
    m2 = c(0.2, 0.7, 0.3, 0.8, 0.4, 0.9)
  )

  res <- pAUC(
    data = dat,
    markers = c("m1", "m2"),
    status = "status",
    range = c(0.5, 1),
    criteria = "Sensitivity",
    direction = "<"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_true(all(c("Marker", "Measure", "Value 1", "Value 2", "Partial AUC") %in% colnames(res)))

  expect_error(
    pAUC(data = dat, markers = "missing", status = "status", range = c(0.5, 1), criteria = "Sensitivity"),
    "markers must reference"
  )

  expect_error(
    pAUC(data = dat, markers = "m1", status = "status", range = c(1, 0.5), criteria = "Sensitivity"),
    "range must be a numeric vector"
  )
})

test_that("resolveTagHealthy handles numeric, factor, and character statuses", {
  numeric_status <- c(0, 1, 0, 1)
  factor_status <- factor(c("healthy", "case", "healthy", "case"))
  char_status <- c("healthy", "case", "healthy")

  out_num <- resolveTagHealthy(numeric_status, "1")
  out_fac <- resolveTagHealthy(factor_status, "case")
  out_chr <- resolveTagHealthy(char_status, "case")

  expect_equal(as.numeric(out_num), 0)
  expect_equal(as.character(out_fac), "healthy")
  expect_equal(out_chr, "healthy")

  out_empty <- resolveTagHealthy(NULL, "x")
  expect_equal(length(out_empty), 0)
})
