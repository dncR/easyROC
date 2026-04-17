build_domain_test_data <- function() {
  data.frame(
    status = c(rep(0, 12), rep(1, 12)),
    marker = c(
      0.20, 0.35, 0.41, 0.44, 0.48, 0.53, 0.57, 0.61, 0.66, 0.71, 0.74, 0.79,
      0.31, 0.39, 0.46, 0.50, 0.55, 0.59, 0.63, 0.68, 0.72, 0.76, 0.81, 0.85
    )
  )
}

test_that("rocdata keeps direction-specific ROC boundary points", {
  dat <- build_domain_test_data()

  high <- suppressWarnings(
    rocdata(
      status = dat$status,
      marker = dat$marker,
      event = 1,
      higherValuesDiseased = TRUE,
      se.method = "DeLong",
      ci.method = "DeLong",
      advanced = TRUE
    )
  )

  low <- suppressWarnings(
    rocdata(
      status = dat$status,
      marker = dat$marker,
      event = 1,
      higherValuesDiseased = FALSE,
      se.method = "DeLong",
      ci.method = "DeLong",
      advanced = TRUE
    )
  )

  expect_true(is.infinite(high$roc$Cutpoint[1]) && high$roc$Cutpoint[1] > 0)
  expect_true(is.infinite(high$roc$Cutpoint[nrow(high$roc)]) && high$roc$Cutpoint[nrow(high$roc)] < 0)
  expect_equal(high$roc$FPR[c(1, nrow(high$roc))], c(0, 1))
  expect_equal(high$roc$TPR[c(1, nrow(high$roc))], c(0, 1))

  expect_true(is.infinite(low$roc$Cutpoint[1]) && low$roc$Cutpoint[1] < 0)
  expect_true(is.infinite(low$roc$Cutpoint[nrow(low$roc)]) && low$roc$Cutpoint[nrow(low$roc)] > 0)
  expect_equal(low$roc$FPR[c(1, nrow(low$roc))], c(0, 1))
  expect_equal(low$roc$TPR[c(1, nrow(low$roc))], c(0, 1))
})

test_that("rocdata forces DeLong metrics when advanced is FALSE", {
  dat <- build_domain_test_data()

  auto <- suppressWarnings(
    rocdata(
      status = dat$status,
      marker = dat$marker,
      event = 1,
      higherValuesDiseased = TRUE,
      se.method = "MW",
      ci.method = "Exact",
      advanced = FALSE
    )
  )

  delong <- suppressWarnings(
    rocdata(
      status = dat$status,
      marker = dat$marker,
      event = 1,
      higherValuesDiseased = TRUE,
      se.method = "DeLong",
      ci.method = "DeLong",
      advanced = TRUE
    )
  )

  expect_equal(auto$stats$auc, delong$stats$auc, tolerance = 1e-12)
  expect_equal(auto$stats$se.auc, delong$stats$se.auc, tolerance = 1e-12)
  expect_equal(auto$stats$ci.lower, delong$stats$ci.lower, tolerance = 1e-12)
  expect_equal(auto$stats$ci.upper, delong$stats$ci.upper, tolerance = 1e-12)
})

test_that("compute_roc_comparisons returns adjusted p-values for 3 markers", {
  skip_if_not_installed("plyr")
  skip_if_not_installed("dplyr")

  set.seed(42)
  status <- rep(c(0, 1), each = 20)
  dat <- data.frame(
    status = status,
    m1 = c(rnorm(20, mean = 0.40, sd = 0.18), rnorm(20, mean = 0.75, sd = 0.18)),
    m2 = c(rnorm(20, mean = 0.45, sd = 0.18), rnorm(20, mean = 0.70, sd = 0.18)),
    m3 = c(rnorm(20, mean = 0.50, sd = 0.18), rnorm(20, mean = 0.65, sd = 0.18))
  )

  cmp <- suppressWarnings(
    compute_roc_comparisons(
      data = dat,
      status_var = "status",
      marker_names = c("m1", "m2", "m3"),
      event_value = 1,
      lowhigh = TRUE,
      conf_int = "DeLong",
      std_err = "DeLong",
      advanced = TRUE,
      alpha = 0.05,
      multiple_method = "bonferroni"
    )
  )

  expect_s3_class(cmp, "data.frame")
  expect_equal(nrow(cmp), 3)
  expect_true("p-value (adj.)" %in% colnames(cmp))
  expect_true(all(!is.na(cmp$`p-value (adj.)`)))
})

test_that("parametricROC returns bounded outputs for asymptotic and exact CI", {
  dat <- build_domain_test_data()

  asym <- parametricROC(
    data = dat,
    marker = "marker",
    status = "status",
    event = 1,
    returnROCdata = TRUE,
    higherValuesPositives = TRUE,
    plot = FALSE,
    exact = FALSE,
    confidence.level = 0.95
  )

  exact <- parametricROC(
    data = dat,
    marker = "marker",
    status = "status",
    event = 1,
    returnROCdata = TRUE,
    higherValuesPositives = TRUE,
    plot = FALSE,
    exact = TRUE,
    confidence.level = 0.95
  )

  expect_true(is.list(asym))
  expect_true(all(c("plotdata", "stats") %in% names(asym)))
  expect_equal(nrow(asym$plotdata), 200)
  expect_true(all(asym$plotdata$FPR >= 0 & asym$plotdata$FPR <= 1))
  expect_true(all(asym$plotdata$TPR >= 0 & asym$plotdata$TPR <= 1))

  expect_true(asym$stats$AUC >= 0 && asym$stats$AUC <= 1)
  expect_true(asym$stats$Lower >= 0 && asym$stats$Lower <= 1)
  expect_true(asym$stats$Upper >= 0 && asym$stats$Upper <= 1)
  expect_true(asym$stats$Lower <= asym$stats$Upper)

  expect_true(exact$stats$Lower >= 0 && exact$stats$Lower <= 1)
  expect_true(exact$stats$Upper >= 0 && exact$stats$Upper <= 1)
  expect_true(exact$stats$Lower <= exact$stats$Upper)
})

test_that("sample size domain helpers guard invalid inputs", {
  expect_error(
    SampleSizeSingleTest(alpha = 0, power = 0.80, auc = 0.70, ratio = 1),
    "Type I error"
  )

  expect_error(
    SampleSizeTwoTests(
      alpha = 0.05,
      power = 0.80,
      auc01 = 0.80,
      auc02 = 0.78,
      auc11 = 0.84,
      auc12 = 0.76,
      ratio = 0
    ),
    "Allocation ratio"
  )

  expect_error(
    SampleSizeStandardvsNew(
      alpha = 0.05,
      power = 0.80,
      aucs = 0.80,
      aucn = 0.40,
      sd = 0.10,
      ratio = 1
    ),
    "AUC must be between 0.5 and 1"
  )
})

test_that("printCutOff2 returns normalized summary table", {
  perf <- list(
    Se = c(0.8123, 0.7345, 0.8899),
    Sp = c(0.7022, 0.6123, 0.7921),
    PPV = c(0.6555, 0.5444, 0.7444),
    NPV = c(0.8444, 0.7555, 0.9333),
    DLR.Positive = c(2.1, 1.8, 2.4),
    DLR.Negative = c(0.28, 0.19, 0.37)
  )

  mock <- list(list(list(list(cutoff = 0.44), perf)))

  out <- printCutOff2(mock)

  expect_s3_class(out, "data.frame")
  expect_equal(dim(out), c(6L, 4L))
  expect_equal(colnames(out), c("", "Value", "Lower_Limit", "Upper_Limit"))
  expect_equal(out[[1]], c(
    "Sensitivity",
    "Specificity",
    "Positive Predictive Value",
    "Negative Predictive Value",
    "Positive Likelihood Ratio",
    "Negative Likelihood Ratio"
  ))
  expect_equal(out$Value[1], "0.812")
  expect_equal(out$Lower_Limit[6], "0.190")
})
