skip_if_not_installed <- testthat::skip_if_not_installed

read_baseline_tsv <- function(name) {
  utils::read.delim(
    file.path(repo_root, "docs", "baseline", "reference_outputs", name),
    check.names = FALSE
  )
}

test_that("app entrypoint still boots as a shiny app object", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("plyr")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("OptimalCutpoints")

  app_obj <- suppressWarnings(source(file.path(repo_root, "app.R"), local = TRUE, chdir = TRUE)$value)
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("mROC outputs remain aligned with reference baselines", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("plyr")
  skip_if_not_installed("dplyr")

  mayo <- utils::read.table(file.path(repo_root, "data", "mayo.txt"), header = TRUE)
  pbc <- utils::read.table(file.path(repo_root, "data", "pbc.txt"), header = TRUE)

  mayo_current <- suppressWarnings(
    mROC(
      data = mayo,
      statusName = "censor",
      markerName = c("mayoscore4", "mayoscore5"),
      eventValue = 1,
      diseaseHigher = TRUE,
      advanced = TRUE,
      ci.method = "DeLong",
      se.method = "DeLong",
      alpha = 0.05
    )
  )$stats

  mayo_ref <- read_baseline_tsv("mayo_roc_stats.tsv")

  expect_equal(mayo_current$Marker, mayo_ref$Marker)
  expect_equal(mayo_current$AUC, mayo_ref$AUC, tolerance = 1e-6)
  expect_equal(mayo_current$SE.AUC, mayo_ref$SE.AUC, tolerance = 1e-6)
  expect_equal(mayo_current$LowerLimit, mayo_ref$LowerLimit, tolerance = 1e-6)
  expect_equal(mayo_current$`UpperLimit (*)`, mayo_ref$`UpperLimit (*)`, tolerance = 1e-6)

  pbc_current <- suppressWarnings(
    mROC(
      data = pbc,
      statusName = "status",
      markerName = c("bili", "chol", "albumin"),
      eventValue = 1,
      diseaseHigher = TRUE,
      advanced = TRUE,
      ci.method = "DeLong",
      se.method = "DeLong",
      alpha = 0.05
    )
  )$stats

  pbc_ref <- read_baseline_tsv("pbc_roc_stats.tsv")

  expect_equal(pbc_current$Marker, pbc_ref$Marker)
  expect_equal(pbc_current$AUC, pbc_ref$AUC, tolerance = 1e-6)
})

test_that("pAUC and Youden cutoff outputs remain aligned with baseline", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("OptimalCutpoints")

  mayo <- utils::read.table(file.path(repo_root, "data", "mayo.txt"), header = TRUE)

  pauc_current <- pAUC(
    data = mayo,
    markers = c("mayoscore4", "mayoscore5"),
    status = "censor",
    range = c(0.5, 1),
    criteria = "Sensitivity",
    correct = TRUE,
    direction = "<"
  )
  pauc_ref <- read_baseline_tsv("mayo_pauc.tsv")

  expect_equal(pauc_current$Marker, pauc_ref$Marker)
  expect_equal(pauc_current$`Partial AUC`, pauc_ref$`Partial AUC`, tolerance = 1e-6)

  cut_current <- suppressWarnings(
    OptimalCutpoints::optimal.cutpoints(
      X = "mayoscore4",
      status = "censor",
      tag.healthy = 0,
      methods = "Youden",
      data = mayo,
      direction = "<",
      control = OptimalCutpoints::control.cutpoints(),
      ci.fit = TRUE,
      conf.level = 0.95,
      trace = FALSE
    )
  )
  cut_tbl <- printCutOff2(cut_current)
  names(cut_tbl)[1] <- "Metric"
  cut_tbl$Value <- as.numeric(cut_tbl$Value)
  cut_tbl$Lower_Limit <- as.numeric(cut_tbl$Lower_Limit)
  cut_tbl$Upper_Limit <- as.numeric(cut_tbl$Upper_Limit)

  cut_ref <- read_baseline_tsv("mayo_cutoff_youden.tsv")
  names(cut_ref)[1] <- "Metric"
  cut_ref$Value <- as.numeric(cut_ref$Value)
  cut_ref$Lower_Limit <- as.numeric(cut_ref$Lower_Limit)
  cut_ref$Upper_Limit <- as.numeric(cut_ref$Upper_Limit)

  expect_equal(cut_tbl$Metric, cut_ref$Metric)
  expect_equal(cut_tbl$Value, cut_ref$Value, tolerance = 1e-6)
  expect_equal(cut_tbl$Lower_Limit, cut_ref$Lower_Limit, tolerance = 1e-6)
  expect_equal(cut_tbl$Upper_Limit, cut_ref$Upper_Limit, tolerance = 1e-6)
})
