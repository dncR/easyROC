find_chrome_path <- function() {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    return("")
  }

  out <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (length(out) == 0 || is.na(out)) {
    ""
  } else {
    out
  }
}

test_that("shinytest2 critical flows cover main UI tabs", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  chrome_path <- find_chrome_path()
  skip_if(!nzchar(chrome_path), "Chrome/Chromium is required for shinytest2")

  withr::local_envvar(c(NOT_CRAN = "true"))

  app <- shinytest2::AppDriver$new(
    app_dir = file.path(repo_root, "tests", "shinytest2"),
    name = "critical-flows",
    load_timeout = 90000,
    seed = 42,
    height = 1000,
    width = 1600
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  # Data upload flow (example data) + required selectors
  app$set_inputs(tabs1 = "Data upload")
  app$set_inputs(`data_upload-dataInput` = "1", `data_upload-sampleData` = "1")
  app$wait_for_idle()

  app$set_inputs(`data_upload-statusVar` = "censor")
  app$set_inputs(`data_upload-valueStatus` = "1")
  app$set_inputs(markerInput = c("mayoscore4", "mayoscore5"), cutoffMarker = "mayoscore4")
  app$wait_for_js("document.querySelectorAll('#RawData tbody tr').length > 0")

  # ROC statistics flow
  app$set_inputs(tabs1 = "ROC curve", navbarROCcurve = "Statistics")
  app$wait_for_js("document.querySelectorAll('#ROCstatistics tbody tr').length > 0")

  # Partial AUC flow
  app$set_inputs(navbarROCcurve = "Partial AUC", partialAUC = TRUE, pointA = 0.5, pointB = 1, sensSpec = "Sensitivity")
  app$wait_for_js("document.querySelectorAll('#resultPAuc tbody tr').length > 0")

  # Cut points flow
  app$set_inputs(tabs1 = "Cut points", cutOffMethods = "Youden", showPlots = FALSE)
  app$wait_for_js("document.querySelector('#cutPoints') && document.querySelector('#cutPoints').innerText.length > 0")
  cutoff_text <- app$get_text("#cutPoints")
  expect_match(cutoff_text, "Cut-off Results")

  # Sample size flow
  app$set_inputs(tabs1 = "Sample size", sampleSizeMethod = "1", alpha1 = 0.05, power1 = 0.80, auc = 0.60, ratio = 1)
  app$wait_for_js("document.querySelector('#SampleSizeForRoc') && document.querySelector('#SampleSizeForRoc').innerText.length > 0")
  ss_text <- app$get_text("#SampleSizeForRoc")
  expect_match(ss_text, "Sample size calculation", ignore.case = TRUE)
})
