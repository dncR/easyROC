test_that("mROC validates marker and status inputs", {
  skip_if_not_installed("plyr")
  skip_if_not_installed("dplyr")

  dat <- data.frame(status = c(0, 1, 0, 1), m1 = c(0.2, 0.8, 0.3, 0.9))

  expect_error(
    mROC(data = dat, statusName = "status", markerName = NULL, event = 1),
    "Marker variable is not specified"
  )

  expect_error(
    mROC(data = dat, statusName = "status", markerName = "missing", event = 1),
    "Some marker variables are not in data"
  )

  expect_error(
    mROC(data = dat, statusName = "missing", markerName = "m1", event = 1),
    "statusName must reference an existing column"
  )
})

test_that("mROC supports both event and eventValue arguments", {
  skip_if_not_installed("plyr")
  skip_if_not_installed("dplyr")

  dat <- data.frame(
    status = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    m1 = c(0.30, 0.70, 0.42, 0.58, 0.33, 0.62, 0.48, 0.55, 0.45, 0.60, 0.38, 0.63)
  )

  res_event <- suppressWarnings(
    mROC(
      data = dat,
      statusName = "status",
      markerName = "m1",
      event = 1,
      advanced = TRUE,
      ci.method = "DeLong",
      se.method = "DeLong"
    )
  )

  res_event_value <- suppressWarnings(
    mROC(
      data = dat,
      statusName = "status",
      markerName = "m1",
      eventValue = 1,
      advanced = TRUE,
      ci.method = "DeLong",
      se.method = "DeLong"
    )
  )

  expect_equal(res_event$stats$AUC, res_event_value$stats$AUC)
  expect_equal(res_event$plotdata$TPR, res_event_value$plotdata$TPR)
})
