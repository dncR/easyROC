test_that("createSharedState creates expected reactive contract fields", {
  state <- createSharedState()

  expect_true(is.list(state))
  expect_true(all(c("data", "status_var", "event_value", "upload_error") %in% names(state)))
  expect_silent(validateSharedState(state))

  state$data(data.frame(status = c(0, 1), marker = c(0.1, 0.9)))
  state$status_var("status")
  state$event_value("1")
  state$upload_error("bad file")

  expect_equal(nrow(shiny::isolate(state$data())), 2)
  expect_equal(shiny::isolate(state$status_var()), "status")
  expect_equal(shiny::isolate(state$event_value()), "1")
  expect_equal(shiny::isolate(state$upload_error()), "bad file")
})

test_that("validateSharedState rejects invalid objects", {
  expect_error(
    validateSharedState(list()),
    "shared_state must include"
  )

  bad_state <- createSharedState()
  bad_state$data <- 123

  expect_error(
    validateSharedState(bad_state),
    "must be a reactiveVal function"
  )
})
