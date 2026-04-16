test_that("create_download_handler_spec builds expected structure", {
  spec <- create_download_handler_spec(
    filename = function() "x.txt",
    content = function(file) writeLines("ok", file),
    content_type = "text/plain"
  )

  expect_true(is.list(spec))
  expect_true(all(c("filename", "content", "content_type") %in% names(spec)))
  expect_true(is.function(spec$filename))
  expect_true(is.function(spec$content))
  expect_equal(spec$content_type, "text/plain")
})

test_that("register_download_handlers validates input specs", {
  expect_error(
    register_download_handlers(output = list(), specs = list()),
    "non-empty list"
  )

  expect_error(
    register_download_handlers(output = list(), specs = list(bad = list())),
    "Invalid download spec"
  )
})
