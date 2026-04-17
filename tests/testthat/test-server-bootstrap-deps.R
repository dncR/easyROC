test_that("server bootstrap no longer sources legacy domain scripts directly", {
  lines <- readLines(file.path(repo_root, "server.R"), warn = FALSE)

  legacy_source_patterns <- c(
    'source\\("R/mROC\\.R"\\)',
    'source\\("R/rocdata\\.R"\\)',
    'source\\("R/pAUC\\.R"\\)',
    'source\\("R/SampleSizeSingleTest\\.R"\\)',
    'source\\("R/SampleSizeTwoTests\\.R"\\)',
    'source\\("R/SampleSizeStandardvsNew\\.R"\\)',
    'source\\("R/status_utils\\.R"\\)',
    'source\\("R/parametricROC\\.R"\\)',
    'source\\("R/data_input_utils\\.R"\\)'
  )

  for (pattern in legacy_source_patterns) {
    expect_false(any(grepl(pattern, lines)))
  }
})

test_that("server bootstrap no longer requires legacy global library calls", {
  lines <- readLines(file.path(repo_root, "server.R"), warn = FALSE)

  expect_false(any(grepl('library\\(pROC\\)', lines)))
  expect_false(any(grepl('library\\(plyr\\)', lines)))
  expect_false(any(grepl('library\\(OptimalCutpoints\\)', lines)))
})
