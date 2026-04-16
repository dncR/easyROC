.libPaths(c(file.path(getwd(), ".Rlib"), .libPaths()))

library(testthat)

test_dir("tests/testthat", reporter = "summary")
