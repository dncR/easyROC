test_that("readDelimitedUpload rejects missing, missing-file, and oversized uploads", {
  missing_path <- readDelimitedUpload(filePath = "")
  expect_null(missing_path$data)
  expect_match(missing_path$error, "No file path", fixed = TRUE)

  missing_file <- readDelimitedUpload(filePath = tempfile("does-not-exist-"))
  expect_null(missing_file$data)
  expect_match(missing_file$error, "can not be found", fixed = TRUE)

  tmp <- tempfile(fileext = ".tsv")
  writeLines("status\tmarker\n0\t0.1\n1\t0.9", tmp)

  oversized <- readDelimitedUpload(
    filePath = tmp,
    fileSize = 100,
    maxBytes = 10
  )
  expect_null(oversized$data)
  expect_match(oversized$error, "bigger than 30MB", fixed = TRUE)
})

test_that("readDelimitedUpload catches empty file and delimiter mismatch", {
  empty_file <- tempfile(fileext = ".txt")
  file.create(empty_file)

  out_empty <- readDelimitedUpload(filePath = empty_file, fileSize = 0)
  expect_null(out_empty$data)
  expect_match(out_empty$error, "empty or inaccessible", fixed = TRUE)

  csv_file <- tempfile(fileext = ".csv")
  writeLines("status,marker\n0,0.1\n1,0.9", csv_file)

  out_sep <- readDelimitedUpload(filePath = csv_file, sep = "\t")
  expect_null(out_sep$data)
  expect_match(out_sep$error, "Only one column was detected", fixed = TRUE)
})

test_that("readDelimitedUpload detects likely missing header row", {
  no_header <- tempfile(fileext = ".txt")
  writeLines("1\t2\n3\t4\n5\t6", no_header)

  out <- readDelimitedUpload(filePath = no_header, sep = "\t")
  expect_null(out$data)
  expect_match(out$error, "Header row could not be detected", fixed = TRUE)
})

test_that("readDelimitedUpload returns parsed data for valid files", {
  valid_file <- tempfile(fileext = ".tsv")
  writeLines("status\tmarker\n0\t0.1\n1\t0.9", valid_file)

  out <- readDelimitedUpload(filePath = valid_file, sep = "\t")
  expect_null(out$error)
  expect_s3_class(out$data, "data.frame")
  expect_equal(colnames(out$data), c("status", "marker"))
  expect_equal(nrow(out$data), 2)
})
