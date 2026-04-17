normalize_sample_size_lines <- function(lines) {
  if (length(lines) == 0) {
    return(lines)
  }

  out <- sub("[[:space:]]+$", "", lines)

  while (length(out) > 0 && tail(out, 1) == "") {
    out <- out[-length(out)]
  }

  out
}

read_sample_size_baseline <- function(name) {
  lines <- readLines(file.path(repo_root, "docs", "baseline", "reference_outputs", name), warn = FALSE)
  normalize_sample_size_lines(lines)
}

test_that("compute_sample_size_lines matches single-test baseline output", {
  out <- compute_sample_size_lines(list(
    sampleSizeMethod = 1,
    alpha1 = 0.05,
    power1 = 0.80,
    auc = 0.60,
    ratio = 1
  ))

  expect_equal(
    normalize_sample_size_lines(out),
    read_sample_size_baseline("sample_size_single_test.txt")
  )
})

test_that("compute_sample_size_lines matches two-tests baseline output", {
  out <- compute_sample_size_lines(list(
    sampleSizeMethod = 2,
    alpha2 = 0.05,
    power2 = 0.80,
    auc01 = 0.80,
    auc02 = 0.80,
    auc11 = 0.90,
    auc12 = 0.70,
    ratio2 = 1
  ))

  expect_equal(
    normalize_sample_size_lines(out),
    read_sample_size_baseline("sample_size_two_tests.txt")
  )
})

test_that("compute_sample_size_lines matches non-inferiority baseline output", {
  out <- compute_sample_size_lines(list(
    sampleSizeMethod = 3,
    alpha3 = 0.05,
    power3 = 0.80,
    aucs = 0.80,
    aucn = 0.80,
    sd = 0.1,
    ratio3 = 1
  ))

  expect_equal(
    normalize_sample_size_lines(out),
    read_sample_size_baseline("sample_size_noninferiority.txt")
  )
})

test_that("compute_sample_size_result returns NULL for unknown method", {
  expect_null(compute_sample_size_result(list(sampleSizeMethod = 999)))
})
