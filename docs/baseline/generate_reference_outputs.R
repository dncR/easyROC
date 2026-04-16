.libPaths(c(".Rlib", .libPaths()))

suppressPackageStartupMessages({
  library(plyr)
  library(dplyr)
  library(pROC)
  library(OptimalCutpoints)
})

source("R/mROC.R")
source("R/rocdata.R")
source("R/pAUC.R")
source("R/printCutOff.R")
source("R/SampleSizeSingleTest.R")
source("R/SampleSizeTwoTests.R")
source("R/SampleSizeStandardvsNew.R")
source("R/parametricROC.R")

dir.create("docs/baseline/reference_outputs", recursive = TRUE, showWarnings = FALSE)

mayo <- read.table("data/mayo.txt", header = TRUE)
pbc <- read.table("data/pbc.txt", header = TRUE)

roc_mayo <- mROC(
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
write.table(roc_mayo$stats, "docs/baseline/reference_outputs/mayo_roc_stats.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(roc_mayo$plotdata, "docs/baseline/reference_outputs/mayo_roc_coordinates.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

roc_pbc <- mROC(
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
write.table(roc_pbc$stats, "docs/baseline/reference_outputs/pbc_roc_stats.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

pauc_mayo <- pAUC(
  data = mayo,
  markers = c("mayoscore4", "mayoscore5"),
  status = "censor",
  range = c(0.5, 1),
  criteria = "Sensitivity",
  correct = TRUE,
  direction = "<"
)
write.table(pauc_mayo, "docs/baseline/reference_outputs/mayo_pauc.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

cut_mayo <- optimal.cutpoints(
  X = "mayoscore4",
  status = "censor",
  tag.healthy = 0,
  methods = "Youden",
  data = mayo,
  direction = "<",
  control = control.cutpoints(),
  ci.fit = TRUE,
  conf.level = 0.95,
  trace = FALSE
)
write.table(printCutOff2(cut_mayo), "docs/baseline/reference_outputs/mayo_cutoff_youden.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

param_mayo <- parametricROC(
  data = mayo,
  marker = "mayoscore4",
  status = "censor",
  event = 1,
  returnROCdata = TRUE,
  higherValuesPositives = TRUE,
  confidence.level = 0.95,
  plot = FALSE,
  exact = FALSE
)
write.table(param_mayo$stats, "docs/baseline/reference_outputs/mayo_parametric_roc_stats.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

ss1 <- capture.output(SampleSizeSingleTest(alpha = 0.05, power = 0.80, auc = 0.60, ratio = 1))
ss2 <- capture.output(SampleSizeTwoTests(alpha = 0.05, power = 0.80, auc01 = 0.80, auc02 = 0.80, auc11 = 0.90, auc12 = 0.70, ratio = 1))
ss3 <- capture.output(SampleSizeStandardvsNew(alpha = 0.05, power = 0.80, aucs = 0.80, aucn = 0.80, sd = 0.1, ratio = 1))
writeLines(ss1, "docs/baseline/reference_outputs/sample_size_single_test.txt")
writeLines(ss2, "docs/baseline/reference_outputs/sample_size_two_tests.txt")
writeLines(ss3, "docs/baseline/reference_outputs/sample_size_noninferiority.txt")

bench_iter <- function(fun, n) {
  elapsed <- system.time(for (i in seq_len(n)) fun())[["elapsed"]]
  data.frame(total_sec = elapsed, iter = n, per_iter_ms = (elapsed * 1000) / n)
}

perf <- list(
  mayo_nonparam_roc_stats = bench_iter(function() {
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
    )$stats
  }, n = 100),
  mayo_pauc = bench_iter(function() {
    pAUC(
      data = mayo,
      markers = c("mayoscore4", "mayoscore5"),
      status = "censor",
      range = c(0.5, 1),
      criteria = "Sensitivity",
      correct = TRUE,
      direction = "<"
    )
  }, n = 500),
  mayo_cutoff_youden = bench_iter(function() {
    optimal.cutpoints(
      X = "mayoscore4",
      status = "censor",
      tag.healthy = 0,
      methods = "Youden",
      data = mayo,
      direction = "<",
      control = control.cutpoints(),
      ci.fit = TRUE,
      conf.level = 0.95,
      trace = FALSE
    )
  }, n = 100),
  mayo_parametric_roc = bench_iter(function() {
    parametricROC(
      data = mayo,
      marker = "mayoscore4",
      status = "censor",
      event = 1,
      returnROCdata = TRUE,
      higherValuesPositives = TRUE,
      confidence.level = 0.95,
      plot = FALSE,
      exact = FALSE
    )$stats
  }, n = 500)
)

perf_tbl <- do.call(rbind, lapply(names(perf), function(nm) cbind(task = nm, perf[[nm]])))
write.table(perf_tbl, "docs/baseline/reference_outputs/performance_baseline.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

sink("docs/baseline/reference_outputs/session_info.txt")
cat("Generated at:", format(Sys.time(), tz = "UTC", usetz = TRUE), "\n\n")
print(sessionInfo())
sink()
