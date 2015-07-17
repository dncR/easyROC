
roc.utils.max.partial.auc <- function(partial.auc, percent) {
  if (!identical(partial.auc, FALSE)) {
    max <- abs(diff(partial.auc))
  }
  else {
    max <- 1 * ifelse(percent, 100, 1)
  }
  return(max)
}
