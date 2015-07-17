roc.utils.min.partial.auc <- function(partial.auc, percent) {
  if (!identical(partial.auc, FALSE)) {
    min <- sum(ifelse(percent, 100, 1)-partial.auc)*abs(diff(partial.auc))/2/ifelse(percent, 100, 1)
  }
  else {
    min <- 0.5 * ifelse(percent, 100, 1)
  }
  return(min)
}
