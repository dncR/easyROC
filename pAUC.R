pAUC <- function(data, roc, partial.auc=NULL, partial.auc.focus = NULL, partial.auc.correct = TRUE, percent = FALSE) {

  
partial.auc <- sort(partial.auc, decreasing = TRUE)
markerName = roc$stats$Marker

#if (!is.null(markerName)){
  mrknames = markerName
  marker = data.frame(data[,markerName])
  colnames(marker) = mrknames
#}

ROCstats = data.frame(matrix(NA, dim(marker)[2], 5))

X <- split(roc$plotdata, roc$plotdata$Marker)

nms = colnames(marker)
for (i in 1:dim(marker)[2]){


se <- rev(as.vector(t(X[[i]][4])))
sp <- rev(as.vector(t(1-X[[i]][3])))

if (partial.auc.focus == "Sensitivity") {
  x <- rev(se)
  y <- rev(sp)
}else{
  x <- sp
  y <- se
}

x.inc <- x[x <= partial.auc[1] & x >= partial.auc[2]]
y.inc <- y[x <= partial.auc[1] & x >= partial.auc[2]]
diffs.x <- abs(x.inc[-1] - x.inc[-length(x.inc)])
means.vert <- (y.inc[-1] + y.inc[-length(y.inc)])/2
auc <- sum(means.vert * diffs.x)

if (length(x.inc) == 0) {
  diff.horiz <- partial.auc[1] - partial.auc[2]
  idx.hi <- match(FALSE, x < partial.auc[1])
  idx.lo <- idx.hi - 1
  proportion.hi <- (x[idx.hi] - partial.auc[1])/(x[idx.hi] - x[idx.lo])
  proportion.lo <- (partial.auc[2] - x[idx.lo])/(x[idx.hi] - x[idx.lo])
  y.hi <- y[idx.hi] + proportion.hi * (y[idx.lo] - y[idx.hi])
  y.lo <- y[idx.lo] - proportion.lo * (y[idx.lo] - y[idx.hi])
  mean.vert <- (y.hi + y.lo)/2
  auc <- mean.vert * diff.horiz
}

  if (!(partial.auc[1] %in% x.inc)) {
    idx.out <- match(FALSE, x < partial.auc[1])
    idx.in <- idx.out - 1
    proportion <- (partial.auc[1] - x[idx.out])/(x[idx.in] - x[idx.out])
    y.interpolated <- y[idx.out] + proportion * (y[idx.in] - y[idx.out])
    auc <- auc + (partial.auc[1] - x[idx.in]) * (y[idx.in] + y.interpolated)/2
  }
  if (!(partial.auc[2] %in% x.inc)) {
    idx.out <- match(TRUE, x > partial.auc[2]) - 1
    idx.in <- idx.out + 1
    proportion <- (x[idx.in] - partial.auc[2])/(x[idx.in] - x[idx.out])
    y.interpolated <- y[idx.in] + proportion * (y[idx.out] - y[idx.in])
    auc <- auc + (x[idx.in] - partial.auc[2]) * (y[idx.in] + y.interpolated)/2
  }


min <- roc.utils.min.partial.auc(partial.auc, percent)
max <- roc.utils.max.partial.auc(partial.auc, percent)
auc <- (1 + ((auc - min)/(max - min)))/2

ROCstats[i,] = c(nms[i], partial.auc.focus, partial.auc[2], partial.auc[1], round(auc,4))

}


#rownames(ROCstats) = NULL
colnames(ROCstats) = c("Marker", "Measure", "Value 1", "Value 2", "Partial AUC")
ROCstats
}

  

