
## mROC(...) function is used to calculate multiple ROC curve calculations, such as, AUC, Sensitivity, 
## Specificity, ROC Curve Coordinates, etc.

mROC <- function(data, statusName = NULL, markerName = NULL, event = NULL, eventValue = NULL, diseaseHigher=TRUE,
                 alpha = 0.05, ci.method = "DeLong", se.method = "DeLong", advanced = FALSE){

  if (!is.data.frame(data)){
    stop("Input 'data' must be a data.frame.")
  }

  if (is.null(statusName) || length(statusName) != 1 || !(statusName %in% colnames(data))){
    stop("statusName must reference an existing column in data.")
  }

  if (is.null(markerName) || length(markerName) == 0){
    stop("Marker variable is not specified.")
  }

  if (!all(markerName %in% colnames(data))){
    stop("Some marker variables are not in data.")
  }

  if (is.null(event) && !is.null(eventValue)){
    event <- eventValue
  }

  if (is.null(event) || length(event) == 0){
    stop("Event value is not specified.")
  }

  mrknames <- markerName
  marker <- data.frame(data[ ,markerName])
  colnames(marker) <- mrknames
  
  ROCstats <- list()
  ROCplotdata <- list()
  
  #if (is.null(markerName)) marker = data.frame(data[,colnames(data)[!(colnames(data) %in% statusName)]])
  #if (!is.null(markerName)) {marker = data.frame(data[,markerName]); colnames(marker) = mrknames}
  
  status <- data[ ,statusName]
  nms <- colnames(marker)
  
  for (i in 1:dim(marker)[2]){
    roc.results <- rocdata(status = status, marker = marker[ ,i], event = event, higherValuesDiseased = diseaseHigher,
                          ci.method = ci.method, se.method = se.method, advanced = advanced, alpha = alpha)
    ROCstats[[i]] <- roc.results$stats
    ROCplotdata[[i]] <- roc.results$roc
  }
  
  names(ROCstats) <- names(ROCplotdata) <- nms
  ROCstats <- plyr::ldply(ROCstats, rbind)
  ROCplotdataframe <- plyr::ldply(ROCplotdata, rbind)
  
  rownames(ROCstats) <- NULL
  colnames(ROCstats) <- c("Marker", "AUC", "SE.AUC", "LowerLimit", paste("UpperLimit (*)", sep=""), "z", "p-value")
  colnames(ROCplotdataframe)[1] <- "Marker"
  
  ROCplotdataframe[ ,c("FPR","TPR")] <- round(ROCplotdataframe[ ,c("FPR","TPR")], 4)
  
  ROCplotdataframe <- dplyr::arrange(ROCplotdataframe, Marker, Cutpoint)
  results <- list(plotdata = ROCplotdataframe, stats = ROCstats)
  return(results)
}
