
## mROC(...) function is used to calculate multiple ROC curve calculations, such as, AUC, Sensitivity, 
## Specificity, ROC Curve Coordinates, etc.

mROC <- function(data, statusName = NULL, markerName = NULL, eventValue=NULL, diseaseHigher=TRUE,
                 alpha = 0.05, ci.method = "DeLong", se.method = "DeLong", advanced = FALSE){
  
  if (!is.null(markerName)){warnings("Marker variable is not specified.")}
    
  if (!is.null(markerName)){
    mrknames = markerName
    marker = data.frame(data[,markerName])
    colnames(marker) = mrknames
  }
  
  ROCstats = list()
  ROCplotdata = list()
  
  #if (is.null(markerName)) marker = data.frame(data[,colnames(data)[!(colnames(data) %in% statusName)]])
  #if (!is.null(markerName)) {marker = data.frame(data[,markerName]); colnames(marker) = mrknames}
  
  status = data[,statusName]
  
  nms = colnames(marker)
  for (i in 1:dim(marker)[2]){
    roc.results = rocdata(status=status,marker=marker[,i],event=eventValue,higherValuesDiseased=diseaseHigher,
                          ci.method = ci.method, se.method = se.method, advanced = advanced, alpha = alpha)
    ROCstats[[i]] = roc.results$stats
    ROCplotdata[[i]] = roc.results$roc
  }
  
  names(ROCstats) = names(ROCplotdata) = nms
  ROCstats = ldply(ROCstats, rbind)
  ROCplotdataframe = ldply(ROCplotdata, rbind)
  
  rownames(ROCstats) = NULL
  colnames(ROCstats) = c("Marker", "AUC", "SE.AUC", "LowerLimit", paste("UpperLimit (*)", sep=""), "z", "p-value")
  colnames(ROCplotdataframe)[1] = "Marker"
  
  ROCplotdataframe[,c("FPR","TPR")] = round(ROCplotdataframe[,c("FPR","TPR")], 4)
  
  results = list(plotdata = ROCplotdataframe, stats = ROCstats)
  return(results)
}
