pAUC <- function(data, markers = NULL, status = NULL, range = NULL, criteria = NULL, 
                 correct = TRUE, percent = FALSE, direction = "<") {
  
  criteria_tmp <- criteria
  criteria <- tolower(criteria)
  
  rocResults_tmp <- lapply(markers, function(x){
                      pROC:::roc.default(response = data[ ,status], predictor = data[ ,x], 
                                         percent = FALSE, direction = direction)
                    })
  names(rocResults_tmp) <- markers
  
  pAUC_tmp <- lapply(rocResults_tmp, function(x){
    as.numeric(pROC:::auc.roc(roc = x, partial.auc = range, partial.auc.focus = criteria, 
                   partial.auc.correct = correct, percent = FALSE))  
  })
  
  #rownames(ROCstats) = NULL
  #
  res <- data.frame(Marker = markers, Measure = criteria_tmp, 
                    Value1 = range[1], Value2 = range[2], PartialAUC = round(unlist(pAUC_tmp), 3))
  rownames(res) <- NULL
  colnames(res) = c("Marker", "Measure", "Value 1", "Value 2", "Partial AUC")
  return(res)
}

  

