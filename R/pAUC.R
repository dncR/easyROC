pAUC <- function(data, markers = NULL, status = NULL, range = NULL, criteria = NULL, 
                 correct = TRUE, percent = FALSE, direction = "<") {

  if (!is.data.frame(data)){
    stop("Input 'data' must be a data.frame.")
  }

  if (is.null(status) || length(status) != 1 || !(status %in% colnames(data))){
    stop("status must reference an existing column in data.")
  }

  if (is.null(markers) || length(markers) == 0 || !all(markers %in% colnames(data))){
    stop("markers must reference one or more existing columns in data.")
  }

  if (is.null(range) || length(range) != 2 || any(!is.finite(range)) || any(range < 0) || any(range > 1) || range[1] >= range[2]){
    stop("range must be a numeric vector of length 2 with 0 <= range[1] < range[2] <= 1.")
  }

  if (is.null(criteria) || !(tolower(criteria) %in% c("sensitivity", "specificity"))){
    stop("criteria must be either 'Sensitivity' or 'Specificity'.")
  }
  
  criteria_tmp <- criteria
  criteria <- tolower(criteria)
  
  rocResults_tmp <- lapply(markers, function(x){
                      pROC::roc(response = data[ ,status], predictor = data[ ,x], 
                                percent = FALSE, direction = direction, quiet = TRUE)
                    })
  names(rocResults_tmp) <- markers
  
  pAUC_tmp <- lapply(rocResults_tmp, function(x){
    as.numeric(pROC::auc(roc = x, partial.auc = range, partial.auc.focus = criteria, 
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

  
