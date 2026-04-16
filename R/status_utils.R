resolveTagHealthy <- function(statusValues, eventValue){
  if (is.null(statusValues) || length(statusValues) == 0 || is.null(eventValue) || length(eventValue) == 0){
    return(statusValues[0])
  }

  uniqueValues <- unique(statusValues)
  keepIdx <- as.character(uniqueValues) != as.character(eventValue)
  uniqueValues[keepIdx]
}
