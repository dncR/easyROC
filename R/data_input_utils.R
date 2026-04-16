readDelimitedUpload <- function(filePath, fileSize = NULL, sep = "\t", decimalComma = FALSE,
                                maxBytes = 30 * 1024^2){
  if (is.null(filePath) || length(filePath) == 0 || filePath == ""){
    return(list(data = NULL, error = "No file path was provided."))
  }

  if (!file.exists(filePath)){
    return(list(data = NULL, error = "Uploaded file can not be found on server. Please re-upload the file."))
  }

  if (is.null(fileSize)){
    fileSize <- file.info(filePath)$size
  }

  if (is.na(fileSize) || fileSize <= 0){
    return(list(data = NULL, error = "Uploaded file appears to be empty or inaccessible."))
  }

  if (fileSize > maxBytes){
    return(list(data = NULL, error = "File is bigger than 30MB and will not be uploaded."))
  }

  data <- tryCatch(
    read.table(
      filePath,
      sep = sep,
      header = TRUE,
      fill = TRUE,
      dec = ifelse(decimalComma, ",", "."),
      stringsAsFactors = FALSE,
      check.names = TRUE
    ),
    error = function(e) e
  )

  if (inherits(data, "error")){
    return(list(data = NULL, error = paste("Uploaded file could not be parsed:", data$message)))
  }

  if (!is.data.frame(data) || ncol(data) == 0 || nrow(data) == 0){
    return(list(data = NULL, error = "Uploaded file did not produce a valid data table."))
  }

  if (ncol(data) == 1 && sep != ""){
    return(list(data = NULL, error = "Only one column was detected. Delimiter selection may be incorrect."))
  }

  colNames <- colnames(data)
  if (all(grepl("^(V|X)\\d+$", colNames))){
    return(list(data = NULL, error = "Header row could not be detected. Please include variable names in the first row."))
  }

  list(data = data, error = NULL)
}
