readDelimitedUpload <- function(filePath, fileSize = NULL, sep = "\t", decimalComma = FALSE,
                                maxBytes = 30 * 1024^2){
  if (is.null(filePath) || length(filePath) == 0 || filePath == ""){
    return(list(data = NULL, error = "No file path was provided. Please choose a file to upload."))
  }

  if (!file.exists(filePath)){
    return(list(data = NULL, error = "Uploaded file cannot be found on server. Please re-upload the file."))
  }

  if (is.null(fileSize)){
    fileSize <- file.info(filePath)$size
  }

  if (is.na(fileSize) || fileSize <= 0){
    return(list(data = NULL, error = "Uploaded file appears empty or inaccessible. Please upload a non-empty file."))
  }

  if (fileSize > maxBytes){
    return(list(data = NULL, error = "File is larger than 30MB and cannot be uploaded. Please upload a smaller file."))
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
    return(list(
      data = NULL,
      error = paste(
        "Uploaded file could not be parsed. Check delimiter and decimal settings.",
        "Details:",
        data$message
      )
    ))
  }

  if (!is.data.frame(data) || ncol(data) == 0 || nrow(data) == 0){
    return(list(data = NULL, error = "Uploaded file did not produce a valid data table. Please verify file format."))
  }

  if (ncol(data) == 1 && sep != ""){
    return(list(data = NULL, error = "Only one column was detected. Delimiter selection may be incorrect. Try a different delimiter."))
  }

  colNames <- colnames(data)
  if (all(grepl("^(V|X)\\d+$", colNames))){
    return(list(data = NULL, error = "Header row could not be detected. Please include variable names in the first row."))
  }

  list(data = data, error = NULL)
}
