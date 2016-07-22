data = iris[51:150, ]
marker = "Sepal.Length"
status = "Species"
event = "versicolor"
means = NULL
sds = NULL
returnROCdata = TRUE
higherValuesPositives = TRUE
plot = FALSE

parametricROC <- function(data = NULL, marker = NULL, status = NULL, event = NULL,
                          means = NULL, sds = NULL, returnROCdata = TRUE,
                          higherValuesPositives = TRUE, plot = FALSE, ...){
  
  if (all(is.null(data), is.null(means), is.null(sds))){
    stop(warning("Either data matrix or distribution parameters should be given."))
  }
  
  if (!is.null(data)){
    if (any(is.data.frame(data) || is.matrix(data))){
      data <- as.data.frame(data)
      data <- data[complete.cases(data[ ,c(status, marker)]), ]
      dims <- dim(data)
      
      if (is.null(dims)){
        stop(warning("Unexpected input for \"data\". \"data.frame\" and \"matrix\" classes are allowed."))
      }
      
      if (any(dims[1] == 1, dims[2] == 1)){
        stop(warning("Incorrect dimensions for \"data\". Row or column vectors are not allowed."))  
      }
    } else {
      stop(warning("Unexpected input for \"data\". \"data.frame\" and \"matrix\" classes are allowed."))
    }
    
    mu1 <- mean(data[data[ ,status] == event, marker], na.rm = TRUE)
    sd1 <- sd(data[data[ ,status] == event, marker], na.rm = TRUE)
    
    mu0 <- mean(data[data[ ,status] != event ,marker], na.rm = TRUE)
    sd0 <- sd(data[data[ ,status] != event ,marker], na.rm = TRUE)
    
  } else {
    
    mu1 <- means[2]
    sd1 <- sds[2]
    
    mu0 <- means[1]
    sd0 <- sds[1]
  }
  
  if (higherValuesPositives){
    a = (mu1 - mu0)/sd1
    cuts <- seq(mu0 - 4*sd0, mu1 + 4*sd1, length=200)
  } else {
    a = (mu0 - mu1)/sd1
    cuts <- seq(mu1 - 4*sd1, mu0 + 4*sd0, length=200)
  }
  
  b = sd0/sd1
  
  z0 = (cuts - mu0)/sd0
  z1 = (cuts - mu1)/sd1
  
  AUC = pnorm(a/sqrt(1+b^2))
  
  FPR <- if (higherValuesPositives){
    pnorm(z0, lower.tail = FALSE)
  } else {
    pnorm(z0, lower.tail = TRUE)
  }
  
  TPR <- if (higherValuesPositives){
    pnorm(z1, lower.tail = FALSE)
  } else {
    pnorm(z1, lower.tail = TRUE)
  }
  
  if (plot){
    plot(FPR, TPR, type="l", xlim=c(0,1), ylim=c(0,1), ...)
    
    eval(substitute(title(bquote(atop(mu[0]~"="~mu0~~sigma[0]~"="~sd0, mu[1]~"="~mu1~~sigma[1]~"="~sd1)), col = "red"),
                    list(mu0 = as.numeric(round(mu0,2)),
                         sd0 = as.numeric(round(sd0,2)),
                         mu1 = as.numeric(round(mu1,2)),
                         sd1 = as.numeric(round(sd1,2)))))
    
    abline(coef=c(0,1), lty=2)
    text(0.6,0.1,paste("AUC = ",round(AUC,2),sep=""), pos = 4)
  }
  
  if (returnROCdata){
    result <- list(rocData = data.frame(cutoff = cuts,
                                        TPR = TPR,
                                        FPR = FPR),
                   AUC = AUC)
    
    return(result)
  }
  
}