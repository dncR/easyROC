#' Fitting the parametric ROC curves
#' 
#' This function is used to fit parametric ROC curve 
#' 
#' @details 
#' 
#' @note 
#' 
#' @param data a data.frame or matrix
#' @param marker a string indicating the name of diagnostic test, marker or biomarker
#' @param status a string indicating the name of status variable.
#' @param event a string or numeric value. This is the value defining the cases or events.
#' @param ns 
#' @param means
#' @param sds
#' @param returnROCdata a logical. If TRUE, ROC data and coordinates (i.e, FPR and TPR values) are returned.
#' @param higherValuesPositives
#' @param plot
#' @param exact
#' @param confidence.level
#' @param ...  
#' 
#' @author Dincer Goksuluk
#' 
#' @examples <examples here>
#' @rdname parametricROC
#' @aliases parametricROC
#' @export
parametricROC <- function(data = NULL, marker = NULL, status = NULL, event = NULL,
                          ns = NULL, means = NULL, sds = NULL, returnROCdata = TRUE,
                          higherValuesPositives = TRUE, plot = FALSE, exact = FALSE,
                          confidence.level = 0.95, ...){
  
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
    
    n1 <- length(data[data[ ,status] == event, marker])
    mu1 <- mean(data[data[ ,status] == event, marker], na.rm = TRUE)
    sd1 <- sd(data[data[ ,status] == event, marker], na.rm = TRUE)
    
    n0 <- length(data[data[ ,status] != event, marker])
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
  
  # Hypothesis tests:
  # Zhou et. al. (2005) page: 140
  f <- exp((-a^2) / (2*(1 + b^2))) / sqrt(2*pi*(1 + b^2))
  g <- -a*b*exp(-a / (2*(1 + b^2))) / (sqrt(2*pi*((1 + b^2)^3)))
  
  var.a <- (n1 * (a^2 + 2) + 2*n0*b^2) / (2*n0*n1)
  var.b <- ((n0 + n1)*b^2) / (2*n0*n1)
  cov.ab <- (a * b) / (2*n0)
  
  se.auc <- sqrt(var.a * f^2 + var.b * g^2 + 2*f*g*cov.ab)
  
  if (!exact){
    lower <- AUC - qnorm((1 - confidence.level)/2, lower.tail = FALSE)*se.auc
    if (lower <= 0){
      lower <- 0
    }
    
    upper <- AUC + qnorm((1 - confidence.level)/2, lower.tail = FALSE)*se.auc
    if (upper >= 1){
      upper <- 1
    }
  } else {
    ## Binomial Exact (via F-distribution)
    n <- n0 + n1
    x <- n*AUC
    alpha <- 1 - confidence.level
    F1 <- qf(1-alpha/2, 2*(n-x+1), 2*x)
    F2 <- qf(1-alpha/2, 2*(x+1), 2*(n-x))
    upper <- ((x+1)*F2/(n-x)) / (1 + (x+1)*F2/(n-x))
    lower <- 1 / (1 + F1*(n-x+1)/x)
  }
  
  statistic <- (AUC - 0.5) / se.auc
  pvalue <- 2*pnorm(abs(statistic), lower.tail = FALSE)
    
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
    result <- list(plotdata = data.frame(Cutpoint = cuts,
                                        FPR = FPR,
                                        TPR = TPR),
                   stats = data.frame(Marker = marker,
                                      Status = status,
                                      Event = event,
                                      n1 = n1,
                                      n0 = n0,
                                      AUC = AUC,
                                      SE = se.auc,
                                      Lower = lower,
                                      Upper = upper,
                                      z = statistic,
                                      p = pvalue))
    
    return(result)
  }
}