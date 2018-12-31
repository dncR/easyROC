rocdata <- function(status, marker, event, higherValuesDiseased, alpha=0.05, 
                    se.method = c("MW","DeLong","Null","Binomial"), 
                    ci.method = c("MW","DeLong","Null","Exact"), advanced = FALSE){
  
  # Produces x and y co-ordinates for ROC curve plot
  # Arguments: status - labels classifying subject status
  #            marker - values of each observation
  #            event  - disease value
  # Output: List with 2 components:
  #         roc = data.frame with x and y co-ordinates of plot
  #         stats = data.frame containing: area under ROC curve, p value, upper and lower 95% confidence interval
  
  ## Complete cases.
  completeIdx <- complete.cases(status, marker)
  status <- status[completeIdx]
  marker <- marker[completeIdx]
  
  se.method = match.arg(se.method)
  ci.method = match.arg(ci.method)
  
  if (!advanced){
    se.method = ci.method = "DeLong"  
  }
    
  status <- as.factor(status)
  if (length(marker) != length(status)) {
    stop("The number of classifiers must match the number of data points")
  } 
  
  if (length(levels(status)) != 2) {
    stop("There must only be 2 values for the status")
  }
  
  cut <- unique(marker)  # possible cutoff points
  mrk.nd = marker[status != event]  ## markers for non-diseased
  mrk.d = marker[status == event]  ## markers for diseased
  
  if (higherValuesDiseased){
    tp <- sapply(cut, function(x) length(which(marker >= x & status == event)))
    fn <- sapply(cut, function(x) length(which(marker < x & status == event)))
    fp <- sapply(cut, function(x) length(which(marker >= x & status != event)))
    tn <- sapply(cut, function(x) length(which(marker < x & status != event)))
    
    v.T1j = (1/length(mrk.nd))*sapply(mrk.d, function(x){length(which(x > mrk.nd)) + 0.5*length(which(x == mrk.nd))})
    v.T0j = (1/length(mrk.d))*sapply(mrk.nd, function(x){length(which(x < mrk.d)) + 0.5*length(which(x == mrk.d))}) 
  }                                            
  
  if (!higherValuesDiseased){
    tp <- sapply(cut, function(x) length(which(marker <= x & status == event)))
    fn <- sapply(cut, function(x) length(which(marker > x & status == event)))
    fp <- sapply(cut, function(x) length(which(marker <= x & status != event)))
    tn <- sapply(cut, function(x) length(which(marker > x & status != event)))
    
    v.T1j = (1/length(mrk.nd))*sapply(mrk.d, function(x){length(which(x < mrk.nd)) + 0.5*length(which(x == mrk.nd))})
    v.T0j = (1/length(mrk.d))*sapply(mrk.nd, function(x){length(which(x > mrk.d)) + 0.5*length(which(x == mrk.d))})
  }
  
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)

  roc = data.frame(Cutpoint = cut, FPR = fpr, TPR = tpr)
  roc <- roc[order(roc$FPR, roc$TPR),]
  
  if (higherValuesDiseased){
    roc = rbind(data.frame(Cutpoint = Inf, FPR = 0, TPR = 0), roc)
    roc = rbind(roc, data.frame(Cutpoint = -Inf, FPR = 1, TPR = 1))
  } else {
    roc = rbind(data.frame(Cutpoint = -Inf, FPR = 0, TPR = 0), roc)
    roc = rbind(roc, data.frame(Cutpoint = Inf, FPR = 1, TPR = 1))
  }
  
  i <- 2:nrow(roc)
  auc <- ((roc$FPR[i] - roc$FPR[i - 1]) %*% (roc$TPR[i] + roc$TPR[i - 1]))/2
    
  pos <- marker[status == event]
  neg <- marker[status != event]
  
  { ##  SE and CI calculations
    
    SE.all = CI.all = list()  
    ## Standart error --- Zhou XH, Obuchowski NA, McClish DK (2002). Statistical methods in diagnostic medicine.
    ##                    Wiley-Interscience. , page: 128
    ## It is based on Mann-Whitney U statistic
    q1 <- auc/(2-auc)
    q2 <- (2*auc^2)/(1+auc)
    se.auc.MW <- sqrt(((auc * (1 - auc)) + ((length(pos) - 1)*(q1 - auc^2)) + ((length(neg) - 1)*(q2 - auc^2)))/(length(pos)*length(neg)))
    SE.all$MW = se.auc.MW
    
    ci.upper.MW <- auc + (se.auc.MW * qnorm(1-alpha/2))
    ci.lower.MW <- auc - (se.auc.MW * qnorm(1-alpha/2))
    ci.MW = c(ci.lower.MW, ci.upper.MW)
    CI.all$MW = ci.MW
    
    ## Standart error, ---  DeLong ER, DeLong DM, Clarke-Pearson DL (1988) Comparing the areas under two or more correlated 
    ##                      receiver operating characteristic curves: a nonparametric approach. Biometrics 44:837-845.
    Ssq.T1 = (1/(length(mrk.d)-1))*sum((v.T1j - auc)^2)
    Ssq.T0 = (1/(length(mrk.nd)-1))*sum((v.T0j - auc)^2)
    se.auc.DeLong = sqrt((1/(length(mrk.d)))*Ssq.T1 + (1/(length(mrk.nd)))*Ssq.T0)
    SE.all$DeLong = se.auc.DeLong
    
    ci.upper.DeLong <- auc + (se.auc.DeLong * qnorm(1-alpha/2))
    ci.lower.DeLong <- auc - (se.auc.DeLong * qnorm(1-alpha/2))
    ci.DeLong = c(ci.lower.DeLong, ci.upper.DeLong) 
    CI.all$DeLong = ci.DeLong
    
    ## Standart error under null hypothesis
    se.auc.Null <- sqrt((1 + length(pos) + length(neg))/(12*length(pos)*length(neg)))
    SE.all$Null = se.auc.Null
    
    ci.upper.Null <- auc + (se.auc.Null * qnorm(1-alpha/2))
    ci.lower.Null <- auc - (se.auc.Null * qnorm(1-alpha/2))
    ci.Null = c(ci.lower.Null, ci.upper.Null)
    CI.all$Null = ci.Null
    
    ## Binomial Exact (via F-distribution)
    n = length(neg) + length(pos)
    x = n*auc
    F1 = qf(1-alpha/2, 2*(n-x+1), 2*x)
    F2 = qf(1-alpha/2, 2*(x+1), 2*(n-x))
    ci.upper.Exact <- ((x+1)*F2/(n-x)) / (1 + (x+1)*F2/(n-x))
    ci.lower.Exact <- 1 / (1 + F1*(n-x+1)/x)
    ci.Exact = c(ci.lower.Exact, ci.upper.Exact)
    CI.all$Exact = ci.Exact
    
    se.auc.Binomial <- sqrt(auc*(1-auc)/n)
    SE.all$Binomial = se.auc.Binomial
    
    if (ci.method == "MW"){
      ci.upper = CI.all$MW[2]
      ci.lower = CI.all$MW[1]
    }
    
    if (ci.method == "DeLong"){
      ci.upper = CI.all$DeLong[2]
      ci.lower = CI.all$DeLong[1]
    }
    
    if (ci.method == "Null"){
      ci.upper = CI.all$Null[2]
      ci.lower = CI.all$Null[1]
    }
    
    if (ci.method == "Exact"){
      ci.upper = CI.all$Exact[2]
      ci.lower = CI.all$Exact[1]
    }
    
    if (se.method == "MW"){
      se.auc = SE.all$MW
    }
    
    if (se.method == "DeLong"){
      se.auc = SE.all$DeLong
    }
    
    if (se.method == "Null"){
      se.auc = SE.all$Null
    }
    
    if (se.method == "Binomial"){
      se.auc = SE.all$Binomial
    }
    
    # Large Sample Normal Distribution Approximation (Logit)
    # ci.upperLogit <- auc + (se.auc.Logit * qnorm(1-alpha/2))
    # ci.lowerLogit <- auc - (se.auc.Logit * qnorm(1-alpha/2))
    # ci.Logit = c(ci.lowerLogit, ci.upperLogit)
  }

  z <- (auc - 0.5)/se.auc
  p <- 2*pnorm(-abs(z))
  
  stats <- data.frame (auc = auc, se.auc = se.auc,
                       ci.lower = ci.lower, ci.upper = ci.upper,
                       z = z, p.value = p
  )
  
  return(list(roc = roc, stats = stats))
}