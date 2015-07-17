SampleSizeTwoTests = function(alpha, power, auc01, auc02, auc11, auc12, ratio) {
  
  if (alpha <= 0 || alpha >= 1) {stop("Type I error must be between 0 and 1.")}
  if (power <= 0 || power >= 1) {stop("Power must be between 0 and 1.")}
  if (auc01 < 0.5 || auc01 >= 1) {stop("AUC must be between 0.5 and 1.")}
  if (auc02 < 0.5 || auc02 >= 1) {stop("AUC must be between 0.5 and 1.")}
  if (auc11 < 0.5 || auc11 >= 1) {stop("AUC must be between 0.5 and 1.")}
  if (auc12 < 0.5 || auc12 >= 1) {stop("AUC must be between 0.5 and 1.")}
  if (ratio <= 0) {stop("Allocation ratio can not be zero negative.")}
  
  
  Za = qnorm(alpha/2, lower.tail = F)
  
  Zb = qnorm(power, lower.tail = T)
  
  A01 = qnorm(auc01, lower.tail = T) * 1.414
  A02 = qnorm(auc02, lower.tail = T) * 1.414
  A11 = qnorm(auc11, lower.tail = T) * 1.414
  A12 = qnorm(auc12, lower.tail = T) * 1.414
  
  V01 = (0.0099*exp((-(A01^2))/2))*(((5*(A01^2)+8)+((A01^2)+8)/ratio))
  V02 = (0.0099*exp((-(A02^2))/2))*(((5*(A02^2)+8)+((A02^2)+8)/ratio))
  V11 = (0.0099*exp((-(A11^2))/2))*(((5*(A11^2)+8)+((A11^2)+8)/ratio))
  V12 = (0.0099*exp((-(A12^2))/2))*(((5*(A12^2)+8)+((A12^2)+8)/ratio))
  
  V0 = V01+V02
  V1 = V11+V12
  
  
  
  
  case = round(((Za*sqrt(V0)+Zb*sqrt(V1))^2)/((auc11-auc12)^2),0)
  
  control = case*ratio
  
  total = case + control
  
  cat("Sample size calculation for comparison of two diagnostic tests", "\n", sep = " ")  
  cat("--------------------------------------------------------------", "\n", sep = " ")
  cat("1) Input:", "\n", sep = " ")  
  cat("--------------------------------------------------------------", "\n", sep = " ")
  cat("Type I error                                     :",  alpha, "\n", sep = " ")
  cat("Power                                            :",  power, "\n", sep = " ")
  cat("AUC for first test under null hypothesis         :",  auc01, "\n", sep = " ")
  cat("AUC for second test under null hypothesis        :",  auc02, "\n", sep = " ")
  cat("AUC for first test under alternative hypothesis  :",  auc11, "\n", sep = " ")
  cat("AUC for second test under alternative hypothesis :",  auc12, "\n", sep = " ")
  cat("Allocation ratio                                 :",  ratio, "\n", sep = " ")
  cat("--------------------------------------------------------------", "\n", sep = " ")
  cat("2) Output:", "\n", sep = " ")  
  cat("--------------------------------------------------------------", "\n", sep = " ")
  cat("Case    :",  case, "\n", sep = " ")
  cat("Control :",  control, "\n", sep = " ")
  cat("Total   :",  total, "\n", sep = " ")
  cat("--------------------------------------------------------------", "\n", sep = " ")
  
}