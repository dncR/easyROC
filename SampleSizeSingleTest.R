SampleSizeSingleTest = function(alpha, power, auc, ratio) {
  
  if (alpha <= 0 || alpha >= 1) {stop("Type I error must be between 0 and 1.")}
  if (power <= 0 || power >= 1) {stop("Power must be between 0 and 1.")}
  if (auc <= 0.5 || auc >= 1) {stop("AUC must be between 0.5 and 1.")}
  if (ratio <= 0) {stop("Allocation ratio can not be zero or negative.")}
  
  
  Za = qnorm(alpha, lower.tail = F)
  
  Zb = qnorm(power, lower.tail = T)
  
  A = qnorm(auc, lower.tail = T) * 1.414
  
  V = (0.0099*exp((-(A^2))/2))*(((5*(A^2)+8)+((A^2)+8)/ratio))
  
  case = round((((Za*sqrt(0.0792*(1+1/ratio))+Zb*sqrt(V))^2)/(auc-0.5)^2),0)
  
  control = case*ratio
  
  total = case + control
  
  cat("Sample size calculation for a single diagnostic test", "\n", sep = " ")  
  cat("----------------------------------------------------", "\n", sep = " ")
  cat("1) Input", "\n", sep = " ")  
  cat("----------------------------------------------------", "\n", sep = " ")
  cat("Type I error     :",  alpha, "\n", sep = " ")
  cat("Power            :",  power, "\n", sep = " ")
  cat("AUC              :",  auc, "\n", sep = " ")
  cat("Allocation ratio :",  ratio, "\n", sep = " ")
  cat("----------------------------------------------------", "\n", sep = " ")
  cat("2) Output", "\n", sep = " ")  
  cat("----------------------------------------------------", "\n", sep = " ")
  cat("Case    :",  case, "\n", sep = " ")
  cat("Control :",  control, "\n", sep = " ")
  cat("Total   :",  total, "\n", sep = " ")
  cat("----------------------------------------------------", "\n", sep = " ")
  
}