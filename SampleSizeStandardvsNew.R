SampleSizeStandardvsNew <- function(alpha, power, aucs, aucn, sd, ratio) {
  
  if (alpha <= 0 || alpha >= 1) {stop("Type I error must be between 0 and 1.")}
  if (power <= 0 || power >= 1) {stop("Power must be between 0 and 1.")}
  if (aucs < 0.5 || aucs >= 1) {stop("AUC must be between 0.5 and 1.")}
  if (aucn < 0.5 || aucn >= 1) {stop("AUC must be between 0.5 and 1.")}
  if (ratio <= 0) {stop("Allocation ratio can not be zero negative.")}

  Za = qnorm(alpha, lower.tail = F)
  Zb = qnorm(power, lower.tail = T)
  
  As = qnorm(aucs, lower.tail = T) * 1.414
  An = qnorm(aucn, lower.tail = T) * 1.414

  Vs = (0.0099*exp((-(As^2))/2))*(((5*(As^2) + 8) + ((As^2) + 8)/ratio))
  Vn = (0.0099*exp((-(An^2))/2))*(((5*(An^2) + 8) + ((An^2) + 8)/ratio))

  V = Vs + Vn
  
  ## Varyans kestirimi icin diger secenekler de kullanilabiliecek sekilde duzenleme yapilacak.
  ## Buradaki hesaplamalar sadece discrete case icin verilmis.
  case = round((((Za+Zb)^2)*V)/((aucs - aucn - sd)^2), 0)
  control = case*ratio
  total = case + control
  
  cat("Sample size calculation for testing noninferiority of a new test to a standard test", "\n", sep = " ")  
  cat("-----------------------------------------", "\n", sep = " ")
  cat("Input:", "\n", sep = " ")  
  cat("-----------------------------------------", "\n", sep = " ")
  cat("Type I error          :",  alpha, "\n", sep = " ")
  cat("Power                 :",  power, "\n", sep = " ")
  cat("AUC for standard test :",  aucs, "\n", sep = " ")
  cat("AUC for new test      :",  aucn, "\n", sep = " ")
  cat("Allocation ratio      :",  ratio, "\n", sep = " ")
  cat("-----------------------------------------", "\n", sep = " ")
  cat("Output:", "\n", sep = " ")  
  cat("-----------------------------------------", "\n", sep = " ")
  cat("Case    :",  case, "\n", sep = " ")
  cat("Control :",  control, "\n", sep = " ")
  cat("Total   :",  total, "\n", sep = " ")
  cat("-----------------------------------------", "\n\n\n", sep = " ")
  
}