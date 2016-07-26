ROCplot <- function(x, legend=TRUE, legendNames = NULL, xlab="1 - Specificity", ylab = "Sensitivity", ...){  
	x.split = split(x[,-c(1,2)], x[,1])
	plot(x.split[[1]], type="n", xlab = xlab, ylab = ylab, ...)
	abline(coef = c(0,1), lty = 2)

	for (i in 1:length(x.split)){
		lines(x.split[[i]], col=i)
	}
    
  if (is.null(legendNames)) legendNames = names(x.split)
	if (legend) legend("bottomright", legend = legendNames, col=1:length(x.split), bty="n", lty=1)
}

