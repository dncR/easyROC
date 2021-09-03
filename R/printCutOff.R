printCutOff <- function(res){
    Se = res[[1]][[1]][[2]]$Se
    Sp = res[[1]][[1]][[2]]$Sp
    PPV = res[[1]][[1]][[2]]$PPV
    NPV = res[[1]][[1]][[2]]$NPV
    PLR = res[[1]][[1]][[2]]$DLR.Positive
    NLR = res[[1]][[1]][[2]]$DLR.Negative
    
    cat(" Table 1. Cut-off Results", "\n")
    cat("------------------------------------------------------------", "\n")
    cat("  Optimal cut-off method :", res$methods, "\n", sep = " " )
    cat("  Optimal cut-off point  :", res[[1]][[1]]$optimal.cutoff$cutoff, "\n", sep = " " )
    cat("  Optimal criterion      :", res[[1]][[1]]$optimal.criterion, "\n", sep = " " )
    cat("------------------------------------------------------------", "\n\n")
    
    
    cat(" Table 2. Performance Measures", "\n")
    cat("---------------------------------------------------------------------------------", "\n")
    cat("                                       Value        Lower Limit     Upper Limit",   "\n", sep = "      ")
    cat("---------------------------------------------------------------------------------", "\n")
    cat(" Sensitivity               :", formatC(Se[1],  digits=3, format="f"),  formatC(Se[2],  digits=3, format="f"),  formatC(Se[3],  digits=3, format="f"), "\n",  sep = "           " )
    cat(" Specificity               :", formatC(Sp[1],  digits=3, format="f"),  formatC(Sp[2],  digits=3, format="f"),  formatC(Sp[3],  digits=3, format="f"), "\n",  sep = "           " )
    cat(" Positive Predictive Value :", formatC(PPV[1], digits=3, format="f"),  formatC(PPV[2], digits=3, format="f"),  formatC(PPV[3], digits=3, format="f"), "\n",  sep = "           " )
    cat(" Negative Predictive Value :", formatC(NPV[1], digits=3, format="f"),  formatC(NPV[2], digits=3, format="f"),  formatC(NPV[3], digits=3, format="f"), "\n",  sep = "           " )
    cat(" Positive Likelihood Ratio :", formatC(PLR[1], digits=3, format="f"),  formatC(PLR[2], digits=3, format="f"),  formatC(PLR[3], digits=3, format="f"), "\n",  sep = "           " )
    cat(" Negative Likelihood Ratio :", formatC(NLR[1], digits=3, format="f"),  formatC(NLR[2], digits=3, format="f"),  formatC(NLR[3], digits=3, format="f"), "\n",  sep = "           " )
    cat("---------------------------------------------------------------------------------", "\n")   
}


printCutOff2 <- function(res){
    Se = res[[1]][[1]][[2]]$Se
    Sp = res[[1]][[1]][[2]]$Sp
    PPV = res[[1]][[1]][[2]]$PPV
    NPV = res[[1]][[1]][[2]]$NPV
    PLR = res[[1]][[1]][[2]]$DLR.Positive
    NLR = res[[1]][[1]][[2]]$DLR.Negative
    
    
    value = c(formatC(Se[1],  digits=3, format="f"), formatC(Sp[1],  digits=3, format="f"),
              formatC(PPV[1], digits=3, format="f"), formatC(NPV[1], digits=3, format="f"),
              formatC(PLR[1], digits=3, format="f"), formatC(NLR[1], digits=3, format="f"))
    
    lower = c(formatC(Se[2],  digits=3, format="f"), formatC(Sp[2],  digits=3, format="f"),
              formatC(PPV[2], digits=3, format="f"), formatC(NPV[2], digits=3, format="f"),
              formatC(PLR[2], digits=3, format="f"), formatC(NLR[2], digits=3, format="f"))
    
    upper = c(formatC(Se[3],  digits=3, format="f"), formatC(Sp[3],  digits=3, format="f"),
              formatC(PPV[3], digits=3, format="f"), formatC(NPV[3], digits=3, format="f"),
              formatC(PLR[3], digits=3, format="f"), formatC(NLR[3], digits=3, format="f"))
    
    nms = c("Sensitivity","Specificity","Positive Predictive Value","Negative Predictive Value",
            "Positive Likelihood Ratio", "Negative Likelihood Ratio")

    out = data.frame(nms, value, lower, upper)
    colnames(out) = c("","Value","Lower_Limit","Upper_Limit")
    return(out)
}