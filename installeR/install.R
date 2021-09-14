## Add required packages for the shiny app here.
pkgs <- c("dplyr", "OptimalCutpoints", "magrittr", "pROC", "plyr")
installed_pkgs <- as.data.frame(installed.packages())[[1]]

if (any(!(pkgs %in% installed_pkgs))){
  pkgs <- pkgs[!(pkgs %in% installed_pkgs)]
  
  for (pkg in pkgs){
    install.packages(pkg)
  }
}
