
library(OptimalCutpoints)

dat = read.table("new.txt", header = TRUE, dec = ",")


cuts <- optimal.cutpoints(X = "mayoscore4", status = "censor", tag.healthy = 0, methods = "Youden", 
                                               data = dat, direction = "<", pop.prev = NULL, categorical.cov = NULL, 
                                               control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE)
summary(cuts)

