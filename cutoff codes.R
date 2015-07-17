opar = par()

source("mROC.R")
source("rocdata.R")
source("ROCplot.R")
library(plyr)
library(OptimalCutpoints)


x = rnorm(100)
y = 1:10
z = rnorm(10)

par(font.lab = 2)
hist(x, xlab="", ylab="", axes = FALSE, main = "")
plot(y,z)


axis(1, cex.axis=1, col="blue", col.ticks = "red", col.axis="green", family = "serif")

title(main = "Deneme", font.main = 1, col.main="red", cex.main=2, family = "sans")
title(xlab = "mayoscore4", font.lab = 1, col.lab="blue", cex.lab=1, family = "mono")

data = read.table("mayo.txt", header=TRUE)
results <- mROC(data=data, statusName="censor", markerName="mayoscore4", 
                event="1", diseaseHigher=TRUE)$plotdata

optimal.cutpoint <- optimal.cutpoints(X = "mayoscore4", status = "censor", tag.healthy = 0, methods = "Youden", 
                                               data = data, direction = "<", pop.prev = NULL, categorical.cov = NULL, 
                                               control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE)

cut.results <- optimal.cutpoint

coord = results[results[,"Marker"] == "mayoscore4",]
cutvals = coord[,"CutOff"]
TPRs = coord[, "TPR"]
FPRs = coord[, "FPR"]
diseased = data[data[,"censor"] == "1", "mayoscore4"]
healthy = data[data[,"censor"] != "1", "mayoscore4"]
dens.diseased = density(diseased)
dens.healthy = density(healthy)
xmin = min(healthy, diseased)
xmax = max(healthy, diseased)
ymax = max(dens.diseased$y, dens.healthy$y)
range = xmax - xmin
expand = 0.05*range

## ROC Curve (Figure 11)
plot(FPRs, TPRs, xlab = "", ylab = "", axes = FALSE, main = "", type = "l", lty = opts$ROClty11, col = opts$ROCcol11)
box()
axis(1, col.axis = opts$xcol.axis11, cex.axis = opts$xcex.axis11)
axis(2, col.axis = opts$ycol.axis11, cex.axis = opts$ycex.axis11)
abline(coef = c(0, 1), lty = 2)

title(main = opts$main11, font.main = opts$font.main11, cex.main = opts$cex.main11, col.main = opts$col.main11,
      xlab = opts$xlab11, font.lab = opts$xfont.lab11, col.lab = opts$xcol.lab11, cex.lab = opts$xcex.lab11)

title(ylab = opts$ylab11, font.lab = opts$yfont.lab11, col.lab = opts$ycol.lab11, cex.lab = opts$ycex.lab11)

arrows(-0.2, cut.results[[1]][[1]][[2]]$Se[1], 1 - cut.results[[1]][[1]][[2]]$Sp[1], cut.results[[1]][[1]][[2]]$Se[1], length=0, lty=2, col="gray70")
arrows(1 - cut.results[[1]][[1]][[2]]$Sp[1], -0.2, 1 - cut.results[[1]][[1]][[2]]$Sp[1], cut.results[[1]][[1]][[2]]$Se[1], length=0, lty=2, col="gray70")
points(1 - cut.results[[1]][[1]][[2]]$Sp[1], cut.results[[1]][[1]][[2]]$Se[1], pch=16)

## Sens. & Spec. Curve (Figure 12)
plot(cutvals, TPRs, type="n", main="", xlab = "", ylab="", axes=FALSE)
lines(cutvals, (1-FPRs), col=opts$specCol, lty=opts$specType)
lines(cutvals, TPRs, col=opts$sensCol, lty=opts$sensType)
box()

axis(1, col.axis = opts$xcol.axis12, cex.axis = opts$xcex.axis12)
axis(2, col.axis = opts$ycol.axis12, cex.axis = opts$ycex.axis12)

title(main = opts$main12, font.main = opts$font.main12, cex.main = opts$cex.main12, col.main = opts$col.main12,
      xlab = opts$xlab12, font.lab = opts$xfont.lab12, col.lab = opts$xcol.lab12, cex.lab = opts$xcex.lab12)

title(ylab = opts$ylab12, font.lab = opts$yfont.lab12, col.lab = opts$ycol.lab12, cex.lab = opts$ycex.lab12)

abline(v=cut.results[[1]][[1]]$optimal.cutoff$cutoff, lty=2, col="gray70")

leg.labels12 = strsplit(opts$legendNames12, ",")[[1]]
leg.position12 = {if (input$legendPos12 == "xy") c(opts$legendXpos12, opts$legendYpos12)
                  else opts$legendPos12}

if(length(leg.labels12) == 2){
  if (input$legendPos12 == "xy"){
    legend(leg.position12[1], leg.position12[2], legend = leg.labels12, lty = c(opts$sensType, opts$specType), 
           col = c(opts$sensCol, opts$specCol), title = opts$legendTitle12, title.col = opts$col.legendTitle12,
           cex = opts$cex.legend12)
  }
  
  else if (input$legendPos21 != "xy"){
    legend(opts$legendPos12, legend = leg.labels12, lty = c(opts$sensType, opts$specType), 
           col = c(opts$sensCol, opts$specCol), title = expression(bold("deneme")), title.col = opts$col.legendTitle12,
           cex = opts$cex.legend12)
  }
}

## Density graph (Figure 21)
hist(data[,input$cutoffMarker], xlab="", xlim=c(xmin - expand, xmax + expand),
     ylim = c(0, ymax + 0.05*ymax), freq=FALSE, border="white", main="", ylab="", axes = FALSE)
box()

lines(dens.diseased, col = opts$lineColD, lty = opts$lineTypeD)
lines(dens.healthy, col = opts$lineColH, lty = opts$lineTypeH)

axis(1, col.axis = opts$xcol.axis21, cex.axis = opts$xcex.axis21)
axis(2, col.axis = opts$ycol.axis21, cex.axis = opts$ycex.axis21)

title(main = opts$main21, font.main = opts$font.main21, cex.main = opts$cex.main21, col.main = opts$col.main21,
      xlab = opts$xlab21, font.lab = opts$xfont.lab21, col.lab = opts$xcol.lab21, cex.lab = opts$xcex.lab21)

title(ylab = opts$ylab21, font.lab = opts$yfont.lab21, col.lab = opts$ycol.lab21, cex.lab = opts$ycex.lab21)

abline(v=cut.results[[1]][[1]]$optimal.cutoff$cutoff, lty=2, col="gray70")

leg.labels21 = strsplit(opts$legendNames21, ",")[[1]]
leg.position21 = ifelse(input$legendPos21 == "xy", c(opts$legendXpos21, opts$legendYpos21), opts$legendPos21)

if(length(leg.labels21) == 2){
  if (input$legendPos21 == "xy"){
    legend(leg.position21[1], leg.position21[2], legend = leg.labels21, lty = c(opts$sensType, opts$specType), 
           col = c(opts$sensCol, opts$specCol), title = opts$legendTitle21, title.col = opts$col.legendTitle21,
           cex = opts$cex.legend21)
  }
  
  else if (input$legendPos21 != "xy"){
    legend(opts$legendPos21, legend = leg.labels21, lty = c(opts$sensType, opts$specType), 
           col = c(opts$sensCol, opts$specCol), title = opts$legendTitle21, title.col = opts$col.legendTitle21,
           cex = opts$cex.legend21)
  }
}

## Figure 22
bgD = strsplit(opts$pchFill, ",")[[1]][2]
bgH = strsplit(opts$pchFill, ",")[[1]][1]

pchColD = strsplit(opts$colPoints, ",")[[1]][2]
pchColH = strsplit(opts$colPoints, ",")[[1]][1]

plot(jitter(rep(1, length(healthy)), amount=opts$jitterAmount), healthy, xlim=c(0.5,2.5), 
     ylim=c(xmin - expand, xmax + expand), axes = FALSE, xlab="", ylab="", main="",
     pch = opts$pchPoints, col = pchColH, bg = bgH, cex = opts$pchSize)
points(jitter(rep(2, length(diseased)), amount=opts$jitterAmount), diseased,
       pch = opts$pchPoints, col = pchColD, bg = bgD, cex = opts$pchSize)

xlabels = strsplit(opts$xlabels22, ",")[[1]]

axis(1, at = c(1,2), col.axis = opts$xcol.axis22, cex.axis = opts$xcex.axis22, labels = xlabels)
axis(2, col.axis = opts$ycol.axis22, cex.axis = opts$ycex.axis22)

title(main = opts$main22, font.main = opts$font.main22, cex.main = opts$cex.main22, col.main = opts$col.main22,
      xlab = opts$xlab22, font.lab = opts$xfont.lab22, col.lab = opts$xcol.lab22, cex.lab = opts$xcex.lab22)

title(ylab = opts$ylab22, font.lab = opts$yfont.lab22, col.lab = opts$ycol.lab22, cex.lab = opts$ycex.lab22)

abline(h=cut.results[[1]][[1]]$optimal.cutoff$cutoff, lty=2, col="gray70")
box()

