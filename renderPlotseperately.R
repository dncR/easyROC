output$plot11 <- renderPlot({
		if (!is.null(input$markerInput) & input$showPlots & input$tabs1 == "Cut points"){
			opts = grphPrmtrs()
			results <- mROC(data=dataM(), statusName=input$statusVar, markerName=input$markerInput, event=input$valueStatus, diseaseHigher=input$lowhigh)$plotdata
			cut.results <- optimal.cutpoint()
			data = dataM()
			
			coord = results[results[,"Marker"] == input$cutoffMarker,]
			cutvals = coord[,"CutOff"]
			TPRs = coord[, "TPR"]
			FPRs = coord[, "FPR"]
			diseased = data[data[,input$statusVar] == input$valueStatus, input$cutoffMarker]
			healthy = data[data[,input$statusVar] != input$valueStatus, input$cutoffMarker]
			dens.diseased = density(diseased)
			dens.healthy = density(healthy)
			
			xmin = min(healthy, diseased)
			xmax = max(healthy, diseased)
			ymax = max(dens.diseased$y, dens.healthy$y)
			
			range = xmax - xmin
			expand = 0.05*range
			
			par(mar=c(5.1, 6.1, 5.1, 6.1), family = opts$fontfamily)
			
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
		
		}
    }, height = heightsizeCutoff, width = widthsizeCutoff)
    
    output$plot12 <- renderPlot({	
		if (!is.null(input$markerInput) & input$showPlots & input$tabs1 == "Cut points"){
			opts = grphPrmtrs()
			results <- mROC(data=dataM(), statusName=input$statusVar, markerName=input$markerInput, event=input$valueStatus, diseaseHigher=input$lowhigh)$plotdata
			cut.results <- optimal.cutpoint()
			data = dataM()
			
			coord = results[results[,"Marker"] == input$cutoffMarker,]
			cutvals = coord[,"CutOff"]
			TPRs = coord[, "TPR"]
			FPRs = coord[, "FPR"]
			diseased = data[data[,input$statusVar] == input$valueStatus, input$cutoffMarker]
			healthy = data[data[,input$statusVar] != input$valueStatus, input$cutoffMarker]
			dens.diseased = density(diseased)
			dens.healthy = density(healthy)
			
			xmin = min(healthy, diseased)
			xmax = max(healthy, diseased)
			ymax = max(dens.diseased$y, dens.healthy$y)
			
			range = xmax - xmin
			expand = 0.05*range
			
			par(mar=c(5.1, 6.1, 5.1, 6.1), family = opts$fontfamily)
			
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
			if(length(leg.labels12) == 2) {legend("topright", legend=leg.labels12, lty=c(opts$sensType, opts$specType), col=c(opts$sensCol, opts$specCol))}
		}
    }, height = heightsizeCutoff, width = widthsizeCutoff)
    
    output$plot21 <- renderPlot({	
		if (!is.null(input$markerInput) & input$showPlots & input$tabs1 == "Cut points"){
			opts = grphPrmtrs()
			results <- mROC(data=dataM(), statusName=input$statusVar, markerName=input$markerInput, event=input$valueStatus, diseaseHigher=input$lowhigh)$plotdata
			cut.results <- optimal.cutpoint()
			data = dataM()
			
			coord = results[results[,"Marker"] == input$cutoffMarker,]
			cutvals = coord[,"CutOff"]
			TPRs = coord[, "TPR"]
			FPRs = coord[, "FPR"]
			diseased = data[data[,input$statusVar] == input$valueStatus, input$cutoffMarker]
			healthy = data[data[,input$statusVar] != input$valueStatus, input$cutoffMarker]
			dens.diseased = density(diseased)
			dens.healthy = density(healthy)
			
			xmin = min(healthy, diseased)
			xmax = max(healthy, diseased)
			ymax = max(dens.diseased$y, dens.healthy$y)
			
			range = xmax - xmin
			expand = 0.05*range
			
			par(mar=c(5.1, 6.1, 5.1, 6.1), family = opts$fontfamily)
			
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
			
			if(length(leg.labels21) == 2){
				legend("topright", legend=leg.labels21, col=c(opts$lineColD, opts$lineColH),
						lty=c(opts$lineTypeD, opts$lineTypeH))
			}
		}
    }, height = heightsizeCutoff, width = widthsizeCutoff)
    
    output$plot22 <- renderPlot({	
		if (!is.null(input$markerInput) & input$showPlots & input$tabs1 == "Cut points"){
			opts = grphPrmtrs()
			results <- mROC(data=dataM(), statusName=input$statusVar, markerName=input$markerInput, event=input$valueStatus, diseaseHigher=input$lowhigh)$plotdata
			cut.results <- optimal.cutpoint()
			data = dataM()
			
			coord = results[results[,"Marker"] == input$cutoffMarker,]
			cutvals = coord[,"CutOff"]
			TPRs = coord[, "TPR"]
			FPRs = coord[, "FPR"]
			diseased = data[data[,input$statusVar] == input$valueStatus, input$cutoffMarker]
			healthy = data[data[,input$statusVar] != input$valueStatus, input$cutoffMarker]
			dens.diseased = density(diseased)
			dens.healthy = density(healthy)
			
			xmin = min(healthy, diseased)
			xmax = max(healthy, diseased)
			ymax = max(dens.diseased$y, dens.healthy$y)
			
			range = xmax - xmin
			expand = 0.05*range
			
			par(mar=c(5.1, 6.1, 5.1, 6.1), family = opts$fontfamily)
			
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
		}
    }, height = heightsizeCutoff, width = widthsizeCutoff)