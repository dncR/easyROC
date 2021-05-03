shinyServer(function(input, output, session) {
	source("mROC.R")
	source("rocdata.R")
  source("pAUC.R")
  source("SampleSizeSingleTest.R")
  source("SampleSizeStandardvsNew.R")
  source("SampleSizeTwoTests.R")
  source("rocdata.R")
  source("ROCplot.R")
  source("printCutOff.R")
  source("parametricROC.R")
  library(pROC)
	library(plyr)
  library(OptimalCutpoints)

	## Paste Data bölümünü kontrol eden kod.
	#observe({
	#	if (input$clearText_button == 0) return()
	#	isolate({updateTextInput(session, "myData", label = ",", value = "")})
	#})
      
    

### REACTIVE FUNCTIONS  ###
{
	dataM <- reactive({  ## Data input.
		if (input$dataInput==1){  ## Load example data.
      if (input$sampleData==1){
				data <- read.table("mayo.txt", header=TRUE)
      } else if (input$sampleData==2){
        data <- read.table("pbc.txt", header=TRUE)
      }
		} else if (input$dataInput==2){  ## Upload data.
			
      inFile <- input$upload
      mySep <- switch(input$fileSepDF, '1'=",",'2'="\t",'3'=";", '4'="")
            
			if (is.null(input$upload)){
			  return(NULL)
			}
						
      if (file.info(inFile$datapath)$size <= 31457280){
				data <- read.table(inFile$datapath, sep=mySep, header=TRUE, fill=TRUE, dec = ifelse(input$decimal, ",", "."))
			} else print("File is bigger than 30MB and will not be uploaded.") 
		} 
		
		#else {  ## Paste data.
		#	if(is.null(input$myData)) {return(NULL)}
		#	
		#	tmp <- matrix(strsplit(input$myData, "\n")[[1]])
		#	mySep <- switch(input$fileSepP, '1'=",",'2'="\t",'3'=";")
		#	myColnames <- strsplit(tmp[1], mySep)[[1]]
		#	data <- matrix(0, length(tmp), length(myColnames))
		#	colnames(data) <- myColnames
        #   
		#	for(i in 2:length(tmp)){
		#		myRow <- as.numeric(strsplit(paste(tmp[i],mySep,mySep,sep=""), mySep)[[1]])
		#		data[i-1,] <- myRow[-length(myRow)]
		#	}
		#	
        #   data <- data.frame(data)
		#}
		
		return(data)   
	})
    
  heightsize <- reactive(input$myheight)
	widthsize <- reactive(input$mywidth)
	
	heightsizeCutoff <- reactive(input$myheightCutoff)
	widthsizeCutoff <- reactive(input$mywidthCutoff)
	
	#heightsizeCutoff <- reactive(600)
	#widthsizeCutoff <- reactive(800)
	
	direct <- reactive({
    ifelse(input$lowhigh, "<", ">")
  })
    
	nMarkerInput <- reactive({
    length(input$markerInput)
	})
    
  CFP = reactive({input$CFP})
    
    ctrl <- reactive({  ### control options for optimal cutpoints.
        if (input$cutOffMethods == "Youden") return(control.cutpoints(CFP = input$CFP_Youden, CFN = input$CFN_Youden, generalized.Youden = input$generalized_Youden, costs.benefits.Youden = input$costs_benefits_Youden))
        if (input$cutOffMethods == "CB") return(control.cutpoints(costs.ratio = input$costs_ratio))
        if (input$cutOffMethods == "MCT") return(control.cutpoints(CFP = input$CFP_MCT, CFN = input$CFN_MCT))
        if (input$cutOffMethods == "MinValueSp") return(control.cutpoints(valueSp = input$valueSp_MVSp))
        if (input$cutOffMethods == "MinValueSe") return(control.cutpoints(valueSe = input$valueSe_MVSe))
        if (input$cutOffMethods == "ValueSe") return(control.cutpoints(valueSe = input$valueSe_VSe))
        if (input$cutOffMethods == "ValueSp") return(control.cutpoints(valueSp = input$valueSp_VSp))
        if (input$cutOffMethods == "MinValueSpSe") return(control.cutpoints(valueSp = input$valueSp_MVSpSe, valueSe = input$valueSe_MVSpSe, maxSp = input$maxSp_MVSpSe))
        if (input$cutOffMethods == "MaxKappa") return(control.cutpoints(CFP = input$CFP_MK, CFN = input$CFN_MK, weighted.Kappa = input$weighted_Kappa))
        if (input$cutOffMethods == "MaxEfficiency") return(control.cutpoints(costs.benefits.Efficiency = input$costs_benefits_Efficiency, standard.deviation.accuracy = input$standard_deviation_accuracy))
        if (input$cutOffMethods == "MinValueNPV") return(control.cutpoints(valueNPV = input$valueNPV_MVNPV))
        if (input$cutOffMethods == "MinValuePPV") return(control.cutpoints(valuePPV = input$valuePPV_MVPPV))
        if (input$cutOffMethods == "ValueNPV") return(control.cutpoints(valueNPV = input$valueNPV_VNPV))
        if (input$cutOffMethods == "ValuePPV") return(control.cutpoints(valuePPV = input$valuePPV_VPPV))
        if (input$cutOffMethods == "MinValueNPVPPV") return(control.cutpoints(valueNPV = input$valueNPV_MVNPVPPV, valuePPV = input$valuePPV_MVNPVPPV, maxNPV = input$maxNPV_MVNPVPPV))
        if (input$cutOffMethods == "ValueDLR.Negative") return(control.cutpoints(valueDLR.Negative = input$valueDLR_Negative))
        if (input$cutOffMethods == "ValueDLR.Positive") return(control.cutpoints(valueDLR.Positive = input$valueDLR_Positive))
        if (input$cutOffMethods == "MinPvalue") return(control.cutpoints(adjusted.pvalue = input$adjusted_pvalue))
        if (!(input$cutOffMethods %in% c("Youden","CB","MCT","MinValueSp", "MinValueSe", "ValueSe", "ValueSp", "MinValueSpSe",
                                         "MaxKappa", "MaxEfficiency", "MinValueNPV", "MinValuePPV", "ValueNPV", "ValuePPV",
                                         "MinValueNPVPPV", "ValueDLR.Negative", "ValueDLR.Positive", "MinPvalue"))){
            return(control.cutpoints())
        }
    })
    
    # Reactive function for "statusVar" and "eventCategories"
    statusVar <- reactive({return(input$statusVar)})
}

###  END REACTIVE FUNCTIONS ###
  

   
####  OBSERVER FUNCTIONS #### 
{
	## Yüklenen veri setinin değişken isimlerini takip eden kısım.
	## "statusVar" ve "markerInput" için seçenekler veri setinin değişken isimleri olarak güncelleniyor.

  # Selecting the category for cases.
  # observe({
  #   data_tmp <- dataM()
  #   updateSelectInput(session = session, inputId = "statusVar", 
  #                     choices = colnames(data_tmp), selected = colnames(data_tmp)[1])
  # })
  
  # Selecting the category for cases.
  observe({
    data_tmp <- dataM()
    if (!is.null(data_tmp)){
      updateSelectInput(session = session, inputId = "statusVar", 
                        choices = colnames(data_tmp), selected = colnames(data_tmp)[1])
    } else {
      updateSelectInput(session = session, inputId = "statusVar", 
                        choices = "", selected = "")
    }
  })
  
  # Update select input with the categories of status variable.
  observe({
    data_tmp <- dataM()
    if (!is.null(data_tmp)){
      idx <- which(colnames(data_tmp) %in% statusVar())
      categories <- levels(as.factor(as.character(data_tmp[ ,idx])))
      
      updateSelectizeInput(session = session, inputId = "valueStatus", choices = categories, 
                           selected = NULL)
    } else {
      updateSelectizeInput(session = session, inputId = "valueStatus", choices = "", 
                           selected = "")
    }
  })
  
  observe({
    data_tmp <- dataM()
    if (!is.null(data_tmp)){
      updateSelectInput(session, "markerInput", choices = colnames(dataM())[colnames(dataM()) != input$statusVar], selected = NULL)
    } else {
      updateSelectInput(session, "markerInput", choices = "", selected = "")
    }
  })
  
  observe({
		updateSelectInput(session, "cutoffMarker", choices = input$markerInput, selected = input$markerInput[1])
	})
  
	## "Advanced options" bölümünde seçilen değerler, bu bölüm seçili olmadığında "DeLong" olarak güncelleniyor.
	observe({
		if (!input$advanced){
		  updateRadioButtons(session, inputId = "rocEstimationType", selected = "nonParametricROC")
		  
			# Nonparametric ROC Options
		  updateRadioButtons(session, inputId = "StdErr", selected = "DeLong")
			updateRadioButtons(session, inputId = "ConfInt", selected = "DeLong")
			updateNumericInput(session, inputId = "alpha", value = 0.05)
			
			# Parametric ROC Options
			updateRadioButtons(session, inputId = "ConfIntParametric", selected = "asymptotic")
			updateNumericInput(session, inputId = "alphaParametric", value = 0.05)
		}
	})
	
	observe({
		if (!input$showPlots){
			updateCheckboxInput(session, inputId = "cutoffPlotsOpts", value = FALSE)
		}
	})
	
	observe({
		if (input$selectedGraph == 1 | input$selectedGraph == 4){
			updateSelectInput(session, "subGrps",
								choices = c("Edit x-axis" = "xAxis", "Edit y-axis" = "yAxis",
											"Other options" = "others"),
								selected = "xAxis")
		}
	})
	
	####  XY coord sorunu çözüldüğünde observer kaldırılacak.
	observe({
		if (!input$cutoffPlotsOpts){
			updateSelectInput(session, "legendPos21",
								choices = c("Top right" = "topright", "Top left" = "topleft", "Bottom right" = "bottomright", 
											"Bottom left" = "bottomleft", "xy coord." = "xy"),
								selected = "topright")
								
			updateSelectInput(session, "legendPos12",
								choices = c("Top right" = "topright", "Top left" = "topleft", "Bottom right" = "bottomright", 
											"Bottom left" = "bottomleft", "xy coord." = "xy"),
								selected = "topright")
		}
	})
	
	observe({
		if (input$selectedGraph == 2 | input$selectedGraph == 3){
			updateSelectInput(session, inputId = "subGrps",
								choices = c("Edit x-axis" = "xAxis", "Edit y-axis" = "yAxis",
											"Legend options" = "Legend", "Other options" = "others"),
								selected = "xAxis")
		}
	})
    
    observe({
        if (input$ROCplotOpts && input$legend.namesRC == "") updateTextInput(session, inputId = "legend.namesRC", value = paste("Marker", 1:length(input$markerInput), sep = "", collapse = ","))
    })
}

######  END OBSERVER FUNCTIONS	#######

##########   DEBUG CONSOLE

#   output$console <- renderPrint({
#     head(dataM())
#   })

#########  
########################	Data Upload Tab 	  #######################
{
	## display 10 rows of uploaded Raw Data
    output$RawData <- renderDataTable({
		if (input$tabs1 == "Data upload"){ 
			dataM()
		}
	}, options = list(iDisplayLength = 10))
}

########################	End Data Upload Tab 	  ###################


########################  Download Handlers     #########################
{
	# 2. PDF Format
	output$downloadCutOffPlotPDF <- downloadHandler(
		filename <- function(){paste('CutOff_Plots.pdf')},
		content <- function(file){
			pdf(file, height = input$myheightCutoff/96, width = input$mywidthCutoff/96)
			#pdf(file, height = 5.9, width = 5.9)
				if (!is.null(input$markerInput) && input$showPlots && input$tabs1 == "Cut points"){
			
					if (!input$cutoffPlotsOpts) opts = grphPrmtrsDefault()
					if (input$cutoffPlotsOpts) opts = grphPrmtrs()
					
					results <- mROC(data=dataM(), statusName=input$statusVar, markerName=input$markerInput, event=input$valueStatus, diseaseHigher=input$lowhigh)$plotdata
					cut.results <- optimal.cutpoint()
					data = dataM()
					
					coord = results[results[,"Marker"] == input$cutoffMarker,]
					cutvals = coord[ ,"Cutpoint"]
					TPRs = coord[ ,"TPR"]
					FPRs = coord[ ,"FPR"]
					diseased = data[data[,input$statusVar] == input$valueStatus, input$cutoffMarker]
					healthy = data[data[,input$statusVar] != input$valueStatus, input$cutoffMarker]
					dens.diseased = density(diseased)
					dens.healthy = density(healthy)
					
					xmin = min(healthy, diseased)
					xmax = max(healthy, diseased)
					ymax = max(dens.diseased$y, dens.healthy$y)
					
					range = xmax - xmin
					expand = 0.05*range
					
					par(mfrow=c(2,2), mar=c(5.1, 6.1, 5.1, 6.1), family = opts$fontfamily)
					
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
					leg.position12 = {
						if (input$legendPos12 == "xy") c(opts$legendXpos12, opts$legendYpos12)
						else opts$legendPos12
					}
					
					if(length(leg.labels12) == 2){
						if (input$legendPos12 == "xy"){
							legend(leg.position12[1], leg.position12[2], legend = leg.labels12, lty = c(opts$sensType, opts$specType), 
									col = c(opts$sensCol, opts$specCol), title = opts$legendTitle12, title.col = opts$col.legendTitle12,
									cex = opts$cex.legend12, bg="white", bty = ifelse(input$borderless12, "n","o"))
						}
						
						else if (input$legendPos12 != "xy"){
							legend(opts$legendPos12, legend = leg.labels12, lty = c(opts$sensType, opts$specType), 
									col = c(opts$sensCol, opts$specCol), title = opts$legendTitle12, title.col = opts$col.legendTitle12,
									cex = opts$cex.legend12, bg="white", bty = ifelse(input$borderless12, "n","o"))
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
					leg.position21 = {
						if (input$legendPos21 == "xy") c(opts$legendXpos21, opts$legendYpos21)
						else opts$legendPos21
					}
					
					if(length(leg.labels21) == 2){
						if (input$legendPos21 == "xy"){
							legend(leg.position21[1], leg.position21[2], legend = leg.labels21, lty = c(opts$lineTypeD, opts$lineTypeH), 
									col = c(opts$lineColD, opts$lineColH), title = opts$legendTitle21, title.col = opts$col.legendTitle21,
									cex = opts$cex.legend21, bg="white", bty = ifelse(input$borderless21, "n","o"))
						}
						
						else if (input$legendPos21 != "xy"){
							legend(opts$legendPos21, legend = leg.labels21, lty = c(opts$lineTypeD, opts$lineTypeH), 
									col = c(opts$lineColD, opts$lineColH), title = opts$legendTitle21, title.col = opts$col.legendTitle21,
									cex = opts$cex.legend21, bg="white", bty = ifelse(input$borderless21, "n","o"))
						}
					}
					
					## Figure 22
					bgD = trimws(strsplit(opts$pchFill, ",")[[1]][2], "both")
					bgH = trimws(strsplit(opts$pchFill, ",")[[1]][1], "both")

					pchColD = strsplit(opts$colPoints, ",")[[1]][2]
					pchColH = strsplit(opts$colPoints, ",")[[1]][1]
          
					set.seed(3627)
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
			dev.off()
		},
		contentType = 'application/pdf'
	)
}

{ ##  Download ROC Stats.
    output$downloadROCStats <- downloadHandler(
        filename = function() { "ROC_Statistics.txt" },
        content = function(file) {
          if (!is.null(input$markerInput) & input$tabs1 == "ROC curve"){
            out <- if (input$rocEstimationType == "nonParametricROC"){
                      # Nonparametric ROC
                      mROC(data = dataM(), statusName = input$statusVar, markerName = input$markerInput, 
                           event = input$valueStatus, diseaseHigher = input$lowhigh, ci.method = input$ConfInt,
                           se.method = input$StdErr, advanced = input$advanced, alpha = input$alpha)$stats
                    } else {
                      # Parametric ROC
                      tmp <- lapply(input$markerInput, function(x){
                        parametricROC(data = dataM(), marker = x, status = input$statusVar,
                                      event = input$valueStatus, returnROCdata = TRUE,
                                      higherValuesPositives = input$lowhigh, confidence.level = 1 - input$alphaParametric,
                                      plot = FALSE, exact = ifelse(input$ConfIntParametric == "Exact", TRUE, FALSE))$stats
                      })
                      names(tmp) <- input$markerInput
                      
                      tmp <- plyr:::ldply(tmp, rbind)[ ,-1]
                      tmp <- tmp[ ,-c(2:5)]
                      
                      colnames(tmp) <- c("Marker", "AUC", "SE.AUC", "LowerLimit", paste("UpperLimit (*)", sep=""), "z", "p-value")
                      tmp
                    }
          }
          
          colnames(out) <- c("Marker","AUC","SE.AUC","LowerLimit","UpperLimit","z","p-value")
          out[,-1] <- round(out[,-1], 5)
          write.table(out, file, row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t") 
        }
    )   
}

{ ##  Download ROC Coordinates.
  output$downloadROCData <- downloadHandler(
    filename = function() { "ROC_Coordinates.txt" },
    content = function(file) {
      
      if (!is.null(input$markerInput) & input$tabs1 == "ROC curve"){
       out <-  ROCstats()$plotdata
      }
      write.table(out, file, row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t") 
    }
  )   
}

{
    # Download ROCPlot (pdf format)
    output$downloadROCPlot <- downloadHandler(
        filename <- function(){paste('ROCplot.pdf')},
        content <- function(file){
            pdf(file, height = input$myheightCutoff/96, width = input$mywidthCutoff/96)
          
            if(!is.null(input$markerInput) & input$tabs1 == "ROC curve"){
              results <- ROCstats()$plotdata
              if (input$ROCplotOpts){
                opts = grphPrmtrsRC()
              } else if (!input$ROCplotOpts){
                opts = grphPrmtrsDefaultRC()
              }
              
              par(family = opts$fontfamilyRC)
              
              ## ROC Curve
              if (input$ROCplotOpts){
                legNms = opts$legend.namesRC
              } else {
                legNms = NULL
              }
              
              ROCplot(results, xlab = "", ylab = "", axes = FALSE, main = "", legend=TRUE,
                      legendNames = legNms, col = opts$ROCcolRC, lty = as.numeric(opts$ROCltyRC))
              box()
              axis(1, col.axis = opts$xcol.axisRC, cex.axis = opts$xcex.axisRC)
              axis(2, col.axis = opts$ycol.axisRC, cex.axis = opts$ycex.axisRC)
              abline(coef = c(0, 1), lty = 2)
              
              title(main = opts$mainRC, font.main = opts$font.mainRC, cex.main = opts$cex.mainRC, col.main = opts$col.mainRC)
              title(xlab = opts$xlabRC, font.lab = opts$xfont.labRC, col.lab = opts$xcol.labRC, cex.lab = opts$xcex.labRC)
              title(ylab = opts$ylabRC, font.lab = opts$yfont.labRC, col.lab = opts$ycol.labRC, cex.lab = opts$ycex.labRC)
            }
            dev.off()
        },
        contentType = 'application/pdf'
    )
}


{ ##  Download Cut-off results.
    output$downloadCutOffresults <- downloadHandler(
        filename = function() { "CutOff_Results.txt" },
        content = function(file) {
            if (!is.null(input$markerInput) & input$tabs1 == "Cut points"){
                res = optimal.cutpoint()
                out = printCutOff2(res)
            }
            write.table(out, file, row.names=F, col.names=TRUE, quote=F, sep="\t")
        }
    )   
}

########################  End Download Handlers		#####################


########################	 ROC Curve Tab 	  ###########################
{
  output$section1 <- renderText({
		if (!is.null(input$markerInput) & input$tabs1 == "ROC curve"){
			'1. ROC Statistics'
		}
	})
	
  output$ROCstatistics <- renderDataTable(options = list(iDisplayLength = 10),{
		if (!is.null(input$markerInput) & input$tabs1 == "ROC curve"){
		  if (input$rocEstimationType == "nonParametricROC"){
		    # Nonparametric ROC
		    mROC(data = dataM(), statusName = input$statusVar, markerName = input$markerInput, 
		         event = input$valueStatus, diseaseHigher = input$lowhigh, ci.method = input$ConfInt,
		         se.method = input$StdErr, advanced = input$advanced, alpha = input$alpha)$stats
		  } else {
		    # Parametric ROC
		    tmp <- lapply(input$markerInput, function(x){
		      parametricROC(data = dataM(), marker = x, status = input$statusVar,
		                    event = input$valueStatus, returnROCdata = TRUE,
		                    higherValuesPositives = input$lowhigh, confidence.level = 1 - input$alphaParametric,
		                    plot = FALSE, exact = ifelse(input$ConfIntParametric == "Exact", TRUE, FALSE))$stats
		    })
		    names(tmp) <- input$markerInput
		    
		    tmp <- plyr:::ldply(tmp, rbind)[ ,-1]
		    tmp <- tmp[ ,-c(2:5)]
		    
		    colnames(tmp) <- c("Marker", "AUC", "SE.AUC", "LowerLimit", paste("UpperLimit (*)", sep=""), "z", "p-value")
		    tmp
		  }
		} 
			
	})
  
  ROCstats <- reactive ({
    # Nonparametric ROC
    if (input$rocEstimationType == "nonParametricROC"){
      mROC(data=dataM(), statusName=input$statusVar, markerName=input$markerInput,
           event=input$valueStatus, diseaseHigher=input$lowhigh)
    } else {
      # Parametric ROC
      tmp <- lapply(input$markerInput, function(x){
        tmp2 <- parametricROC(data = dataM(), marker = x, status = input$statusVar,
                              event = input$valueStatus, returnROCdata = TRUE,
                              higherValuesPositives = input$lowhigh, confidence.level = 1 - input$alphaParametric,
                              plot = FALSE, exact = FALSE)$plotdata
        
        tmp2 <- round(tmp2, 4)
        tmp2 <- tmp2[order(tmp2$FPR, tmp2$TPR),]
        
        if (input$lowhigh){
          tmp2 = rbind(data.frame(Cutpoint = Inf, FPR = 0, TPR = 0), tmp2)
          tmp2 = rbind(tmp2, data.frame(Cutpoint = -Inf, FPR = 1, TPR = 1))
        } else {
          tmp2 = rbind(data.frame(Cutpoint = -Inf, FPR = 0, TPR = 0), tmp2)
          tmp2 = rbind(tmp2, data.frame(Cutpoint = Inf, FPR = 1, TPR = 1))
        }
        return(tmp2)
      })
      names(tmp) <- input$markerInput
      
      tmp <- plyr:::ldply(tmp, rbind)
      colnames(tmp)[1] <- "Marker"
      
      tmp <- dplyr:::arrange(tmp, Marker, Cutpoint)
      
      tmp <- list(plotdata = as.data.frame(tmp))
      tmp
    }
  })
	
	output$ROCcoordinates <- renderDataTable(options = list(iDisplayLength = 10), {
	  if (!is.null(input$markerInput) & input$tabs1 == "ROC curve"){
      ROCstats()$plotdata
	  }
	})
		
    
    output$ROCcomparisons <- renderDataTable(options = list(iDisplayLength = 10),
		{
        if(length(input$markerInput) >= 2 & input$tabs1 == "ROC curve"){
			
			combs = data.frame(combn(length(input$markerInput), 2))  ## pairwise combinations (index)
            nCol = ifelse(nMarkerInput() > 2, 9, 8)
		
			Comparisons = data.frame(matrix(NA, nrow = ncol(combs), ncol = nCol))
			if (nCol == 9){
                colnames(Comparisons) = c("Marker1 (I)", "Marker2 (J)", "AUC(I)", "AUC(J)", "|I - J|", "SE(|I - J|)", "z", "p-value", "p-value (adj.)")
			}
            if(nCol == 8){
                colnames(Comparisons) = c("Marker1 (I)", "Marker2 (J)", "AUC(I)", "AUC(J)", "|I - J|", "SE(|I - J|)", "z", "p-value")
            }
			
			Comparisons[,1] = input$markerInput[as.numeric(combs[1,])]
			Comparisons[,2] = input$markerInput[as.numeric(combs[2,])]
						
			stats = mROC(data=dataM(), statusName=input$statusVar, markerName=input$markerInput, 
						 event=input$valueStatus, diseaseHigher=input$lowhigh, ci.method=input$ConfInt,
						 se.method=input$StdErr, advanced=input$advanced, alpha=input$alpha)$stats
			
			stats.tmp = stats[,1:3]
			
			Comparisons[,3] = stats.tmp[as.numeric(combs[1,]),2]
			Comparisons[,4] = stats.tmp[as.numeric(combs[2,]),2]

			Comparisons[,5] = abs(Comparisons[,4] - Comparisons[,3])
			Comparisons[,6] = sqrt(stats.tmp[as.numeric(combs[1,]),3]^2 + stats.tmp[as.numeric(combs[2,]),3]^2)
			Comparisons[,7] = Comparisons[,5] / Comparisons[,6]
			Comparisons[,8] = 2*(1 - pnorm(Comparisons[,7]))
			
            if (nCol == 9) {Comparisons[,9] = p.adjust(Comparisons[,8], method = input$MultipleCompMethod)}
			            
			Comparisons[, -c(1,2)] = round(Comparisons[, -c(1,2)], 4)
			Comparisons
        }}
    )
    	
	output$CIreminderLine1 <- renderText({
		if (!is.null(input$markerInput) & input$tabs1 == "ROC curve"){
      if (input$navbarROCcurve == 'Statistics'){
        '______________________________'
      }
      
      else if (input$navbarROCcurve == 'Multiple Comparisons'){
        ifelse(length(input$markerInput) >= 2, '______________________________', "")
      }
      
      else ""
		}
	})
	
	output$CIreminderLine2 <- renderText({
		if (!is.null(input$markerInput) & input$tabs1 == "ROC curve"){
			if (input$navbarROCcurve == 'Statistics'){
				paste("\U2005 * \U2007", 'Upper limit might exceed 1.0 in some cases. See "Manual" for further information. Default estimation method is "DeLong (1988)".', sep="")
			}
			
			else if (input$navbarROCcurve == 'Multiple Comparisons'){
        ifelse(length(input$markerInput) >= 2, '   Compared tests are assumed to be independent, i.e Cov(I,J) = 0.', "")
			}
			
			else ""
		}
	})
	
	output$section2 <- renderText({
		if (!is.null(input$markerInput) & input$tabs1 == "ROC curve"){
			'2. Plot Output'
		}
	})
	
	output$ROCplot <- renderPlot({
		if(!is.null(input$markerInput) & input$tabs1 == "ROC curve"){
		  
# 		  if (input$rocEstimationType == "nonParametricROC"){
# 		    # Nonparametric ROC
#   			results <- mROC(data=dataM(), statusName=input$statusVar, markerName=input$markerInput, 
#   			                event=input$valueStatus, diseaseHigher=input$lowhigh)$plotdata
# 		  } else {
# 		    # Parametric ROC
# 		    
# 		  }
# 		  
      results <- ROCstats()$plotdata
		  
			if (input$ROCplotOpts){
			  opts = grphPrmtrsRC()
			} else if (!input$ROCplotOpts){
			  opts = grphPrmtrsDefaultRC()
			}
            
			par(family = opts$fontfamilyRC)
            
			## ROC Curve
      if (input$ROCplotOpts){
        legNms = opts$legend.namesRC
      } else {
        legNms = NULL
      }
            
			ROCplot(results, xlab = "", ylab = "", axes = FALSE, main = "", legend=TRUE,
			        legendNames = legNms, col = opts$ROCcolRC, lty = as.numeric(opts$ROCltyRC))
			
			box()
			axis(1, col.axis = opts$xcol.axisRC, cex.axis = opts$xcex.axisRC)
			axis(2, col.axis = opts$ycol.axisRC, cex.axis = opts$ycex.axisRC)
			abline(coef = c(0, 1), lty = 2)
			
			title(main = opts$mainRC, font.main = opts$font.mainRC, cex.main = opts$cex.mainRC, col.main = opts$col.mainRC)
			title(xlab = opts$xlabRC, font.lab = opts$xfont.labRC, col.lab = opts$xcol.labRC, cex.lab = opts$xcex.labRC)
			title(ylab = opts$ylabRC, font.lab = opts$yfont.labRC, col.lab = opts$ycol.labRC, cex.lab = opts$ycex.labRC)
		}
	}, height = heightsize, width = widthsize)
    
    
    pAUCresult <- reactive({
        
        pAUC(data = dataM(), range = c(input$pointA, input$pointB), 
             criteria = input$sensSpec, correct = TRUE, percent = FALSE, markers = input$markerInput,
             status = input$statusVar, direction = ifelse(input$lowhigh, "<", ">"))
        
    })
    
    
    output$resultPAuc <- renderDataTable(options = list(iDisplayLength = 10),
    {
        if(!is.null(input$markerInput) & input$tabs1 == "ROC curve")
            
            pAUCresult()
            
        }
    )
	
    #output$console2 <- renderPrint({
    #    if (!input$ROCplotOpts ) grphPrmtrsDefaultRC()
    #    else if (input$ROCplotOpts ) grphPrmtrsRC()
    #})
   
    #output$console3 <- renderPrint({
    #    ctrl()    
    #}
    #)
}	
########################   End ROC Curve Tab 	  #######################	

	
########################	 Cut Off Tab 	  ###########################
{
    
    tagHealthy <- reactive({
        cl = unique(dataM()[,input$statusVar])
        ind = which(cl != as.numeric(input$valueStatus))
        return(cl[ind])
    })
    
    
	optimal.cutpoint <- reactive(optimal.cutpoints(X = input$cutoffMarker, status = input$statusVar, tag.healthy = tagHealthy(), methods = input$cutOffMethods, 
											  data = dataM(), direction = direct(), pop.prev = NULL, categorical.cov = NULL, 
											  control = ctrl(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE))
											   
    output$cutPoints <- renderPrint({
        if (!is.null(input$markerInput) & input$tabs1 == "Cut points"){
			res = optimal.cutpoint()
			printCutOff(res)
        }
    })
    
    grphPrmtrsDefaultRC <- reactive({
        opts = list()
        opts$fontfamilyRC = "sans"
        
        opts$mainRC = ""
        opts$font.mainRC = NULL
        opts$cex.mainRC = NULL
        opts$col.mainRC = NULL
        
        opts$legend.namesRC = strsplit(input$legend.namesRC, split = ",")[[1]]
        
        opts$ROCcolRC = 1:length(input$markerInput)
        opts$ROCltyRC = 1
        opts$xlabRC = "1-Specificity"
        opts$xfont.labRC = 1
        opts$xcol.labRC = "black"
        opts$xcex.labRC = 1
        
        opts$xcol.axisRC = "black"
        opts$xcex.axisRC = 1
        
        opts$ylabRC = "Sensitivity"
        opts$yfont.labRC = 1
        opts$ycol.labRC = "black"
        opts$ycex.labRC = 1
        
        opts$ycol.axisRC = "black"
        opts$ycex.axisRC = 1
        
        return(opts)
    })
    
    grphPrmtrsRC <- reactive({
      opts = list()
      opts$fontfamilyRC = input$fontfamilyRC
      
      opts$mainRC = input$mainRC
      opts$font.mainRC = input$font.mainRC
      opts$cex.mainRC = input$cex.mainRC
      opts$col.mainRC = input$col.mainRC
      
      opts$legend.namesRC = strsplit(input$legend.namesRC, split = ",")[[1]]
      
      opts$ROCcolRC = trimws(strsplit(input$ROCcolRC, ",")[[1]], "both")
      opts$ROCltyRC = input$ROCltyRC
      opts$xlabRC = input$xlabRC
      opts$xfont.labRC = input$xfont.labRC
      opts$xcol.labRC = input$xcol.labRC
      opts$xcex.labRC = input$xcex.labRC
      
      opts$xcol.axisRC = input$xcol.axisRC
      opts$xcex.axisRC = input$xcex.axisRC
      
      opts$ylabRC = input$ylabRC
      opts$yfont.labRC = input$yfont.labRC
      
      opts$ycol.labRC = input$ycol.labRC
      opts$ycex.labRC = input$ycex.labRC
      opts$ycol.axisRC = input$ycol.axisRC
      opts$ycex.axisRC = input$ycex.axisRC
      
      return(opts)
    })
    
    grphPrmtrsDefault <- reactive({
  		opts = list()
  		opts$fontfamily = "sans"
  		
  		opts$main11 = "ROC Curve"
  		opts$main12 = "Sens. & Spec. Curves"
  		opts$main21 = paste("Distribution of ", input$cutoffMarker, sep="")
  		opts$main22 = paste("Distribution of ", input$cutoffMarker, sep="")
  		
  		opts$font.main11 = opts$font.main12 = opts$font.main21 = opts$font.main22 = 2
  		opts$cex.main11 = opts$cex.main12 = opts$cex.main21 = opts$cex.main22 = 1.2
  		opts$col.main11 = opts$col.main12 = opts$col.main21 = opts$col.main22 = "black"
  		
  		opts$ROCcol11 = "black"  	# ROC line color
  		opts$ROClty11 = 1			# ROC line type
  		
  		opts$sensCol = "red" 		# Line color for Sensitivity
  		opts$specCol = "blue"		# Line color for Specificity
  		opts$sensType = opts$specType = 1   # Line type for Sensitivity and Specificity
  		
  		opts$lineColD = "red"
  		opts$lineColH = "blue"
  		opts$lineTypeD = opts$lineTypeH = 1
  		
  		opts$xlab11 = "1-Specificity"
  		opts$xlab12 = opts$xlab21 = input$cutoffMarker
  		opts$xlab22 = "Disease Status"
  		
  		opts$xfont.lab11 = opts$xfont.lab12 = opts$xfont.lab21 = opts$xfont.lab22 = 1
  		opts$xcol.lab11 = opts$xcol.lab12 = opts$xcol.lab21 = opts$xcol.lab22 = "black"
  		
  		opts$xcex.lab11 = opts$xcex.lab12 = opts$xcex.lab21 = opts$xcex.lab22 = 1
  		opts$xcol.axis11 = opts$xcol.axis12 = opts$xcol.axis21 = opts$xcol.axis22 = "black"
  		opts$xcex.axis11 = opts$xcex.axis12 = opts$xcex.axis21 = opts$xcex.axis22 = 1
  		
  		opts$ylab11 = "Sensitivity"
  		opts$ylab12 = ""
  		opts$ylab22 = input$cutoffMarker
  		opts$ylab21 = "Density"
  		
  		opts$yfont.lab11 = opts$yfont.lab12 = opts$yfont.lab21 = opts$yfont.lab22 = 1
  		opts$ycol.lab11 = opts$ycol.lab12 = opts$ycol.lab21 = opts$ycol.lab22 = "black"
  		
  		opts$ycex.lab11 = opts$ycex.lab12 = opts$ycex.lab21 = opts$ycex.lab22 = 1
  		opts$ycol.axis11 = opts$ycol.axis12 = opts$ycol.axis21 = opts$ycol.axis22 = "black"
  		opts$ycex.axis11 = opts$ycex.axis12 = opts$ycex.axis21 = opts$ycex.axis22 = 1
  		
  		opts$legendPos12 = "topright"
  		opts$legendXpos12 = opts$legendYpos12 = 1
  		opts$legendNames12 = "Sens.,Spec."
  		opts$cex.legend12 = 1
  		opts$legendTitle12 = NULL
  		opts$font.legendTitle12 = 2
  		opts$col.legendTitle12 = "black"
      opts$borderless12 = FALSE
          
  		opts$legendPos21 = "topright"
  		opts$legendXpos21 = opts$legendYpos21 = 1
  		opts$legendNames21 = "Diseased,Healthy"
  		opts$cex.legend21 = 1
  		opts$legendTitle21 = NULL
  		opts$font.legendTitle21 = 2
  		opts$col.legendTitle21 = "black"
  		opts$borderless21 = FALSE
          
  		opts$colPoints = "black,black"
  		opts$pchFill = "white,white"
  		opts$xlabels22 = "Healthy,Diseased"
  		opts$pchPoints = 1
  		opts$pchSize = 1
  		opts$jitterAmount = 0.05
  		
  		return(opts)
    })
    
    grphPrmtrs <- reactive({
		
  		opts = list()
  		opts$fontfamily = input$fontfamily
  		opts$main11 = input$main11
  		opts$main12 = input$main12
  		opts$main21 = input$main21
  		opts$main22 = input$main22
  		
  		opts$font.main11 = as.numeric(input$font.main11)
  		opts$font.main12 = as.numeric(input$font.main12)
  		opts$font.main21 = as.numeric(input$font.main21)
  		opts$font.main22 = as.numeric(input$font.main22)
  		
  		opts$cex.main11 = input$cex.main11
  		opts$cex.main12 = input$cex.main12
  		opts$cex.main21 = input$cex.main21
  		opts$cex.main22 = input$cex.main22
  		
  		opts$col.main11 = input$col.main11
  		opts$col.main12 = input$col.main12
  		opts$col.main21 = input$col.main21
  		opts$col.main22 = input$col.main22
  		
  		opts$ROCcol11 = input$ROCcol11
  		opts$ROClty11 = as.numeric(input$ROClty11)
  		
  		opts$sensCol = input$sensCol
  		opts$specCol = input$specCol
  		opts$sensType = as.numeric(input$sensType)
  		opts$specType = as.numeric(input$specType)
  		
  		opts$lineColD = input$lineColD
  		opts$lineColH = input$lineColH
  		opts$lineTypeD = as.numeric(input$lineTypeD)
  		opts$lineTypeH = as.numeric(input$lineTypeH)
  		
  		opts$xlab11 = input$xlab11
  		opts$xlab12 = input$xlab12
  		opts$xlab21 = input$xlab21
  		opts$xlab22 = input$xlab22
  		
  		opts$xfont.lab11 = as.numeric(input$xfont.lab11)
  		opts$xfont.lab12 = as.numeric(input$xfont.lab12)
  		opts$xfont.lab21 = as.numeric(input$xfont.lab21)
  		opts$xfont.lab22 = as.numeric(input$xfont.lab22)
  						
  		opts$xcol.lab11 = input$xcol.lab11
  		opts$xcol.lab12 = input$xcol.lab12
  		opts$xcol.lab21 = input$xcol.lab21
  		opts$xcol.lab22 = input$xcol.lab22
  		
  		opts$xcex.lab11 = input$xcex.lab11
  		opts$xcex.lab12 = input$xcex.lab12
  		opts$xcex.lab21 = input$xcex.lab21
  		opts$xcex.lab22 = input$xcex.lab22
  		
  		opts$xcol.axis11 = input$xcol.axis11
  		opts$xcol.axis12 = input$xcol.axis12
  		opts$xcol.axis21 = input$xcol.axis21
  		opts$xcol.axis22 = input$xcol.axis22
  		
  		opts$xcex.axis11 = input$xcex.axis11
  		opts$xcex.axis12 = input$xcex.axis12
  		opts$xcex.axis21 = input$xcex.axis21
  		opts$xcex.axis22 = input$xcex.axis22
  
  		opts$ylab11 = input$ylab11
  		opts$ylab12 = input$ylab12
  		opts$ylab22 = input$ylab22
  		opts$ylab21 = input$ylab21
  		
  		opts$yfont.lab11 = as.numeric(input$yfont.lab11)
  		opts$yfont.lab12 = as.numeric(input$yfont.lab12)
  		opts$yfont.lab21 = as.numeric(input$yfont.lab21)
  		opts$yfont.lab22 = as.numeric(input$yfont.lab22)
  		
  		opts$ycol.lab11 = input$ycol.lab11
  		opts$ycol.lab12 = input$ycol.lab12
  		opts$ycol.lab21 = input$ycol.lab21
  		opts$ycol.lab22 = input$ycol.lab22
  		
  		opts$ycex.lab11 = input$ycex.lab11
  		opts$ycex.lab12 = input$ycex.lab12
  		opts$ycex.lab21 = input$ycex.lab21
  		opts$ycex.lab22 = input$ycex.lab22
  		
  		opts$ycol.axis11 = input$ycol.axis11
  		opts$ycol.axis12 = input$ycol.axis12
  		opts$ycol.axis21 = input$ycol.axis21
  		opts$ycol.axis22 = input$ycol.axis22
  		
  		opts$ycex.axis11 = input$ycex.axis11
  		opts$ycex.axis12 = input$ycex.axis12
  		opts$ycex.axis21 = input$ycex.axis21
  		opts$ycex.axis22 = input$ycex.axis22
  		
  		opts$legendPos12 = input$legendPos12
  		opts$legendXpos12 = input$legendXpos12
  		opts$legendYpos12 = input$legendYpos12
  		opts$legendNames12 = input$legendNames12
  		opts$cex.legend12 = input$cex.legend12
  		opts$borderless12 = input$borderless12
  		
  		opts$legendTitle12 = { 
  			if (is.null(input$legendTitle12) | input$legendTitle12 == "") NULL
  			else input$legendTitle12
  		}
  		
  		opts$font.legendTitle12 = as.numeric(input$font.legendTitle12)
  		opts$col.legendTitle12 = input$col.legendTitle12
  		
  		opts$legendPos21 = input$legendPos21
  		opts$legendXpos21 = input$legendXpos21
  		opts$legendYpos21 = input$legendYpos21
  		opts$legendNames21 = input$legendNames21
  		opts$cex.legend21 = input$cex.legend21
  		opts$borderless21 = input$borderless21
          
  		opts$legendTitle21 = {
  			if (is.null(input$legendTitle21) | input$legendTitle21 == "") NULL
  			else input$legendTitle21
  		}
  		
  		opts$font.legendTitle21 = as.numeric(input$font.legendTitle21)
  		opts$col.legendTitle21 = input$col.legendTitle21
  		
  		opts$colPoints = input$colPoints
  		opts$pchFill = input$pchFill
  		opts$xlabels22 = input$xlabels22
  		opts$pchPoints = input$pchPoints
  		opts$pchSize = input$pchSize
  		opts$jitterAmount = input$jitterAmount
  		
  		return(opts)
    })
    
  
    output$cutPointsPlot <- renderPlot({
		
		if (!is.null(input$markerInput) && input$showPlots){
			
			if (!input$cutoffPlotsOpts) opts = grphPrmtrsDefault()
			if (input$cutoffPlotsOpts) opts = grphPrmtrs()
			
			results <- mROC(data=dataM(), statusName=input$statusVar, markerName=input$markerInput, 
			                event=input$valueStatus, diseaseHigher=input$lowhigh)$plotdata
			cut.results <- optimal.cutpoint()
			data = dataM()
			
			coord = results[results[ ,"Marker"] == input$cutoffMarker, ]
			cutvals = coord[ ,"Cutpoint"]
			TPRs = coord[ ,"TPR"]
			FPRs = coord[ ,"FPR"]
			diseased = data[data[,input$statusVar] == input$valueStatus, input$cutoffMarker]
			healthy = data[data[,input$statusVar] != input$valueStatus, input$cutoffMarker]
			dens.diseased = density(diseased)
			dens.healthy = density(healthy)
			
			xmin = min(healthy, diseased)
			xmax = max(healthy, diseased)
			ymax = max(dens.diseased$y, dens.healthy$y)
			
			range = xmax - xmin
			expand = 0.05*range
			
			par(mfrow=c(2,2), mar=c(5.1, 6.1, 5.1, 6.1), family = opts$fontfamily)
			
			## ROC Curve (Figure 11)
			plot(FPRs, TPRs, xlab = "", ylab = "", axes = FALSE, main = "", type = "l", lty = opts$ROClty11, col = opts$ROCcol11)
			box()
			axis(1, col.axis = opts$xcol.axis11, cex.axis = opts$xcex.axis11)
			axis(2, col.axis = opts$ycol.axis11, cex.axis = opts$ycex.axis11)
			abline(coef = c(0, 1), lty = 2)
			
			title(main = opts$main11, font.main = opts$font.main11, cex.main = opts$cex.main11, col.main = opts$col.main11)
			title(xlab = opts$xlab11, font.lab = opts$xfont.lab11, col.lab = opts$xcol.lab11, cex.lab = opts$xcex.lab11)
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

			title(main = opts$main12, font.main = opts$font.main12, cex.main = opts$cex.main12, col.main = opts$col.main12)
			title(xlab = opts$xlab12, font.lab = opts$xfont.lab12, col.lab = opts$xcol.lab12, cex.lab = opts$xcex.lab12)
			title(ylab = opts$ylab12, font.lab = opts$yfont.lab12, col.lab = opts$ycol.lab12, cex.lab = opts$ycex.lab12)

			abline(v=cut.results[[1]][[1]]$optimal.cutoff$cutoff, lty=2, col="gray70")
			
			leg.labels12 = strsplit(opts$legendNames12, ",")[[1]]
			leg.position12 = {
				if (input$legendPos12 == "xy") c(opts$legendXpos12, opts$legendYpos12)
                else opts$legendPos12
            }
			
			if(length(leg.labels12) == 2){
				if (input$legendPos12 == "xy"){
					legend(leg.position12[1], leg.position12[2], legend = leg.labels12, lty = c(opts$sensType, opts$specType), 
							col = c(opts$sensCol, opts$specCol), title = opts$legendTitle12, title.col = opts$col.legendTitle12,
							cex = opts$cex.legend12, bty = ifelse(input$borderless12, "n","o"))
				}
				
				if (input$legendPos12 != "xy"){
					legend(opts$legendPos12, legend = leg.labels12, lty = c(opts$sensType, opts$specType), 
							col = c(opts$sensCol, opts$specCol), title = opts$legendTitle12, title.col = opts$col.legendTitle12,
							cex = opts$cex.legend12, bty = ifelse(input$borderless12, "n","o"))
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
			
			title(main = opts$main21, font.main = opts$font.main21, cex.main = opts$cex.main21, col.main = opts$col.main21)
			title(xlab = opts$xlab21, font.lab = opts$xfont.lab21, col.lab = opts$xcol.lab21, cex.lab = opts$xcex.lab21)
			title(ylab = opts$ylab21, font.lab = opts$yfont.lab21, col.lab = opts$ycol.lab21, cex.lab = opts$ycex.lab21)

			abline(v=cut.results[[1]][[1]]$optimal.cutoff$cutoff, lty=2, col="gray70")
			
			leg.labels21 = strsplit(opts$legendNames21, ",")[[1]]
			leg.position21 = {
				if (input$legendPos21 == "xy") c(opts$legendXpos21, opts$legendYpos21)
                else opts$legendPos21
            }
			
			if(length(leg.labels21) == 2){
				if (input$legendPos21 == "xy"){
					legend(leg.position21[1], leg.position21[2], legend = leg.labels21, lty = c(opts$lineTypeD, opts$lineTypeH), 
							col = c(opts$lineColD, opts$lineColH), title = opts$legendTitle21, title.col = opts$col.legendTitle21,
							cex = opts$cex.legend21, bty = ifelse(input$borderless21, "n","o"))
				}
				
				if (input$legendPos21 != "xy"){
					legend(opts$legendPos21, legend = leg.labels21, lty = c(opts$lineTypeD, opts$lineTypeH), 
							col = c(opts$lineColD, opts$lineColH), title = opts$legendTitle21, title.col = opts$col.legendTitle21,
							cex = opts$cex.legend21, bty = ifelse(input$borderless21, "n","o"))
				}
			}
			
			## Figure 22
			bgD = trimws(strsplit(opts$pchFill, ",")[[1]][2], "both")
			bgH = trimws(strsplit(opts$pchFill, ",")[[1]][1], "both")

			pchColD = trimws(strsplit(opts$colPoints, ",")[[1]][2], "both")
			pchColH = trimws(strsplit(opts$colPoints, ",")[[1]][1], "both")
      
			set.seed(3627)
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
    
}
######################  End Cut Off Tab   ###############################

######################  Begin Sample SizeTab   ###############################


SampleSize <- reactive({
  if(input$sampleSizeMethod == 1){
    SampleSizeSingleTest(input$alpha1, input$power1, input$auc, input$ratio)
    
  } else if (input$sampleSizeMethod == 2){
    SampleSizeTwoTests(input$alpha2, input$power2, input$auc01, input$auc02, input$auc11, input$auc12, input$ratio2)
    
  } else if (input$sampleSizeMethod == 3){
    SampleSizeStandardvsNew(input$alpha3, input$power3, input$aucs,input$aucn, input$sd, input$ratio3)
  }
})

output$SampleSizeForRoc<- renderPrint({ SampleSize() })

output$downloadSampleSizeResults <- downloadHandler(
filename = function() { "Sample_Size_Results.txt" },
content = function(file) {
    #if(input$tabs1 == "Sample size"){
        result = SampleSize()
        out <- capture.output(result)
        write.table(out, file, row.names=F, col.names=F, quote=F)
    #}
}
)

######################  End Sample SizeTab   ###############################

})

