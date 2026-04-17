# Maximum file upload size is 30 MB.
options(shiny.maxRequestSize = 30*1024^2)

shinyServer(function(input, output, session) {
  source("R/logging_utils.R")
  source("R/mod_data_upload.R")
  source("R/mod_roc_analysis.R")
  source("R/mod_partial_auc.R")
  source("R/mod_cut_points.R")
  source("R/mod_sample_size.R")
  source("R/plot_options_service.R")
  source("R/mod_downloads.R")
  source("R/shared_state.R")
  source("R/ROCplot.R")
  source("R/printCutOff.R")

  easyroc_log(
    level = "INFO",
    event = "session_started",
    context = list(
      module = "server",
      session = session$token,
      r_config = Sys.getenv("R_CONFIG_ACTIVE", "unset")
    )
  )

  session$onSessionEnded(function() {
    easyroc_log(
      level = "INFO",
      event = "session_ended",
      context = list(module = "server", session = session$token)
    )
  })
      
    

### REACTIVE FUNCTIONS  ###
{
  shared_state <- createSharedState()
  validateSharedState(shared_state)
  mod_data_upload_server("data_upload", shared_state = shared_state)
  roc_analysis <- mod_roc_analysis_server("roc_analysis", shared_state = shared_state, root_input = input)
  partial_auc <- mod_partial_auc_server("partial_auc", shared_state = shared_state, root_input = input)
  cut_points <- mod_cut_points_server("cut_points", shared_state = shared_state, root_input = input)
  sample_size <- mod_sample_size_server("sample_size", root_input = input)

  dataM <- reactive(shared_state$data())
  statusVar <- reactive(shared_state$status_var())
  valueStatus <- reactive(shared_state$event_value())
    
  heightsize <- reactive(input$myheight)
	widthsize <- reactive(input$mywidth)
	
	heightsizeCutoff <- reactive(input$myheightCutoff)
	widthsizeCutoff <- reactive(input$mywidthCutoff)
	
	#heightsizeCutoff <- reactive(600)
	#widthsizeCutoff <- reactive(800)
	
}

###  END REACTIVE FUNCTIONS ###
  

   
####  OBSERVER FUNCTIONS #### 
{
  observe({
    data_tmp <- dataM()
    if (!is.null(data_tmp)){
      current_status <- statusVar()
      marker_choices <- colnames(dataM())
      if (!is.null(current_status) && current_status != ""){
        marker_choices <- marker_choices[marker_choices != current_status]
      }
      updateSelectInput(session, "markerInput", choices = marker_choices, selected = NULL)
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
	  if (input$ROCplotOpts && input$legend.namesRC == ""){
	    updateTextInput(session, inputId = "legend.namesRC", 
	                    value = paste("Marker", 1:length(input$markerInput), sep = "", collapse = ","))
	  } 
	})
	
	# Observe line colors for ROC curves under "ROC Curve" tab when Plot Options is activated.
	observe({
	  if (input$ROCplotOpts && input$ROCcolRC == ""){
	    updateTextInput(session, inputId = "ROCcolRC", 
	                    value = paste0(1:length(input$markerInput), collapse = ","))
	  } 
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
  download_specs <- list()

	# 2. PDF Format
	download_specs$downloadCutOffPlotPDF <- create_download_handler_spec(
		filename = function(){paste('CutOff_Plots.pdf')},
		content = function(file){
			pdf(file, height = input$myheightCutoff/96, width = input$mywidthCutoff/96)
			#pdf(file, height = 5.9, width = 5.9)
				if (!is.null(input$markerInput) && input$showPlots && input$tabs1 == "Cut points"){
			
					if (!input$cutoffPlotsOpts) opts = grphPrmtrsDefault()
					if (input$cutoffPlotsOpts) opts = grphPrmtrs()
					
							results <- cut_points$cutoff_roc_coordinates()
							cut.results <- cut_points$optimal_cutpoint()
							if (is.null(results) || is.null(cut.results)) {
								dev.off()
								return(invisible(NULL))
							}
						data = dataM()
					
					coord = results[results[,"Marker"] == input$cutoffMarker,]
					cutvals = coord[ ,"Cutpoint"]
					TPRs = coord[ ,"TPR"]
					FPRs = coord[ ,"FPR"]
					diseased = data[data[,statusVar()] == valueStatus(), input$cutoffMarker]
					healthy = data[data[,statusVar()] != valueStatus(), input$cutoffMarker]
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
		content_type = 'application/pdf'
	)
}

{ ##  Download ROC Stats.
    download_specs$downloadROCStats <- create_download_handler_spec(
        filename = function() { "ROC_Statistics.txt" },
        content = function(file) {
          if (isTRUE(roc_analysis$is_active())) {
            out <- roc_analysis$roc_statistics()
          } else {
            out <- NULL
          }

          if (is.null(out)) {
            return(invisible(NULL))
          }

          colnames(out) <- c("Marker","AUC","SE.AUC","LowerLimit","UpperLimit","z","p-value")
          out[,-1] <- round(out[,-1], 5)
          write.table(out, file, row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t") 
        }
    )   
}

{ ##  Download ROC Coordinates.
  download_specs$downloadROCData <- create_download_handler_spec(
    filename = function() { "ROC_Coordinates.txt" },
    content = function(file) {

      if (isTRUE(roc_analysis$is_active())) {
        out <- roc_analysis$roc_coordinates()
      } else {
        out <- NULL
      }

      if (is.null(out)) {
        return(invisible(NULL))
      }
      write.table(out, file, row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t") 
    }
  )   
}

{
    # Download ROCPlot (pdf format)
    download_specs$downloadROCPlot <- create_download_handler_spec(
        filename = function(){paste('ROCplot.pdf')},
        content = function(file){
            pdf(file, height = input$myheightCutoff/96, width = input$mywidthCutoff/96)
          
            if (isTRUE(roc_analysis$is_active())){
              results <- roc_analysis$roc_coordinates()
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
        content_type = 'application/pdf'
    )
}


{ ##  Download Cut-off results.
    download_specs$downloadCutOffresults <- create_download_handler_spec(
        filename = function() { "CutOff_Results.txt" },
        content = function(file) {
            if (!is.null(input$markerInput) & input$tabs1 == "Cut points"){
                res = cut_points$optimal_cutpoint()
                if (is.null(res)) {
                  return(invisible(NULL))
                }
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
		if (isTRUE(roc_analysis$is_active())){
		  roc_analysis$roc_statistics()
		}
			
	})
	
	output$ROCcoordinates <- renderDataTable(options = list(iDisplayLength = 10), {
	  if (isTRUE(roc_analysis$is_active())){
      roc_analysis$roc_coordinates()
	  }
	})
		
    
    output$ROCcomparisons <- renderDataTable(options = list(iDisplayLength = 10),
		{
        if (isTRUE(roc_analysis$is_active()) && length(input$markerInput) >= 2){
          roc_analysis$roc_comparisons()
        }
      }
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
		if(isTRUE(roc_analysis$is_active())){

      results <- roc_analysis$roc_coordinates()
		  
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
    
    
    output$resultPAuc <- renderDataTable(options = list(iDisplayLength = 10),
    {
        if(isTRUE(partial_auc$is_active()))
          partial_auc$pauc_result()
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
    
    output$cutPoints <- renderPrint({
        if (isTRUE(cut_points$is_active())){
			res = cut_points$optimal_cutpoint()
			if (is.null(res)) {
			  return(invisible(NULL))
			}
			printCutOff(res)
        }
    })
    
    grphPrmtrsDefaultRC <- reactive({
      build_roc_plot_options_default(
        marker_count = length(input$markerInput),
        legend_names_input = input$legend.namesRC
      )
    })

    grphPrmtrsRC <- reactive({
      build_roc_plot_options_custom(root_input = input)
    })

    grphPrmtrsDefault <- reactive({
      build_cutoff_plot_options_default(cutoff_marker = input$cutoffMarker)
    })

    grphPrmtrs <- reactive({
      build_cutoff_plot_options_custom(root_input = input)
    })
    
  
    output$cutPointsPlot <- renderPlot({
		
		if (!is.null(input$markerInput) && input$showPlots){
			
			if (!input$cutoffPlotsOpts) opts = grphPrmtrsDefault()
			if (input$cutoffPlotsOpts) opts = grphPrmtrs()
			
					results <- cut_points$cutoff_roc_coordinates()
					cut.results <- cut_points$optimal_cutpoint()
					if (is.null(results) || is.null(cut.results)) {
						return(invisible(NULL))
					}
				data = dataM()
			
			coord = results[results[ ,"Marker"] == input$cutoffMarker, ]
			cutvals = coord[ ,"Cutpoint"]
			TPRs = coord[ ,"TPR"]
			FPRs = coord[ ,"FPR"]
			diseased = data[data[,statusVar()] == valueStatus(), input$cutoffMarker]
			healthy = data[data[,statusVar()] != valueStatus(), input$cutoffMarker]
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


output$SampleSizeForRoc<- renderPrint({
  if (isTRUE(sample_size$is_active())) {
    sample_size$sample_size_result()
  }
})

download_specs$downloadSampleSizeResults <- create_download_handler_spec(
filename = function() { "Sample_Size_Results.txt" },
content = function(file) {
        out <- sample_size$sample_size_lines()
        if (is.null(out)) {
          return(invisible(NULL))
        }
        write.table(out, file, row.names=F, col.names=F, quote=F)
}
)

######################  End Sample SizeTab   ###############################

register_download_handlers(output, download_specs)

})
