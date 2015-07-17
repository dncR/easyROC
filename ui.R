shinyUI(pageWithSidebar(

	titlePanel("easyROC: a web-tool for ROC curve analysis (ver. 1.1)"),
  
	sidebarPanel(
	# Sol panelde nelerin yer alacağı bu bölümde belirleniyor.
	# Tab panellerden veya sol panel içindeki çeşitli radyo butonlar
	# checkbox'lar gibi seçimlerden sonra panelde nelerin değişeceği de
	# ayrıca bir conditionalPanel() bloğu ile burada tanımlanabiliyor.
	
		conditionalPanel(condition="input.tabs1=='Introduction'",
			HTML('<p><img src="multi.png" width=400 height=400></p>'),
            tags$head(includeScript("google-analytics.js"))

		),

		conditionalPanel(condition="input.tabs1=='Data upload'",
			h4("Input data"),
			#radioButtons("dataInput", "", list("Load example data"=1, "Upload a file"=2, "Paste your data"=3), selected=1),
			radioButtons("dataInput", "", list("Load example data"=1, "Upload a file"=2), selected=1),
			
			selectizeInput("statusVar", "Name of the status variable", choices = NULL, multiple = FALSE),
			textInput("valueStatus", "Value of the status:", ""),
			HTML('<br>'),
			
			conditionalPanel(condition="input.dataInput=='1'",
				h5("Load example data:"),
				radioButtons("sampleData", "", list("Mayo data (n=312, p=4)"=1, "PBC data set (n=418, p=20)"=2), selected=1),
				HTML('<p>n: number of observations</p>'),
				HTML('<p>p: number of variables</p>')
			),

			conditionalPanel(condition="input.dataInput=='2'",
				h5("Upload a delimited text file (max. 10MB): "),
				#HTML('<i class="fa fa-beer fa-lg"></i>'),
				fileInput("upload", "", multiple = FALSE),
				radioButtons("fileSepDF", "Delimiter:", list("Comma"=1, "Tab"=2, "Semicolon"=3, "Space"=4),selected=2),
				
				conditionalPanel(condition="input.fileSepDF!='1'",
					checkboxInput(inputId = "decimal", label = "Use comma as decimal", value = FALSE)
				),
        
				HTML('<br>'),
				HTML('<p>You can upload your data as separated by comma, tab, semicolon or space.</p>'),
				HTML('<p><b>Note</b>: First row must be header.</p>')
			),
			
			conditionalPanel(condition="input.dataInput=='3'",
				h5("Paste or enter your data below:"),
				tags$textarea(id="myData", rows=10, cols=5, ""),
				actionButton('clearText_button','Clear data'),
				HTML('<br>'),
				HTML('<br>'),

				radioButtons("fileSepP", "Separator:", list("Comma"=1,"Tab"=2,"Semicolon"=3), selected=2),
				HTML('<p>You can paste or manually enter your data as separated by comma, tab or semicolon.</p>'),
				HTML('<p>Note: First row must be header.</p>')
			)
		),
		
		conditionalPanel(condition="input.tabs1=='ROC curve'",
			
            selectizeInput("markerInput", "Select markers (*)", choices = NULL, multiple = TRUE),
            checkboxInput("lowhigh", "Higher values indicate disease", TRUE),
			
            HTML('<br>'),
            
            conditionalPanel(condition = "input.navbarROCcurve == 'Multiple Comparisons'",
                selectInput(inputId = "MultipleCompMethod", label = "Multiple Comparison Method", selected = "bonferroni",
                            choices = c("Bonferroni" = "bonferroni", "False discovery rate" = "fdr", "None" = "none")
                            ),
                HTML('<br>')
            ),


            helpText("(*) Multiple markers are allowed."),
            HTML('<br>'),


			checkboxInput(inputId = "advanced", label = "Advanced options", value = FALSE),
            
      		conditionalPanel(condition = "input.advanced",
				radioButtons(inputId = "StdErr", label = "1. Select a method for SE estimation", 
							 choices = list("Mann-Whitney"="MW", "DeLong(1988)[+]"="DeLong", "Under Null Hyp."="Null", "Binomial"="Binomial"), selected = "DeLong"),
							 
				radioButtons(inputId = "ConfInt", label = "2. Select a method for Conf. Interval", 
							 choices = list("Mann-Whitney"="MW", "DeLong(1988)[+]"="DeLong", "Under Null Hyp."="Null", "Binomial Exact"="Exact"), selected = "DeLong"),
				numericInput(inputId = "alpha", label = "Type I error", value = 0.05, min = 0, max = 1, step = 0.01),
				HTML('<br>'),
				HTML('<br>'),
				helpText("[+]: Default options."),
				HTML('<br>')
			),
            
			checkboxInput(inputId = "ROCplotOpts", label = "Plot options", value = FALSE),


            
            conditionalPanel(condition = "input.ROCplotOpts",
                fluidRow(column(5,sliderInput("myheight", "Plot height:", value=400, min=200, max=1200)),
                         column(2),
                         column(5,sliderInput("mywidth", "Plot width:", value=400, min=200, max=1200 ))
                ),
                
                HTML('<br>'),
                
                fluidRow(
                    column(10, selectizeInput("fontfamilyRC", "Font family", 
                                              choices = c("Times New Roman" = "serif", "Arial" = "sans", 
                                                          "Corier New" = "mono"), selected = "sans"))
                ),
                
                HTML('<br>'),
                
                selectizeInput("subGrpsRC", "", choices = c("Edit x-axis" = "xAxis", "Edit y-axis" = "yAxis",
                                                            "Other options" = "others"), selected = "xAxis"),
                
                conditionalPanel(condition = "input.subGrpsRC == 'others'",
                                 fluidRow(
                                     column(5, textInput("mainRC", "Graph title", "ROC Curve")),
                                     column(1),
                                     column(5, selectizeInput("font.mainRC", "Title font", choices = c("Regular" = "1", "Bold" = "2", 
                                                                                                       "Italic" = "3", "Bold Italic" = "4"), 
                                                              selected = "2"))
                                 ),
                                 
                                 fluidRow(
                                     column(5, textInput("col.mainRC", "Title color", "black")),
                                     column(1),
                                     column(5, numericInput("cex.mainRC", "Title size", min=0.1, max=5, value = 1.2, step = 0.1))
                                 ),
                                 
                                 fluidRow(
                                     column(5, textInput("ROCcolRC", "ROC line color", "black,red")),
                                     column(1),
                                     column(5, selectizeInput("ROCltyRC", "ROC line type", 
                                                              choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "1",
                                                                          "\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "2", 
                                                                          "\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "3",
                                                                          "\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "4",
                                                                          "\U2500 \U2500 \U2500 \U2500 \U2500" = "5",
                                                                          "\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "6"),
                                                              selected = "1"))
                                 ),
                                 
                                 textInput(inputId = "legend.namesRC", label = "Legend names", value = "")
                ),
                
                conditionalPanel(condition = "input.subGrpsRC == 'xAxis'",
                                 h5('X-axis options:'),
                                 
                                 ## X axis label options
                                 fluidRow(
                                     column(5, textInput("xlabRC", "Axis label", "1-Specificity")),
                                     column(1),
                                     column(5, selectizeInput("xfont.labRC", "Label font", choices = c("Regular" = "1", "Bold" = "2", 
                                                                                                       "Italic" = "3", "Bold Italic" = "4"), 
                                                              selected = "1"))    
                                 ),
                                 
                                 fluidRow(
                                     column(5, textInput("xcol.labRC", "Label color", "black")),
                                     column(1),
                                     column(5, numericInput("xcex.labRC", "Label size", min=0.1, max=5, value = 1, step = 0.1))
                                 ),
                                 
                                 ## X axis annotation options
                                 fluidRow(
                                     column(5, textInput("xcol.axisRC", "Annotation color", "black")),
                                     column(1),
                                     column(5, numericInput("xcex.axisRC", "Annotation size", min=0.1, max=5, value = 1, step = 0.1))
                                 )
                                 
                                 #fluidRow(
                                 #	column(5, textInput("xcol11", "Axis color", "black")),
                                 #	column(1),
                                 #	column(5, textInput("xcol.ticks11", "Tickmarks color", "black"))
                                 #),
                ),
                
                conditionalPanel(condition = "input.subGrpsRC == 'yAxis'",
                                 h5('Y-axis options:'),
                                 
                                 ## Y axis label options
                                 fluidRow(
                                     column(5, textInput("ylabRC", "Axis label", "Sensitivity")),
                                     column(1),
                                     column(5, selectizeInput("yfont.labRC", "Label font", choices = c("Regular" = "1", "Bold" = "2", 
                                                                                                       "Italic" = "3", "Bold Italic" = "4"), 
                                                              selected = "1"))	
                                 ),
                                 
                                 fluidRow(
                                     column(5, textInput("ycol.labRC", "Label color", "black")),
                                     column(1),
                                     column(5, numericInput("ycex.labRC", "Label size", min=0.1, max=5, value = 1, step = 0.1))
                                 ),
                                 
                                 ## Y axis annotation options
                                 fluidRow(
                                     column(5, textInput("ycol.axisRC", "Annotation color", "black")),
                                     column(1),
                                     column(5, numericInput("ycex.axisRC", "Annotation size", min=0.1, max=5, value = 1, step = 0.1))
                    )
                )

            ),

            conditionalPanel(condition = "input.navbarROCcurve == 'Partial AUC'",

                checkboxInput(inputId = "partialAUC", label = "Partial AUC", value = TRUE),

                    conditionalPanel(condition = "input.partialAUC",

                        fluidRow(
                            column(5, numericInput("pointA", "Value 1", min=0, max=1, value = 0.5, step = 0.1)),
                            column(1),
                            column(5, numericInput("pointB", "Value 2", min=0, max=1, value = 1, step = 0.1))
                        ),

                    selectizeInput("sensSpec", "Select a measure", choices = c("Sensitivity", "Specificity"), multiple = FALSE, selected = "Sensitivity")

                )
            )

        ),





        conditionalPanel(condition="input.tabs1=='Cut points'",
			h5("1. Select a marker"),
            selectizeInput("cutoffMarker", "", choices = NULL, multiple = FALSE),
            HTML('<br>'),
            h5("2. Select a method for optimal cut-off (*)"),
            selectizeInput("cutOffMethods", "", choices = c("Youden", "CB", "MCT", "MinValueSp", "MinValueSe", "ValueSe", "ValueSp", "MinValueSpSe", 
															"MaxSp", "MaxSe", "MaxSpSe", "MaxProdSpSe", "ROC01", "SpEqualSe", "MaxEfficiency", 
															"Minimax", "MaxDOR", "MaxKappa", "MinValueNPV", "MinValuePPV", "ValueNPV", "ValuePPV", 
															"MinValueNPVPPV", "PROC01", "NPVEqualPPV", "MaxNPVPPV", "MaxSumNPVPPV", "MaxProdNPVPPV", 
															"ValueDLR.Negative", "ValueDLR.Positive", "MinPvalue", "ObservedPrev", "MeanPrev", 
															"PrevalenceMatching"), multiple = FALSE),
            #HTML('<br>'),

            conditionalPanel(condition="input.cutOffMethods=='Youden'",
                HTML('<p><b>Youden:</b> Youden index</p>'),
                HTML('<br>'),
                numericInput(inputId = "CFP_Youden", label = "Cost of a False Positive", value = 1, min = 0, max = 9999999),
                numericInput(inputId = "CFN_Youden", label = "Cost of a False Negative", value = 1, min = 0, max = 9999999),
                checkboxInput(inputId = "generalized_Youden", label = "Generalized Youden Index", value = FALSE),
                checkboxInput(inputId = "costs_benefits_Youden", label = "Cost-benefit based Youden Index", value = FALSE)
            ),

            conditionalPanel(condition="input.cutOffMethods=='CB'",
                HTML('<p><b>CB:</b> cost-benefit method</p>'),
                HTML('<br>'),
                numericInput(inputId = "costs_ratio", label = "Cost ratio", value = 1, min = 0, max = 99999999)
            ),

            conditionalPanel(condition="input.cutOffMethods=='MCT'",
                HTML('<p><b>MCT:</b> minimizes misclassification cost term</p>'),
                HTML('<br>'),
                numericInput(inputId = "CFP_MCT", label = "Cost of a False Positive", value = 1, min = 0, max = 9999999),
                numericInput(inputId = "CFN_MCT", label = "Cost of a False Negative", value = 1, min = 0, max = 9999999)
            ),

            conditionalPanel(condition="input.cutOffMethods=='MinValueSp'",
                HTML('<p><b>MinValueSp:</b> a minimum value set for specificity</p>'),
                HTML('<br>'),
                numericInput(inputId = "valueSp_MVSp", label = "Value of Specificity", value = 0.85, min = 0, max = 1, step = 0.01)
            ),

            conditionalPanel(condition="input.cutOffMethods=='MinValueSe'",
                HTML('<p><b>MinValueSe:</b> a minimum value set for sensitivity</p>'),
                HTML('<br>'),
                numericInput(inputId = "valueSe_MVSe", label = "Value of Sensitivity", value = 0.85, min = 0, max = 1, step = 0.01)
            ),

            conditionalPanel(condition="input.cutOffMethods=='ValueSe'",
                HTML('<p><b>ValueSe:</b> a value set for sensitivity</p>'),
                HTML('<br>'),
                numericInput(inputId = "valueSe_VSe", label = "Value of Sensitivity", value = 0.85, min = 0, max = 1, step = 0.01)
            ),
            
			conditionalPanel(condition="input.cutOffMethods=='ValueSp'",
                HTML('<p><b>ValueSp:</b> a value set for specificity</p>'),
                HTML('<br>'),
                numericInput(inputId = "valueSp_VSp", label = "Value of Specificity", value = 0.85, min = 0, max = 1, step = 0.01)
			),

            conditionalPanel(condition="input.cutOffMethods=='MinValueSpSe'",
                HTML('<p><b>MinValueSpSe:</b> a minimum value set for specificity and sensitivity</p>'),
                HTML('<br>'),
                numericInput(inputId = "valueSe_MVSpSe", label = "Value of Sensitivity", value = 0.85, min = 0, max = 1, step = 0.01),
                numericInput(inputId = "valueSp_MVSpSe", label = "Value of Specificity", value = 0.85, min = 0, max = 1, step = 0.01),
                checkboxInput(inputId = "maxSp_MVSpSe", label = "Maximum specificity", value = TRUE)
            ),

            conditionalPanel(condition="input.cutOffMethods=='MaxSp'",
                HTML('<p><b>MaxSp:</b> maximizes specificity</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='MaxSe'",
                HTML('<p><b>MaxSe:</b> maximizes sensitivity</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='MaxSpSe'",
                HTML('<p><b>MaxSpSe:</b> maximizes sensitivity and specificity simultaneously</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='MaxProdSpSe'",
                HTML('<p><b>MaxProdSpSe:</b> maximizes the product of sensitivity and specificity or accuracy area</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='ROC01'",
                HTML('<p><b>ROC01:</b> minimizes distance between ROC plot and point (0,1)</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='SpEqualSe'",
                HTML('<p><b>SpEqualSe:</b> sensitivity = specificity</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='MaxEfficiency'",
                HTML('<p><b>MaxEfficiency:</b> maximizes efficiency or accuracy, similar to minimize error rate </p>'),
                HTML('<br>'),
                checkboxInput(inputId = "costs_benefits_Efficiency", label = "Cost-benefit based Efficiency", value = FALSE),
                checkboxInput(inputId = "standard_deviation_accuracy", label = "Standart deviation accuracy", value = FALSE)
            ),

            conditionalPanel(condition="input.cutOffMethods=='Minimax'",
                HTML('<p><b>Minimax:</b> minimizes the most frequent error</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='MaxDOR'",
                HTML('<p><b>MaxDOR:</b> maximizes diagnostic odds ratio</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='MaxKappa'",
                HTML('<p><b>MaxKappa:</b> maximizes kappa index</p>'),
                HTML('<br>'),
                numericInput(inputId = "CFP_MK", label = "Cost of a False Positive", value = 1, min = 0, max = 9999999),
                numericInput(inputId = "CFN_MK", label = "Cost of a False Negative", value = 1, min = 0, max = 9999999),
                checkboxInput(inputId = "weighted_Kappa", label = "Weighted Kappa", value = FALSE)
            ),

            conditionalPanel(condition="input.cutOffMethods=='MinValueNPV'",
                HTML('<p><b>MinValueNPV:</b> a minimum value set for negative predictive value</p>'),
                HTML('<br>'),
                numericInput(inputId = "valueNPV_MVNPV", label = "Value of NPV", value = 0.85, min = 0, max = 1, step = 0.01)
            ),

            conditionalPanel(condition="input.cutOffMethods=='MinValuePPV'",
                HTML('<p><b>MinValuePPV:</b> a minimum value set for positive predictive value</p>'),
                HTML('<br>'),
                numericInput(inputId = "valuePPV_MVPPV", label = "Value of PPV", value = 0.85, min = 0, max = 1, step = 0.01)
            ),

            conditionalPanel(condition="input.cutOffMethods=='ValueNPV'",
                HTML('<p><b>ValueNPV:</b> a value set for negative predictive value</p>'),
                HTML('<br>'),
                numericInput(inputId = "valueNPV_VNPV", label = "Value of NPV", value = 0.85, min = 0, max = 1, step = 0.01)
            ),

            conditionalPanel(condition="input.cutOffMethods=='ValuePPV'",
                HTML('<p><b>ValuePPV:</b> a value set for positive predictive value</p>'),
                HTML('<br>'),
                numericInput(inputId = "valuePPV_VPPV", label = "Value of PPV", value = 0.85, min = 0, max = 1, step = 0.01)
            ),

            conditionalPanel(condition="input.cutOffMethods=='MinValueNPVPPV'",
                HTML('<p><b>MinValueNPVPPV:</b> a minimum value set for predictive values</p>'),
                HTML('<br>'),
                numericInput(inputId = "valuePPV_MVNPVPPV", label = "Value of PPV", value = 0.85, min = 0, max = 1, step = 0.01),
                numericInput(inputId = "valueNPV_MVNPVPPV", label = "Value of NPV", value = 0.85, min = 0, max = 1, step = 0.01),
                checkboxInput(inputId = "maxNPV_MVNPVPPV", label = "Maximum NPV", value = TRUE)
            ),

            conditionalPanel(condition="input.cutOffMethods=='PROC01'",
                HTML('<p><b>PROC01:</b> minimizes distance between PROC plot and point (0,1)</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='NPVEqualPPV'",
                HTML('<p><b>NPVEqualPPV:</b> negative predictive value = positive predictive value</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='MaxNPVPPV'",
                HTML('<p><b>MaxNPVPPV:</b> maximizes positive predictive value and negative predictive value simultaneously</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='MaxSumNPVPPV'",
                HTML('<p><b>MaxSumNPVPPV:</b> maximizes the sum of the predictive values</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='MaxProdNPVPPV'",
                HTML('<p><b>MaxProdNPVPPV:</b> maximizes the product of predictive values</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='ValueDLR.Negative'",
                HTML('<p><b>ValueDLR.Negative:</b> a value set for negative diagnostic likelihood ratio</p>'),
                HTML('<br>'),
                numericInput(inputId = "valueDLR_Negative", label = "Value of Negative DLR", value = 0.5, min = 0, max = 999999)
            ),

            conditionalPanel(condition="input.cutOffMethods=='ValueDLR.Positive'",
                HTML('<p><b>ValueDLR.Positive:</b> a value set for positive diagnostic likelihood ratio</p>'),
                HTML('<br>'),
                numericInput(inputId = "valueDLR_Positive", label = "Value of Positive DLR", value = 2, min = 0, max = 999999)
            ),

            conditionalPanel(condition="input.cutOffMethods=='MinPvalue'",
                HTML('<p><b>MinPvalue:</b> minimizes p-value associated with the statistical Chi-squared test which measures the association between the marker and the binary result obtained on using the cutpoint</p>'),
                HTML('<br>'),
                selectInput(inputId = "adjusted_pvalue", label = "Adjustment method for p-value", 
                            choices = c("Miller and Siegmund" = "PADJMS", "Altman_5" = "PALT5", "Altman_10" = "PALT10"),
                            multiple = FALSE, selected = "PADJMS")
            ),

            conditionalPanel(condition="input.cutOffMethods=='ObservedPrev'",
                HTML('<p><b>ObservedPrev:</b> the closest value to observed prevalence</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='MeanPrev'",
                HTML('<p><b>MeanPrev:</b> the closest value to the mean of the diagnostic test values</p>')
            ),

            conditionalPanel(condition="input.cutOffMethods=='PrevalenceMatching'",
                HTML('<p><b>PrevalenceMatching:</b> the value for which predicted prevalence is practically equal to observed prevalence</p>')
            ),

            HTML('<br>'),
            HTML('<p>(*) See <a href="http://cran.r-project.org/web/packages/OptimalCutpoints/index.html" target="_blank"> OptimalCutpoints</a> package from R</p>'),
            HTML('<br>'),
            
            checkboxInput("showPlots", "Include plots.", FALSE),
            
            conditionalPanel(condition="input.showPlots",
				fluidRow(
					column(1),
					column(5, sliderInput("myheightCutoff", "Plot height:", value=600, min=400, max=1400)),
					column(1),
					column(5, sliderInput("mywidthCutoff", "Plot width:", value=800, min=400, max=1400 ))
				),
				
				HTML('<br>'),
				checkboxInput("cutoffPlotsOpts", "More plot options (See Manual)", FALSE),
				HTML('<br>')
            ),
            
            conditionalPanel(condition="input.cutoffPlotsOpts",
				fluidRow(
					column(10, selectizeInput("fontfamily", "Font family", 
											choices = c("Times New Roman" = "serif", "Arial" = "sans", 
														"Corier New" = "mono"), selected = "sans"))
				),

				radioButtons("selectedGraph", "", list("Top Left \U2003 \U2003" = 1, "Top Right \U2003 \U2003" = 2, 
														"Bottom Left \U2002\U2008" = 3, "Bottom Right" = 4), selected=1),
				
				HTML('<br>'),
				
				selectizeInput("subGrps", "", choices = NULL, selected = NULL),
				
				## First graph options (11)
				conditionalPanel(condition = "input.selectedGraph == '1'",
					
					conditionalPanel(condition = "input.subGrps == 'others'",
						fluidRow(
						column(5, textInput("main11", "Graph title", "ROC Curve")),
						column(1),
						column(5, selectizeInput("font.main11", "Title font", choices = c("Regular" = "1", "Bold" = "2", 
																						"Italic" = "3", "Bold Italic" = "4"), 
												selected = "2"))
						),
					
						fluidRow(
							column(5, textInput("col.main11", "Title color", "black")),
							column(1),
							column(5, numericInput("cex.main11", "Title size", min=0.1, max=5, value = 1.2, step = 0.1))
						),
					
						fluidRow(
							column(5, textInput("ROCcol11", "ROC line color", "black")),
							column(1),
							column(5, selectizeInput("ROClty11", "ROC line type", 
													choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "1",
																"\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "2", 
																"\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "3",
																"\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "4",
																"\U2500 \U2500 \U2500 \U2500 \U2500" = "5",
																"\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "6"),
													selected = "1"))
						)
					),
					
					conditionalPanel(condition = "input.subGrps == 'xAxis'",
						h5('X-axis options:'),
						
						## X axis label options
						fluidRow(
							column(5, textInput("xlab11", "Axis label", "1-Specificity")),
							column(1),
							column(5, selectizeInput("xfont.lab11", "Label font", choices = c("Regular" = "1", "Bold" = "2", 
																							"Italic" = "3", "Bold Italic" = "4"), 
													selected = "1"))	
						),
						
						fluidRow(
							column(5, textInput("xcol.lab11", "Label color", "black")),
							column(1),
							column(5, numericInput("xcex.lab11", "Label size", min=0.1, max=5, value = 1, step = 0.1))
						),
						
						## X axis annotation options
						fluidRow(
							column(5, textInput("xcol.axis11", "Annotation color", "black")),
							column(1),
							column(5, numericInput("xcex.axis11", "Annotation size", min=0.1, max=5, value = 1, step = 0.1))
						)
						
						#fluidRow(
						#	column(5, textInput("xcol11", "Axis color", "black")),
						#	column(1),
						#	column(5, textInput("xcol.ticks11", "Tickmarks color", "black"))
						#),
					),
					
					conditionalPanel(condition = "input.subGrps == 'yAxis'",
						h5('Y-axis options:'),
						
						## Y axis label options
						fluidRow(
							column(5, textInput("ylab11", "Axis label", "Sensitivity")),
							column(1),
							column(5, selectizeInput("yfont.lab11", "Label font", choices = c("Regular" = "1", "Bold" = "2", 
																								"Italic" = "3", "Bold Italic" = "4"), 
													selected = "1"))	
						),
						
						fluidRow(
							column(5, textInput("ycol.lab11", "Label color", "black")),
							column(1),
							column(5, numericInput("ycex.lab11", "Label size", min=0.1, max=5, value = 1, step = 0.1))
						),
						
						## Y axis annotation options
						fluidRow(
							column(5, textInput("ycol.axis11", "Annotation color", "black")),
							column(1),
							column(5, numericInput("ycex.axis11", "Annotation size", min=0.1, max=5, value = 1, step = 0.1))
						)
					)	
				),
				
				## Second graph options (12)
				conditionalPanel(condition = "input.selectedGraph == '2'",
					
					conditionalPanel(condition = "input.subGrps == 'others'",
						fluidRow(
							column(5, textInput("main12", "Graph title", "Sens. & Spec. Curve")),
							column(1),
							column(5, selectizeInput("font.main12", "Title font", choices = c("Regular" = "1", "Bold" = "2", 
																								"Italic" = "3", "Bold Italic" = "4"), 
													selected = "2"))
						),
					
						fluidRow(
							column(5, textInput("col.main12", "Title color", "black")),
							column(1),
							column(5, numericInput("cex.main12", "Title size", min=0.1, max=5, value = 1.2, step = 0.1))
						),
					
						fluidRow(
							column(5, textInput("specCol", "Spec. line color", "blue")),
							column(1),
							column(5, selectizeInput("specType", "Spec. line type", 
													choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "1",
																"\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "2", 
																"\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "3",
																"\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "4",
																"\U2500 \U2500 \U2500 \U2500 \U2500" = "5",
																"\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "6"),
													selected = "1"))
						),
						
						fluidRow(
							column(5, textInput("sensCol", "Sens. line color", "red")),
							column(1),
							column(5, selectizeInput("sensType", "Sens. line type", 
													choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "1",
																"\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "2", 
																"\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "3",
																"\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "4",
																"\U2500 \U2500 \U2500 \U2500 \U2500" = "5",
																"\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "6"),
													selected = "1"))
						)
					),
					
					conditionalPanel(condition = "input.subGrps == 'Legend'",
						fluidRow(
							column(5, selectizeInput("legendPos12", "Position", choices = c("Top right" = "topright", "Top left" = "topleft", "Bottom right" = "bottomright", "Bottom left" = "bottomleft", "xy coord." = "xy"),
														selected = "topright")),
							column(1),
							column(2,
								conditionalPanel(condition = "input.legendPos12 == 'xy'",
									numericInput("legendXpos12", "x", value = 0, step = 0.1)
								)
							),
							column(1),
							column(2,
								conditionalPanel(condition = "input.legendPos12 == 'xy'",
									numericInput("legendYpos12", "y", value = 0, step = 0.02)
								)
							)
						),
						
						fluidRow(
							column(7, textInput("legendNames12", "Leg. labels (comma sep.)", "Sens.,Spec.")),
							column(1),
							column(3, numericInput("cex.legend12", "Leg. size", min = 0.5, max = 5, step = 0.1, value = 1))
						),
						
						fluidRow(
							column(5, textInput("legendTitle12", "Title", "")),
							column(1),
							column(5, textInput("col.legendTitle12", "Title color", "black"))
						),
                        
                        checkboxInput("borderless12", label = "Remove borders.", value = FALSE)
					),
					
					conditionalPanel(condition = "input.subGrps == 'xAxis'",
						h5('X-axis options:'),
						
						## X axis label options
						fluidRow(
							column(5, textInput("xlab12", "Axis label", "Marker Name")),
							column(1),
							column(5, selectizeInput("xfont.lab12", "Label font", choices = c("Regular" = "1", "Bold" = "2", 
																								"Italic" = "3", "Bold Italic" = "4"), 
													selected = "1"))	
						),
						
						fluidRow(
							column(5, textInput("xcol.lab12", "Label color", "black")),
							column(1),
							column(5, numericInput("xcex.lab12", "Label size", min=0.1, max=5, value = 1, step = 0.1))
						),
						
						## X axis annotation options
						fluidRow(
							column(5, textInput("xcol.axis12", "Annotation color", "black")),
							column(1),
							column(5, numericInput("xcex.axis12", "Annotation size", min=0.1, max=5, value = 1, step = 0.1))
						)
					),
					
					conditionalPanel(condition = "input.subGrps == 'yAxis'",
						h5('Y-axis options:'),
						
						## Y axis label options
						fluidRow(
							column(5, textInput("ylab12", "Axis label", "Value")),
							column(1),
							column(5, selectizeInput("yfont.lab12", "Label font", choices = c("Regular" = "1", "Bold" = "2", 
																								"Italic" = "3", "Bold Italic" = "4"), 
													selected = "1"))	
						),
						
						fluidRow(
							column(5, textInput("ycol.lab12", "Label color", "black")),
							column(1),
							column(5, numericInput("ycex.lab12", "Label size", min=0.1, max=5, value = 1, step = 0.1))
						),
						
						## Y axis annotation options
						fluidRow(
							column(5, textInput("ycol.axis12", "Annotation color", "black")),
							column(1),
							column(5, numericInput("ycex.axis12", "Annotation size", min=0.1, max=5, value = 1, step = 0.1))
						)
					)
				),
				
				## Third graph options (21)
				conditionalPanel(condition = "input.selectedGraph == '3'",
					
					conditionalPanel(condition = "input.subGrps == 'others'",
						fluidRow(
							column(5, textInput("main21", "Graph title", "Distribution Graph")),
							column(1),
							column(5, selectizeInput("font.main21", "Title font", choices = c("Regular" = "1", "Bold" = "2", 
																							"Italic" = "3", "Bold Italic" = "4"), 
													selected = "2"))
						),
					
						fluidRow(
							column(5, textInput("col.main21", "Title color", "black")),
							column(1),
							column(5, numericInput("cex.main21", "Title size", min=0.1, max=5, value = 1.2, step = 0.1))
						),
					
						fluidRow(
							column(6, textInput("lineColD", "Line color (Diseased)", "red")),
							column(1),
							column(5, selectizeInput("lineTypeD", "Line type", 
													choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "1",
																"\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "2", 
																"\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "3",
																"\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "4",
																"\U2500 \U2500 \U2500 \U2500 \U2500" = "5",
																"\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "6"),
													selected = "1"))
						),
						
						fluidRow(
							column(6, textInput("lineColH", "Line color (Healthy)", "blue")),
							column(1),
							column(5, selectizeInput("lineTypeH", "Line type", 
													choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "1",
																"\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "2", 
																"\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "3",
																"\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "4",
																"\U2500 \U2500 \U2500 \U2500 \U2500" = "5",
																"\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "6"),
													selected = "1"))
						)
					),
					
					conditionalPanel(condition = "input.subGrps == 'Legend'",
						fluidRow(
							column(5, selectizeInput("legendPos21", "Position", choices = c("Top right" = "topright", "Top left" = "topleft", "Bottom right" = "bottomright", "Bottom left" = "bottomleft", "xy coord." = "xy"),
														selected = "topright")),
							column(1),
							column(2,
								conditionalPanel(condition = "input.legendPos21 == 'xy'",
									numericInput("legendXpos21", "x", value = 0, step = 0.1)
								)
							),
							column(1),
							column(2,
								conditionalPanel(condition = "input.legendPos21 == 'xy'",
									numericInput("legendYpos21", "y", value = 0, step = 0.1)
								)
							)
						),
						
						fluidRow(
							column(7, textInput("legendNames21", "Leg. labels (comma sep.)", "Diseased,Healthy")),
							column(1),
							column(3, numericInput("cex.legend21", "Leg. Size", min = 0.5, max = 5, step = 0.1, value = 1))
						),
						
						fluidRow(
							column(5, textInput("legendTitle21", "Title", "")),
							column(1),
							column(5, textInput("col.legendTitle21", "Title color", "black"))
						),
                        
						checkboxInput("borderless21", label = "Remove borders.", value = FALSE)
					),
					
					conditionalPanel(condition = "input.subGrps == 'xAxis'",
						h5('X-axis options:'),
						
						## X axis label options
						fluidRow(
							column(5, textInput("xlab21", "Axis label", "Marker Name")),
							column(1),
							column(5, selectizeInput("xfont.lab21", "Label font", choices = c("Regular" = "1", "Bold" = "2", 
																								"Italic" = "3", "Bold Italic" = "4"), 
													selected = "1"))	
						),
						
						fluidRow(
							column(5, textInput("xcol.lab21", "Label color", "black")),
							column(1),
							column(5, numericInput("xcex.lab21", "Label size", min=0.1, max=5, value = 1, step = 0.1))
						),
						
						## X axis annotation options
						fluidRow(
							column(5, textInput("xcol.axis21", "Annotation color", "black")),
							column(1),
							column(5, numericInput("xcex.axis21", "Annotation size", min=0.1, max=5, value = 1, step = 0.1))
						)
					),
					
					conditionalPanel(condition = "input.subGrps == 'yAxis'",
						h5('Y-axis options:'),
						
						## Y axis label options
						fluidRow(
							column(5, textInput("ylab21", "Axis label", "Density")),
							column(1),
							column(5, selectizeInput("yfont.lab21", "Label font", choices = c("Regular" = "1", "Bold" = "2", 
																								"Italic" = "3", "Bold Italic" = "4"), 
													selected = "1"))	
						),
						
						fluidRow(
							column(5, textInput("ycol.lab21", "Label color", "black")),
							column(1),
							column(5, numericInput("ycex.lab21", "Label size", min=0.1, max=5, value = 1, step = 0.1))
						),
						
						## Y axis annotation options
						fluidRow(
							column(5, textInput("ycol.axis21", "Annotation color", "black")),
							column(1),
							column(5, numericInput("ycex.axis21", "Annotation size", min=0.1, max=5, value = 1, step = 0.1))
						)
					)
				),
				
				## Fourth graph options (22)
				conditionalPanel(condition = "input.selectedGraph == '4'",
					conditionalPanel(condition = "input.subGrps == 'others'",
						fluidRow(
							column(5, textInput("main22", "Graph title", "Distribution Graph")),
							column(1),
							column(5, selectizeInput("font.main22", "Title font", choices = c("Regular" = "1", "Bold" = "2", 
																							"Italic" = "3", "Bold Italic" = "4"), 
													selected = "2"))
						),
					
						fluidRow(
							column(5, textInput("col.main22", "Title color", "black")),
							column(1),
							column(5, numericInput("cex.main22", "Title size", min=0.1, max=5, value = 1.2, step = 0.1))
						),
						
						numericInput("jitterAmount", "Amount of jittering", min=0.01, max = 0.5, value = 0.05, step = 0.01),
						
						## pch options for Healthy and Diseased subjects
						fluidRow(
							column(8, textInput("colPoints", "Point colors (comma sep.)", "black,black")),
							column(1),
							column(2, numericInput("pchPoints", "Type", min = 0, max = 25, step = 1, value = 1))
						),
						
						fluidRow(
							column(3, numericInput("pchSize", "Point size", min = 0.1, max = 5, step = 0.1, value = 1)),
							column(1),
							column(7,
								conditionalPanel(condition = "input.pchPoints > 20",
									textInput("pchFill", "Fill points with color" ,"white,white")
								)
							)
						)
					),
					
					conditionalPanel(condition = "input.subGrps == 'xAxis'",
						h5('X-axis options:'),
						
						## X axis label options
						fluidRow(
							column(5, textInput("xlab22", "Axis label", "Disease Status")),
							column(1),
							column(5, selectizeInput("xfont.lab22", "Label font", choices = c("Regular" = "1", "Bold" = "2", 
																							"Italic" = "3", "Bold Italic" = "4"), 
													selected = "1"))	
						),
						
						fluidRow(
							column(5, textInput("xcol.lab22", "Label color", "black")),
							column(1),
							column(5, numericInput("xcex.lab22", "Label size", min=0.1, max=5, value = 1, step = 0.1))
						),
						
						## X axis annotation options
						fluidRow(
							column(12, textInput("xlabels22", "Annotation names (comma seperated)", "Healthy,Diseased"))
						),
						
						fluidRow(
							column(5, textInput("xcol.axis22", "Annotation color", "black")),
							column(1),
							column(5, numericInput("xcex.axis22", "Annotation size", min=0.1, max=5, value = 1, step = 0.1))
						)
					),
					
					conditionalPanel(condition = "input.subGrps == 'yAxis'",
						h5('Y-axis options:'),
						
						## Y axis label options
						fluidRow(
							column(5, textInput("ylab22", "Axis label", "Marker Name")),
							column(1),
							column(5, selectizeInput("yfont.lab22", "Label font", choices = c("Regular" = "1", "Bold" = "2", 
																								"Italic" = "3", "Bold Italic" = "4"), 
													selected = "1"))	
						),
						
						fluidRow(
							column(5, textInput("ycol.lab22", "Label color", "black")),
							column(1),
							column(5, numericInput("ycex.lab22", "Label size", min=0.1, max=5, value = 1, step = 0.1))
						),
						
						## Y axis annotation options
						fluidRow(
							column(5, textInput("ycol.axis22", "Annotation color", "black")),
							column(1),
							column(5, numericInput("ycex.axis22", "Annotation size", min=0.1, max=5, value = 1, step = 0.1))
						)
					)	
				)
			)
        ),


conditionalPanel(condition="input.tabs1=='Sample size'",

h5("Choose one of the following methods (*):"),
HTML('<br>'),


checkboxInput("singleTest", "Single diagnostic test", FALSE),

conditionalPanel(condition="input.singleTest",

numericInput(inputId = "alpha1", label = "Type I error", value = 0.05, min = 0, max = 1, step = 0.01),
numericInput(inputId = "power1", label = "Power", value = 0.80, min = 0, max = 1, step = 0.1),
numericInput(inputId = "auc", label = "Area under the ROC curve", value = 0.60, min = 0.5, max = 1, step = 0.1),
numericInput(inputId = "ratio", label = "Allocation ratio", value = 1, min = 0, step = 0.1)

),

HTML('<br>'),

checkboxInput("twoTests", "Comparison of two diagnostic tests", FALSE),

conditionalPanel(condition="input.twoTests",

numericInput(inputId = "alpha2", label = "Type I error", value = 0.05, min = 0, max = 1, step = 0.01),
numericInput(inputId = "power2", label = "Power", value = 0.80, min = 0, max = 1, step = 0.1),
numericInput(inputId = "auc01", label = "AUC for 1st test under null hypothesis", value = 0.80, min = 0.5, max = 1, step = 0.1),
numericInput(inputId = "auc02", label = "AUC for 2nd test under null hypothesis", value = 0.80, min = 0.5, max = 1, step = 0.1),
numericInput(inputId = "auc11", label = "AUC for 1st test under alternative hypothesis", value = 0.90, min = 0.5, max = 1, step = 0.1),
numericInput(inputId = "auc12", label = "AUC for 2nd test under alternative hypothesis", value = 0.70, min = 0.5, max = 1, step = 0.1),
numericInput(inputId = "ratio2", label = "Allocation ratio", value = 1, min = 0, step = 0.1)

),
HTML('<br>'),


checkboxInput("StandardVsNew", "Noninferiority of a new test to a standard test", FALSE),


conditionalPanel(condition="input.StandardVsNew",

numericInput(inputId = "alpha3", label = "Type I error", value = 0.05, min = 0, max = 1, step = 0.01),
numericInput(inputId = "power3", label = "Power", value = 0.80, min = 0, max = 1, step = 0.1),
numericInput(inputId = "aucs", label = "AUC for standard test", value = 0.80, min = 0.5, max = 1, step = 0.1),
numericInput(inputId = "aucn", label = "AUC for new test", value = 0.80, min = 0.5, max = 1, step = 0.1),
numericInput(inputId = "sd", label = "Smallest difference", value = 0.1, step = 0.1),
numericInput(inputId = "ratio3", label = "Allocation ratio", value = 1, min = 0, step = 0.1)

),

HTML('<br>'),
HTML('<p>(*) See <a href="http://66.199.228.237/boundary/complex_decision_making_and_ethics/ROC_Analysis.pdf" target="_blank"> Obuchowski, 2005</a> for further details.</p>'),
HTML('<br>')


),


        conditionalPanel(condition="input.tabs1=='Manual'",
			  HTML('<p align="center"><img src="manual.png" width=200 height=200></p>')
		),

		conditionalPanel(condition="input.tabs1=='Authors & News'",
		    HTML('<p align="center"> <a href="https://www.hacettepe.edu.tr/english/" target="_blank"><img src="hulogo.JPEG" width=150 height=150></a> </p>')
		)
	),


	mainPanel(
	# Tab panelinden herhangi bir sekme seçildiği zaman ana bölmede (Ekranın orta kısmı)
	# nelerin görüneceği bu kısımda tanımlanıyor. Belirli koşullar tanımlanmak istenir ise
	# ayrıca bir conditionalPanel() bloğu burada tanımlanabilir.
		tabsetPanel(
			tabPanel(title="Introduction", 
		         h5("The easiest way to perform ROC analysis!"),
		         HTML('<p> A receiver operating characteristics (ROC) curve is a graphical approach which 
                      assess the performance of a binary classifier system. The ROC curve analysis is widely 
                      used in medicine, radiology, biometrics and various application of machine learning. </p>'),
		         HTML('<p align="justify"> Here we developed an easy way to carry out ROC analysis.
                      This application creates ROC curves, calculates area under the curve (AUC)
                      values and confidence intervals for the AUC values, and performs multiple comparisons 
                      for ROC curves in a user-friendly, up-to-date and comprehensive way. Moreover,
                      easyROC computes and compares partial AUCs. It can also perform sample size calculation.</p>'),
		         
		         HTML('<p> An important feature of this application is to determine cut-off values especially 
                      for diagnostic tests. For this task, we made use of 
                      <a href="http://cran.r-project.org/web/packages/OptimalCutpoints/index.html" target="_blank">OptimalCutpoints</a> package (Lopez-Raton et al, 2014) of R [1].</p>'),

#HTML('<br>'),

                HTML('<p><div align="center"><table cellpadding="0" cellspacing="0"><tr><td><img src="ROCplot.png" width="300" height="300" border="10000"></td><td><img src="CutOff_Plots.png" width="400" height="400" border="70"></td></tr></table></div></p>'),

# HTML('<br>'),


                h6("[1] Monica Lopez-Raton, Maria Xose Rodriguez-Alvarez, Carmen Cadarso Suarez, Francisco Gude Sampedro (2014). OptimalCutpoints: An R Package for Selecting Optimal Cutpoints in Diagnostic Tests. Journal of Statistical Software, 61(8), 1-36.")
			),

			tabPanel("Data upload",
                navbarPage(title = '',
                    tabPanel('Data', dataTableOutput('RawData'))
                )
            ),

            tabPanel("ROC curve",
                downloadButton("downloadROCStats", "Download ROC statistics as txt-file"),
                downloadButton("downloadROCData", "Download ROC coordinates as txt-file"),
                downloadButton("downloadROCPlot", "Download plot as pdf-file"),
                
                h4(textOutput(outputId = "section1")),
                navbarPage(id = "navbarROCcurve",
                    title = '',
                    tabPanel('Statistics',          dataTableOutput('ROCstatistics')),
                    tabPanel('ROC Coordinates',     dataTableOutput('ROCcoordinates')),
                    tabPanel('Multiple Comparisons',     dataTableOutput('ROCcomparisons')),
                    tabPanel('Partial AUC',     dataTableOutput('resultPAuc'))



                ),

                conditionalPanel(condition = "input.navbarROCcurve != 'ROC Coordinates'",
					textOutput(outputId = "CIreminderLine1"),
					h6(textOutput(outputId = "CIreminderLine2"))
                ),
                
                HTML('<br>'),
                h4(textOutput(outputId = "section2")),
                plotOutput("ROCplot")



#verbatimTextOutput("resultPAuc")
            ),

            tabPanel("Cut points",
                downloadButton("downloadCutOffresults", "Download results as txt-file"),
                downloadButton("downloadCutOffPlotPDF", "Download plots as pdf-file"),   
                verbatimTextOutput("cutPoints"),
				
                ## İçeriği sayfada ortalamak için kullanılan HTML kodu.
                HTML('<div align="center">'),
					plotOutput("cutPointsPlot"),
				HTML('</div>')
			),

tabPanel(title="Sample size",

downloadButton("downloadSampleSizeResults", "Download results as txt-file"),
verbatimTextOutput("SampleSizeForRoc")


),

            tabPanel(title="Manual",
                h5("Detailed manual will be released soon...")
            ),

			tabPanel("Authors & News",
                h4("Authors"),
    	        HTML('<p><a href="http://www.biostatistics.hacettepe.edu.tr/cv/Dincer_Goksuluk_CV_Eng.pdf" target="_blank"> <b>Dincer Goksuluk</b></a><p>'),
    	        HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
    	        HTML('<p><a href="mailto:dincer.goksuluk@hacettepe.edu.tr" target="_blank">dincer.goksuluk@hacettepe.edu.tr</a><p>'),
                HTML('<p><a href="http://yunus.hacettepe.edu.tr/~selcuk.korkmaz/" target="_blank"> <b>Selcuk Korkmaz</b></a><p>'),
                HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
                HTML('<p><a href="mailto:selcuk.korkmaz@hacettepe.edu.tr" target="_blank">selcuk.korkmaz@hacettepe.edu.tr</a><p>'),
                HTML('<p><a href="http://www.biostatistics.hacettepe.edu.tr/cv/Gokmen_Zararsiz_CV_Eng.pdf" target="_blank"> <b>Gokmen Zararsiz</b></a><p>'),
                HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
                HTML('<p><a href="mailto:gokmen.zararsiz@hacettepe.edu.tr" target="_blank">gokmen.zararsiz@hacettepe.edu.tr</a><p>'),
                HTML('<br>'),
		        h4("News"),
                HTML('<br>'),
                HTML('<p><b> Version 1.1 (June 23, 2015) </b><p>'),
                HTML('<p> (1) Partial AUC feature has been added.<p>'),
                HTML('<p> (2) Sample size calculation tab has been added.<p>'),
                HTML('<p> (3) Minor improvements and bug fixes.<p>'),

                HTML('<br>'),
                HTML('<p><b> Version 1.0 (March 19, 2015)</b><p>'),
		        HTML('<p> (1) Initial version has been released.<p>'),
                HTML('<br>'),


            h5("Other Tools"),

            HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/MLViS/" target="_blank"> <b>MLViS: a machine learning-based virtual screening tool</b></a><p>'),
            HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/MVN/" target="_blank"> <b>MVN: a web-tool for assessing multivariate normality </b></a><p>'),
            HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/DDNAA/" target="_blank"> <b>DDNAA: Decision support system for differential diagnosis of nontraumatic acute abdomen </b></a><p>'),
            HTML('<br>'),

                h6("Please feel free to send us bugs and feature requests.")
            ),

            #tabPanel("News",
            #    h4("New Features")
            #),
            id="tabs1"
            ),

            tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
            tags$style(type="text/css", "select { max-width: 200px; }"),
            tags$style(type="text/css", "textarea { max-width: 185px; }"),
            tags$style(type="text/css", ".jslider { max-width: 200px; }"),
            tags$style(type='text/css', ".well { max-width: 330px; }"),
            tags$style(type='text/css', ".span4 { max-width: 330px; }")),

            tags$head(
            tags$link(rel = "shortcut icon", href = "favicon-2.ico"))

        )
))




