shinyServer(function(input, output, session) {

    source("classes.R")
    source("hzTest.R")
    source("roystonTest.R")
    source("mardiaTest.R")
    source("mvnPlot.R")
    source("dhTest.R")
    source("outlier.R")
    library("mvoutlier")
    library("nortest")
    library("robustbase")
    library("asbio")
    library("moments")
    library("MASS")
    library("shiny")
    library("plyr")
     

    
    observe({
		if (input$clearText_button == 0) return()
		isolate({ updateTextInput(session, "myData", label = ",", value = "") })
	})
	

	
	dataM <- reactive({  ## Data input.
		if(input$dataInput==1){  ## Load example data.
            
            if(input$sampleData==1){
				data <- read.table("bivariate.txt", header=TRUE)
            }
			
			else if(input$sampleData==2){

                data <- iris[1:50,1:2]
            }
			
			else if(input$sampleData==3){
				data <- iris[,-4]
			}
		} 
		
		else if(input$dataInput==2){  ## Upload data.
			
            inFile <- input$upload

            mySep <- switch(input$fileSepDF, '1'=",",'2'="\t",'3'=";", '4'="")
            
			if (is.null(input$upload))  {return(NULL)}
						
            if (file.info(inFile$datapath)$size <= 10485800){
				data <- read.table(inFile$datapath, sep=mySep, header=TRUE, fill=TRUE)
			}
            
            else print("File is bigger than 10MB and will not be uploaded.")
            
		} 
		
		else {  ## Paste data.
			if(is.null(input$myData)) {return(NULL)}
            
			
			tmp <- matrix(strsplit(input$myData, "\n")[[1]])
			mySep <- switch(input$fileSepP, '1'=",",'2'="\t",'3'=";")
			myColnames <- strsplit(tmp[1], mySep)[[1]]
			data <- matrix(0, length(tmp)-1, length(myColnames))
			colnames(data) <- myColnames
            
			for(i in 2:length(tmp)){
				myRow <- as.numeric(strsplit(paste(tmp[i],mySep,mySep,sep=""), mySep)[[1]])
				data[i-1,] <- myRow[-length(myRow)]
			}
			
            data <- data.frame(data)
		}
		return(data)
	})


	
    heightSize <- reactive(input$myHeight)
	widthSize <- reactive(input$myWidth)
    
    heightsize <- reactive(input$myheight)
	widthsize <- reactive(input$mywidth)

    
  output$MVN <- renderPrint({
  
    if(input$firstLast==0){   ## Single group MVN test
	
        if(input$testType=='0'){
			dataset <- dataM()
			mardiaTest(dataset, qqplot=FALSE)
        } 
		
		else if(input$testType=='1'){
            dataset <- dataM()
            hzTest(dataset, qqplot=FALSE)
        } 
		
		else if(input$testType=='2'){
            dataset <- dataM()
            roystonTest(dataset, qqplot=FALSE)
        }
		
		else if(input$testType=='3'){
            dataset <- dataM()
            dhTest(dataset, qqplot=FALSE)
        }
    }
	
	else if (input$firstLast == 1){  ## First column is "Group" variable.
        
		if(input$testType=='0'){
            dataset <- dataM()
			dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
            lapply(dataset.split, function(x)mardiaTest(x,qqplot=FALSE))
			 			
        } 
		
		else if(input$testType=='1'){
            dataset <- dataM()
			dataset.split = split(dataset[,-1], dataset[,1])
			lapply(dataset.split, function(x)hzTest(x,qqplot=FALSE))
			
        } 
		
		else if(input$testType=='2'){
            dataset <- dataM()
			dataset.split = split(dataset[,-1], dataset[,1])
			lapply(dataset.split, function(x)roystonTest(x,qqplot=FALSE))
             
        }
		
		else if(input$testType=='3'){
            dataset <- dataM()
			dataset.split = split(dataset[,-1], dataset[,1])
			lapply(dataset.split, function(x)dhTest(x,qqplot=FALSE))
            
        }
    }
	
	else if(input$firstLast == 2){  ## Last column is "Group" variable.
        
		if(input$testType=='0'){
            dataset <- dataM()
			ind.last = dim(dataset)[2]
			dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
			lapply(dataset.split, function(x)mardiaTest(x,qqplot=FALSE))
			
        } 
		
		else if(input$testType=='1'){
			dataset <- dataM()
			ind.last = dim(dataset)[2]
			dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
			lapply(dataset.split, function(x)hzTest(x,qqplot=FALSE))
           
        } 
		
		else if(input$testType=='2'){
            dataset <- dataM()
			ind.last = dim(dataset)[2]
			dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
			lapply(dataset.split, function(x)roystonTest(x,qqplot=FALSE))
            
        }
		
		else if(input$testType=='3'){
            dataset <- dataM()
			ind.last = dim(dataset)[2]
			dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
			lapply(dataset.split, function(x)dhTest(x,qqplot=FALSE))
            
        } 
    }
  })
  


  output$mvnPLOT <- renderPlot({
  
    if (input$firstLast==0){  ## Single group MVN test, plot rendering..
	
		if (input$plotType=='0'){  ## Q-Q Plots.
          
			if (input$testType=='0'){
				dataset <- dataM()
				mardiaTest(dataset, qqplot=TRUE)
			} 
			
			else if (input$testType=='1'){
				dataset <- dataM()
				hzTest(dataset, qqplot=TRUE)
			} 
			
			else if (input$testType=='2'){
				dataset <- dataM()
				roystonTest(dataset, qqplot=TRUE)
			}
          
			else if (input$testType=='3'){
				dataset <- dataM()
				dhTest(dataset, qqplot=TRUE)
			}
		}
		
		if (input$plotType=='1'){  ## Perspective Plots.
			dataset <- dataM()
			result <- mardiaTest(dataset, qqplot=FALSE)
			mvnPlot(result, type="persp")
		}
		
		else if (input$plotType=='2'){	## Contour plots.
			dataset <- dataM()
			result <- mardiaTest(dataset)
			mvnPlot(result, type="contour")
		}
	}

    if (input$firstLast == 1){  ## First column is "Group" variable.
        
		if (input$plotType=='0'){  ## Q-Q Plots.
              
            if (input$testType=='0'){
                dataset <- dataM()
				dataset.split = split(dataset[,-1], dataset[,1])
				nrow.plot = length(dataset.split)		
				ncol.plot = 1							
				
				par(mfrow=c(nrow.plot, ncol.plot))
                lapply(dataset.split, function(x)mardiaTest(x, qqplot=TRUE))
				
            } 
			
			else if(input$testType=='1'){
                dataset <- dataM()
				dataset.split = split(dataset[,-1], dataset[,1])
                nrow.plot = length(dataset.split)		
				ncol.plot = 1							
				
				par(mfrow=c(nrow.plot, ncol.plot))
                lapply(dataset.split, function(x)hzTest(x, qqplot=TRUE))
				
            } 
			
			else if(input$testType=='2'){
                dataset <- dataM()
				dataset.split = split(dataset[,-1], dataset[,1])
				nrow.plot = length(dataset.split)		
				ncol.plot = 1							
				
				par(mfrow=c(nrow.plot, ncol.plot))
                lapply(dataset.split, function(x)roystonTest(x, qqplot=TRUE))
                
            }
              
            else if(input$testType=='3'){
                dataset <- dataM()
				dataset.split = split(dataset[,-1], dataset[,1])
				nrow.plot = length(dataset.split)		
				ncol.plot = 1							
				
				par(mfrow=c(nrow.plot, ncol.plot))
                lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
                
            }
		}
		
		if (input$plotType=='1'){  ## Perspective Plots.
			dataset <- dataM()
			dataset.split = split(dataset[,-1], dataset[,1])
			nrow.plot = length(dataset.split)		
			ncol.plot = 1							
			
			par(mfrow=c(nrow.plot, ncol.plot), mar=c(1,0.5,2,0))
			#par(mfrow=c(1, 3), mar=c(0,0.5,2,0))
			result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
			lapply(result, function(x)mvnPlot(x, type="persp"))
		}
		
		else if (input$plotType=='2'){	## Contour plots.
			dataset <- dataM()
			dataset.split = split(dataset[,-1], dataset[,1])
			nrow.plot = length(dataset.split)		
			ncol.plot = 1							
			
			par(mfrow=c(nrow.plot, ncol.plot))
			result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
			lapply(result, function(x)mvnPlot(x, type="contour"))
		}
		
	}
      
    if(input$firstLast == 2){	## Last column is "Group" variable.
	
        if (input$plotType=='0'){  ## Q-Q Plots.
              
            if (input$testType=='0'){
                dataset <- dataM()
				ind.last = dim(dataset)[2]
				dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
				nrow.plot = length(dataset.split)		
				ncol.plot = 1							
				
				par(mfrow=c(nrow.plot, ncol.plot))
                lapply(dataset.split, function(x)mardiaTest(x, qqplot=TRUE))
				
            } 
			
			else if(input$testType=='1'){
                dataset <- dataM()
				ind.last = dim(dataset)[2]
				dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                nrow.plot = length(dataset.split)		
				ncol.plot = 1							
				
				par(mfrow=c(nrow.plot, ncol.plot))
                lapply(dataset.split, function(x)hzTest(x, qqplot=TRUE))
				
            } 
			
			else if(input$testType=='2'){
                dataset <- dataM()
				ind.last = dim(dataset)[2]
				dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
				nrow.plot = length(dataset.split)		
				ncol.plot = 1							
				
				par(mfrow=c(nrow.plot, ncol.plot))
                lapply(dataset.split, function(x)roystonTest(x, qqplot=TRUE))
                
            }
              
            else if(input$testType=='3'){
                dataset <- dataM()
				ind.last = dim(dataset)[2]
				dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
				nrow.plot = length(dataset.split)		
				ncol.plot = 1							
				
				par(mfrow=c(nrow.plot, ncol.plot))
                lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
                
            }
		}
		
		if (input$plotType=='1'){  ## Perspective Plots.
			dataset <- dataM()
			ind.last = dim(dataset)[2]
			dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
			nrow.plot = length(dataset.split)		
			ncol.plot = 1							
			
			par(mfrow=c(nrow.plot, ncol.plot), mar=c(1,0.5,2,0))
			#par(mfrow=c(1, 3), mar=c(0,0.5,2,0))
			result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
			lapply(result, function(x)mvnPlot(x, type="persp"))
		}
		
		else if (input$plotType=='2'){	## Contour plots.
			dataset <- dataM()
			ind.last = dim(dataset)[2]
			dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
			nrow.plot = length(dataset.split)		
			ncol.plot = 1							
			
			par(mfrow=c(nrow.plot, ncol.plot))
			result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
			lapply(result, function(x)mvnPlot(x, type="contour"))
		}
	}
  }, height = heightSize, width = widthSize)

	
	## 1. EPS Format
	output$downloadPlotEPS <- downloadHandler(
		filename <- function() { paste('MVN.eps') },
		content <- function(file) {
			postscript(file, horizontal = FALSE, onefile = FALSE, paper = "special", width = input$myWidth/72, height = input$myHeight/72)
			
			if (input$firstLast == 0){   ## Download graphs for single groups.
			
				if(input$plotType=='0'){  ## Q-Q plots.
                
					if(input$testType=='0'){
						dataset <- dataM()
						mardiaTest(dataset, qqplot=TRUE)
					} 
				
					else if(input$testType=='1'){
						dataset <- dataM()
						hzTest(dataset, qqplot=TRUE)
					} 
				
					else if(input$testType=='2'){
						dataset <- dataM()
						roystonTest(dataset, qqplot=TRUE)
					}
            
					else if(input$testType=='3'){  
						dataset <- dataM()
						dhTest(dataset, qqplot=TRUE)
					}
				}
			
				else if(input$plotType=='1'){  ## Perspective plots.
					dataset <- dataM()
					result <- mardiaTest(dataset)
					mvnPlot(result, type="persp")
				}
			
				else if(input$plotType=='2'){	## Contour plots.
					dataset <- dataM()
					result <- mardiaTest(dataset)
					mvnPlot(result, type="contour")
				}
			}
			
			else if (input$firstLast == 1){  ## Download graphs, first column is group variable.
			
				if (input$plotType=='0'){  ## Q-Q Plots.
              
					if (input$testType=='0'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)mardiaTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='1'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)hzTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='2'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)roystonTest(x, qqplot=TRUE))
						
					}
              
					else if(input$testType=='3'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
						
					}
				}
		
				if (input$plotType=='1'){  ## Perspective Plots.
					dataset <- dataM()
					dataset.split = split(dataset[,-1], dataset[,1])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot), mar=c(1,0.5,2,0))
					#par(mfrow=c(1, 3), mar=c(0,0.5,2,0))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="persp"))
				}
		
				else if (input$plotType=='2'){	## Contour plots.
					dataset <- dataM()
					dataset.split = split(dataset[,-1], dataset[,1])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="contour"))
				}
			}
			
			else if (input$firstLast == 2){  ## Download graphs, last column is group variable.
			
				if (input$plotType=='0'){  ## Q-Q Plots.
              
					if (input$testType=='0'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)mardiaTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='1'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)hzTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='2'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)roystonTest(x, qqplot=TRUE))
						
					}
              
					else if(input$testType=='3'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
						
					}
				}
		
				if (input$plotType=='1'){  ## Perspective Plots.
					dataset <- dataM()
					ind.last = dim(dataset)[2]
					dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot), mar=c(1,0.5,2,0))
					#par(mfrow=c(1, 3), mar=c(0,0.5,2,0))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="persp"))
				}
		
				else if (input$plotType=='2'){	## Contour plots.
					dataset <- dataM()
					ind.last = dim(dataset)[2]
					dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="contour"))
				}
			}
			
			dev.off()
		},
		contentType = 'application/postscript'
	)

	# 2. PDF Format
	output$downloadPlotPDF <- downloadHandler(
		filename <- function() { paste('MVN.pdf') },
		content <- function(file) {
			pdf(file, width = input$myWidth/72, height = input$myHeight/72)

            if (input$firstLast == 0){   ## Download graphs for single groups.
			
				if(input$plotType=='0'){  ## Q-Q plots.
                
					if(input$testType=='0'){
						dataset <- dataM()
						mardiaTest(dataset, qqplot=TRUE)
					} 
				
					else if(input$testType=='1'){
						dataset <- dataM()
						hzTest(dataset, qqplot=TRUE)
					} 
				
					else if(input$testType=='2'){
						dataset <- dataM()
						roystonTest(dataset, qqplot=TRUE)
					}
            
					else if(input$testType=='3'){  
						dataset <- dataM()
						dhTest(dataset, qqplot=TRUE)
					}
				}
			
				else if(input$plotType=='1'){  ## Perspective plots.
					dataset <- dataM()
					result <- mardiaTest(dataset)
					mvnPlot(result, type="persp")
				}
			
				else if(input$plotType=='2'){	## Contour plots.
					dataset <- dataM()
					result <- mardiaTest(dataset)
					mvnPlot(result, type="contour")
				}
			}
			
			else if (input$firstLast == 1){  ## Download graphs, first column is group variable.
			
				if (input$plotType=='0'){  ## Q-Q Plots.
              
					if (input$testType=='0'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)mardiaTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='1'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)hzTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='2'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)roystonTest(x, qqplot=TRUE))
						
					}
              
					else if(input$testType=='3'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
						
					}
				}
		
				if (input$plotType=='1'){  ## Perspective Plots.
					dataset <- dataM()
					dataset.split = split(dataset[,-1], dataset[,1])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot), mar=c(1,0.5,2,0))
					#par(mfrow=c(1, 3), mar=c(0,0.5,2,0))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="persp"))
				}
		
				else if (input$plotType=='2'){	## Contour plots.
					dataset <- dataM()
					dataset.split = split(dataset[,-1], dataset[,1])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="contour"))
				}
			}
			
			else if (input$firstLast == 2){  ## Download graphs, last column is group variable.
			
				if (input$plotType=='0'){  ## Q-Q Plots.
              
					if (input$testType=='0'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)mardiaTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='1'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)hzTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='2'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)roystonTest(x, qqplot=TRUE))
						
					}
              
					else if(input$testType=='3'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
						
					}
				}
		
				if (input$plotType=='1'){  ## Perspective Plots.
					dataset <- dataM()
					ind.last = dim(dataset)[2]
					dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot), mar=c(1,0.5,2,0))
					#par(mfrow=c(1, 3), mar=c(0,0.5,2,0))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="persp"))
				}
		
				else if (input$plotType=='2'){	## Contour plots.
					dataset <- dataM()
					ind.last = dim(dataset)[2]
					dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="contour"))
				}
			}

			dev.off()
		},
		contentType = 'application/pdf'
	)

	# 3. PNG format
	output$downloadPlotPNG <- downloadHandler(
		filename <- function() { paste('MVN.png') },
		content <- function(file) {
			png(file, width = input$myWidth, height = input$myHeight)
			
            if (input$firstLast == 0){   ## Download graphs for single groups.
			
				if(input$plotType=='0'){  ## Q-Q plots.
                
					if(input$testType=='0'){
						dataset <- dataM()
						mardiaTest(dataset, qqplot=TRUE)
					} 
				
					else if(input$testType=='1'){
						dataset <- dataM()
						hzTest(dataset, qqplot=TRUE)
					} 
				
					else if(input$testType=='2'){
						dataset <- dataM()
						roystonTest(dataset, qqplot=TRUE)
					}
            
					else if(input$testType=='3'){  
						dataset <- dataM()
						dhTest(dataset, qqplot=TRUE)
					}
				}
			
				else if(input$plotType=='1'){  ## Perspective plots.
					dataset <- dataM()
					result <- mardiaTest(dataset)
					mvnPlot(result, type="persp")
				}
			
				else if(input$plotType=='2'){	## Contour plots.
					dataset <- dataM()
					result <- mardiaTest(dataset)
					mvnPlot(result, type="contour")
				}
			}
			
			else if (input$firstLast == 1){  ## Download graphs, first column is group variable.
			
				if (input$plotType=='0'){  ## Q-Q Plots.
              
					if (input$testType=='0'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)mardiaTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='1'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)hzTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='2'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)roystonTest(x, qqplot=TRUE))
						
					}
              
					else if(input$testType=='3'){
						dataset <- dataM()
						dataset.split = split(dataset[,-1], dataset[,1])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
						
					}
				}
		
				if (input$plotType=='1'){  ## Perspective Plots.
					dataset <- dataM()
					dataset.split = split(dataset[,-1], dataset[,1])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot), mar=c(1,0.5,2,0))
					#par(mfrow=c(1, 3), mar=c(0,0.5,2,0))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="persp"))
				}
		
				else if (input$plotType=='2'){	## Contour plots.
					dataset <- dataM()
					dataset.split = split(dataset[,-1], dataset[,1])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="contour"))
				}
			}
			
			else if (input$firstLast == 2){  ## Download graphs, last column is group variable.
			
				if (input$plotType=='0'){  ## Q-Q Plots.
              
					if (input$testType=='0'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)mardiaTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='1'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)hzTest(x, qqplot=TRUE))
						
					} 
			
					else if(input$testType=='2'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)roystonTest(x, qqplot=TRUE))
						
					}
              
					else if(input$testType=='3'){
						dataset <- dataM()
						ind.last = dim(dataset)[2]
						dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
						nrow.plot = length(dataset.split)		
						ncol.plot = 1							
				
						par(mfrow=c(nrow.plot, ncol.plot))
						lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
						
					}
				}
		
				if (input$plotType=='1'){  ## Perspective Plots.
					dataset <- dataM()
					ind.last = dim(dataset)[2]
					dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot), mar=c(1,0.5,2,0))
					#par(mfrow=c(1, 3), mar=c(0,0.5,2,0))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="persp"))
				}
		
				else if (input$plotType=='2'){	## Contour plots.
					dataset <- dataM()
					ind.last = dim(dataset)[2]
					dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
					nrow.plot = length(dataset.split)		
					ncol.plot = 1							
			
					par(mfrow=c(nrow.plot, ncol.plot))
					result <- lapply(dataset.split, function(x)hzTest(x, qqplot=FALSE))
					lapply(result, function(x)mvnPlot(x, type="contour"))
				}
			}
			
			dev.off()
		},
		contentType = 'image/png'
	)
    
    
    ## Plot area for outlier detection tab.
    output$outlierPLOT <- renderPlot({
   
   if (input$firstLast==0){   # Grup degiskeni yok
       
        if(input$outlierDetect=='1'){
            dataset <- dataM()
            outlier(dataset, method="quan")
        }
    
        if(input$outlierDetect=='2'){
            dataset <- dataM()
            outlier(dataset, method="adj.quan")
        }
        
        if(input$outlierDetect=='3'){
            dataset <- dataM()
            outlier(dataset, method="pcout")
        }
        
    }
   
   if (input$firstLast == 1){  ## First column is "Group" variable.
       
       
       if(input$outlierDetect=='1'){
           dataset <- dataM()
           dataset.split = split(dataset[,-1], dataset[,1])
           nrow.plot = length(dataset.split)		
           ncol.plot = 1							
           par(mfrow=c(nrow.plot, ncol.plot))
           lapply(dataset.split, function(x)outlier(x, method="quan"))
           
       }
       
       if(input$outlierDetect=='2'){
           dataset <- dataM()
           dataset.split = split(dataset[,-1], dataset[,1])
           nrow.plot = length(dataset.split)		
           ncol.plot = 1							
           par(mfrow=c(nrow.plot, ncol.plot))
           lapply(dataset.split, function(x)outlier(x, method="adj.quan"))
           
       }
       
       if(input$outlierDetect=='3'){
           dataset <- dataM()
           dataset.split = split(dataset[,-1], dataset[,1])
           nrow.plot = length(dataset.split)		
           ncol.plot = 1							
           par(mfrow=c(nrow.plot, ncol.plot))
           lapply(dataset.split, function(x)outlier(x, method="pcout"))
           
       }
       
   }


  if(input$firstLast == 2){	## Last column is "Group" variable.
      
      if(input$outlierDetect=='1'){
          dataset <- dataM()
          ind.last = dim(dataset)[2]
          dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
          nrow.plot = length(dataset.split)		
          ncol.plot = 1							
          
          par(mfrow=c(nrow.plot, ncol.plot))
          lapply(dataset.split, function(x)outlier(x, method="quan"))
          
      }
      
      if(input$outlierDetect=='2'){
          dataset <- dataM()
          ind.last = dim(dataset)[2]
          dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
          nrow.plot = length(dataset.split)		
          ncol.plot = 1							
          
          par(mfrow=c(nrow.plot, ncol.plot))
          lapply(dataset.split, function(x)outlier(x, method="adj.quan"))
          
      }
      
      if(input$outlierDetect=='3'){
          dataset <- dataM()
          ind.last = dim(dataset)[2]
          dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
          nrow.plot = length(dataset.split)		
          ncol.plot = 1							
          
          par(mfrow=c(nrow.plot, ncol.plot))
          lapply(dataset.split, function(x)outlier(x, method="pcout"))
          
          
        }
      
      }
   }, height = heightsize, width = widthsize)


    output$downloadMVNData <- downloadHandler(
    	filename = function() { "data.txt" },
   		content = function(file) {
		write.table(dataM(), file, row.names=FALSE, quote=FALSE, sep="\t")
    })
        
        
    output$downloadOutlier <- downloadHandler(
    	filename = function() { "outlier.txt" },
   		content = function(file) {
 
       if (input$firstLast == 0){   ## Download graphs for single groups.
       if(input$outlierDetect=='1'){
           dataset <- dataM()
           write.table(outlier(dataset, qqplot=FALSE, method="quan")$outlier, file, row.names=FALSE,
					   col.names=TRUE, quote=FALSE, sep="\t")
           
       }else
       
       if(input$outlierDetect=='2'){
           dataset <- dataM()
           write.table(outlier(dataset, qqplot=FALSE, method="adj.quan")$outlier, file, row.names=FALSE,
					   col.names=TRUE,  quote=FALSE, sep="\t")
           
       }else
       
       if(input$outlierDetect=='3'){
           dataset <- dataM()
           write.table(outlier(dataset, qqplot=FALSE, method="pcout")$outlier, file, row.names=FALSE,
					   col.names=TRUE, quote=FALSE, sep="\t")
       }
       
       }
       
       else if (input$firstLast == 1){  ## Download graphs, first column is group variable.
       
       if(input$outlierDetect=='1'){
           dataset <- dataM()
           dataset.split = split(dataset[,-1], dataset[,1])
           quantile = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="quan"))
           outlier = list()
           for(i in 1:length(dataset.split)){
               
               outlier[i] = quantile[[i]][1]
           }
           names(outlier) = names(quantile)
           o = ldply(outlier, rbind)
           names(o)[1]="Group"
           
           
           write.table(o, file, row.names=FALSE,
           col.names=TRUE, quote=FALSE, sep="\t")
           
       }else
       
       if(input$outlierDetect=='2'){
           dataset <- dataM()
           dataset.split = split(dataset[,-1], dataset[,1])
           adjquan = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="adj.quan"))
           outlier = list()
           for(i in 1:length(dataset.split)){
               
               outlier[i] = adjquan[[i]][1]
           }
           names(outlier) = names(adjquan)
           o = ldply(outlier, rbind)
           names(o)[1]="Group"
           
           
           write.table(o, file, row.names=FALSE,
           col.names=TRUE, quote=FALSE, sep="\t")
           
       }else
       
       if(input$outlierDetect=='3'){
           dataset <- dataM()
           dataset.split = split(dataset[,-1], dataset[,1])
           out = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
           outlier = list()
           for(i in 1:length(dataset.split)){
               
               outlier[i] = out[[i]][1]
           }
           names(outlier) = names(out)
           o = ldply(outlier, rbind)
           names(o)[1]="Group"
           
           
           write.table(o, file, row.names=FALSE,
           col.names=TRUE, quote=FALSE, sep="\t")
       }
       
       
       }

else if (input$firstLast == 2){  ## outlier download, last column is group variable.
    
    if(input$outlierDetect=='1'){
        dataset <- dataM()
        ind.last = dim(dataset)[2]
        dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
        quantile = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="quan"))
        outlier = list()
        for(i in 1:length(dataset.split)){
            
            outlier[i] = quantile[[i]][1]
        }
        names(outlier) = names(quantile)
        o = ldply(outlier, rbind)
        names(o)[1]="Group"
        
        
        write.table(o, file, row.names=FALSE,
        col.names=TRUE, quote=FALSE, sep="\t")
        
    }else
    
    if(input$outlierDetect=='2'){
        dataset <- dataM()
        ind.last = dim(dataset)[2]
        dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
        adjquan = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="adj.quan"))
        outlier = list()
        for(i in 1:length(dataset.split)){
            
            outlier[i] = adjquan[[i]][1]
        }
        names(outlier) = names(adjquan)
        o = ldply(outlier, rbind)
        names(o)[1]="Group"
        
        
        write.table(o, file, row.names=FALSE,
        col.names=TRUE, quote=FALSE, sep="\t")
        
    }else
    
    if(input$outlierDetect=='3'){
        dataset <- dataM()
        ind.last = dim(dataset)[2]
        dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
        out = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
        outlier = list()
        for(i in 1:length(dataset.split)){
            
            outlier[i] = out[[i]][1]
        }
        names(outlier) = names(out)
        o = ldply(outlier, rbind)
        names(o)[1]="Group"
        
        
        write.table(o, file, row.names=FALSE,
        col.names=TRUE, quote=FALSE, sep="\t")
    }
    
    
  }



})
        
        output$downloadNewData <- downloadHandler(
    	filename = function() { "newData.txt" },
   		content = function(file) {
            
            if (input$firstLast == 0){   ## Download graphs for single groups.
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    write.table(outlier(dataset, qqplot=FALSE, method="quan")$outlier, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    write.table(outlier(dataset, qqplot=FALSE, method="adj.quan")$outlier, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    write.table(outlier(dataset, qqplot=FALSE, method="pcout")$outlier, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                }
                
            }
            
            else if (input$firstLast == 1){  ## Download graphs, first column is group variable.
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    quantile = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="quan"))
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = quantile[[i]][2]
                    }
                    names(outlier) = names(quantile)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    adjquan = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="adj.quan"))
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = adjquan[[i]][2]
                    }
                    names(outlier) = names(adjquan)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    out = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = out[[i]][2]
                    }
                    names(outlier) = names(out)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                }
                
                
            }
            
            else if (input$firstLast == 2){  ## outlier download, last column is group variable.
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    quantile = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="quan"))
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = quantile[[i]][2]
                    }
                    names(outlier) = names(quantile)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    adjquan = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="adj.quan"))
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = adjquan[[i]][2]
                    }
                    names(outlier) = names(adjquan)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    out = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = out[[i]][2]
                    }
                    names(outlier) = names(out)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                }
                
                
            }
            
            
        })
        
    output$downloadOutlierPlot <- downloadHandler(
            filename <- function() { paste('outlier.pdf') },
            content <- function(file) {
			pdf(file, width = input$mywidth/72, height = input$myheight/72)
            
            if (input$firstLast==0){   # Grup degiskeni yok
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    outlier(dataset, method="quan")
                }
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    outlier(dataset, method="adj.quan")
                }
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    outlier(dataset, method="pcout")
                }
                
            }
            
            if (input$firstLast == 1){  ## First column is "Group" variable.
                
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    nrow.plot = length(dataset.split)		
                    ncol.plot = 1							
                    par(mfrow=c(nrow.plot, ncol.plot))
                    lapply(dataset.split, function(x)outlier(x, method="quan"))
                    
                }
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    nrow.plot = length(dataset.split)		
                    ncol.plot = 1							
                    par(mfrow=c(nrow.plot, ncol.plot))
                    lapply(dataset.split, function(x)outlier(x, method="adj.quan"))
                    
                }
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    nrow.plot = length(dataset.split)		
                    ncol.plot = 1							
                    par(mfrow=c(nrow.plot, ncol.plot))
                    lapply(dataset.split, function(x)outlier(x, method="pcout"))
                    
                }
                
            }
            
            
            if(input$firstLast == 2){	## Last column is "Group" variable.
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    nrow.plot = length(dataset.split)		
                    ncol.plot = 1							
                    
                    par(mfrow=c(nrow.plot, ncol.plot))
                    lapply(dataset.split, function(x)outlier(x, method="quan"))
                    
                }
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    nrow.plot = length(dataset.split)		
                    ncol.plot = 1							
                    
                    par(mfrow=c(nrow.plot, ncol.plot))
                    lapply(dataset.split, function(x)outlier(x, method="adj.quan"))
                    
                }
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    nrow.plot = length(dataset.split)		
                    ncol.plot = 1							
                    
                    par(mfrow=c(nrow.plot, ncol.plot))
                    lapply(dataset.split, function(x)outlier(x, method="pcout"))
                    
                    
                }
                
            }
         
			dev.off()
		},
		contentType = 'application/pdf'
        )
        
        
        output$downloadMVNresult <- downloadHandler(
		filename <- function() { paste("result.txt") },
		content <- function(file) {
            
             if (input$firstLast == 0){   ## Download graphs for single groups.
                 
                if(input$testType=='0'){
                    
                    result <- mardiaTest(dataM())
                    out <- capture.output(result)
   
                } else
                
                if(input$testType=='1'){
    
                    result <- hzTest(dataM())
                      out<-capture.output(result)
                    
                } else
                
                if(input$testType=='2'){
                  
                     result <- roystonTest(dataM())
                       out <- capture.output(result)
                    
                } else
                
                if(input$testType=='3'){
                   
                result <- dhTest(dataM())
                out <- capture.output(result)
                    
                }
                
                	write.table(out, file, row.names=F, col.names=F, quote=F)
             }
             
             else if (input$firstLast == 1){  ## Download graphs, first column is group variable.
                 
                 
                 if(input$testType=='0'){
                     
                     dataset <- dataM()
                     dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                     result = lapply(dataset.split, function(x)mardiaTest(x,qqplot=FALSE))
                     out <- capture.output(result)
                     
                 } else
                 
                 if(input$testType=='1'){
                     
                  dataset <- dataM()
                  dataset.split = split(dataset[,-1], dataset[,1])
                  result = lapply(dataset.split, function(x)hzTest(x,qqplot=FALSE))
                  out <- capture.output(result)
                     
                 } else
                 
                 if(input$testType=='2'){
                     
                  dataset <- dataM()
                  dataset.split = split(dataset[,-1], dataset[,1])
                  result = lapply(dataset.split, function(x)roystonTest(x,qqplot=FALSE))
                  out <- capture.output(result)
                     
                 } else
                 
                 if(input$testType=='3'){
                     
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    result = lapply(dataset.split, function(x)dhTest(x,qqplot=FALSE))
                    out <- capture.output(result)
                     
                 }
                 
                 write.table(out, file, row.names=F, col.names=F, quote=F)
                 
                 
             }
             
             else if(input$firstLast == 2){  ## Last column is "Group" variable.
                 
                 if(input$testType=='0'){
                     dataset <- dataM()
                     ind.last = dim(dataset)[2]
                     dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                     result = lapply(dataset.split, function(x)mardiaTest(x,qqplot=FALSE))
                     out <- capture.output(result)
                     
                 }
                 
                 else if(input$testType=='1'){
                     dataset <- dataM()
                     ind.last = dim(dataset)[2]
                     dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                     result = lapply(dataset.split, function(x)hzTest(x,qqplot=FALSE))
                     out <- capture.output(result)
                    
                 }
                 
                 else if(input$testType=='2'){
                     dataset <- dataM()
                     ind.last = dim(dataset)[2]
                     dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                     result = lapply(dataset.split, function(x)roystonTest(x,qqplot=FALSE))
                     out <- capture.output(result)
                     
                 }
                 
                 else if(input$testType=='3'){
                     dataset <- dataM()
                     ind.last = dim(dataset)[2]
                     dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                     result = lapply(dataset.split, function(x)dhTest(x,qqplot=FALSE))
                     out <- capture.output(result)
                     
                 }
                 
                 write.table(out, file, row.names=F, col.names=F, quote=F)
             }
		})
        ###
        
        # display 10 rows initially
        output$RawData <- renderDataTable(dataM(), options = list(iDisplayLength = 10))
        
        # display 10 rows initially
        output$OutlierData <- renderDataTable(
        
        
        
        
        if(input$firstLast==0){   ## Single group outlier detection
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                outlier(dataset, qqplot=FALSE, method="quan")$outlier
                
            }else
            
            if(input$outlierDetect=='2'){
                dataset <- dataM()
                outlier(dataset, qqplot=FALSE, method="adj.quan")$outlier
                
            }else
            
            if(input$outlierDetect=='3'){
                dataset <- dataM()
                o = outlier(dataset, qqplot=FALSE, method="pcout")$outlier
                names(o)= c("Observation", "Outlier")
                o
                
            }
            
            
        }
        
        else if (input$firstLast == 1){  ## First column is "Group" variable.
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                quantile = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="quan"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = quantile[[i]][1]
                }
                names(outlier) = names(quantile)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='2'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])
                adjquan = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="adj.quan"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = adjquan[[i]][1]
                }
                names(outlier) = names(adjquan)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='3'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])
                out =lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = out[[i]][1]
                }
                names(outlier) = names(out)
                o = ldply(outlier, rbind)
                names(o) = c("Group", "Observation", "Outlier")
                o
            }
            
            
        }
        
        else if(input$firstLast == 2){  ## Last column is "Group" variable.
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                quantile = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="quan"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = quantile[[i]][1]
                }
                names(outlier) = names(quantile)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='2'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                adjquan = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="adj.quan"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = adjquan[[i]][1]
                }
                names(outlier) = names(adjquan)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
                
            }
            
            else if(input$outlierDetect=='3'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                out = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = out[[i]][1]
                }
                names(outlier) = names(out)
                o = ldply(outlier, rbind)
                names(o) = c("Group", "Observation", "Outlier")
                o
            }
            
            
        },
			
        options = list(iDisplayLength = 10))
        
        # display 10 rows initially
        output$NewData <- renderDataTable(
        
        
        
        if(input$firstLast==0){   ## Single group outlier detection
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                outlier(dataset, qqplot=FALSE, method="quan")$newData
                
            }else
            
            if(input$outlierDetect=='2'){
                dataset <- dataM()
                outlier(dataset, qqplot=FALSE, method="adj.quan")$newData
                
            }else
            
            if(input$outlierDetect=='3'){
                dataset <- dataM()
                outlier(dataset, qqplot=FALSE, method="pcout")$newData
                
            }
            
            
        }
        
        else if (input$firstLast == 1){  ## First column is "Group" variable.
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                quantile = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="quan"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = quantile[[i]][2]
                }
                names(outlier) = names(quantile)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='2'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])
                adjquan = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="adj.quan"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = adjquan[[i]][2]
                }
                names(outlier) = names(adjquan)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='3'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])
                out =lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = out[[i]][2]
                }
                names(outlier) = names(out)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            
        }
        
        else if(input$firstLast == 2){  ## Last column is "Group" variable.
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                quantile = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="quan"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = quantile[[i]][2]
                }
                names(outlier) = names(quantile)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='2'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                adjquan = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="adj.quan"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = adjquan[[i]][2]
                }
                names(outlier) = names(adjquan)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
                
            }
            
            else if(input$outlierDetect=='3'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                out = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = out[[i]][2]
                }
                names(outlier) = names(out)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            
        },
			
        options = list(iDisplayLength = 10))
        
 
}

)





