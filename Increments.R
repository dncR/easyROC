	yIncrement21 <- reactive({
		if((input$valueStatus != "") && (!is.null(input$markerInput)) && input$showPlots){
			data = dataM()

			diseased = data[data[,input$statusVar] == input$valueStatus, input$cutoffMarker]
			healthy = data[data[,input$statusVar] != input$valueStatus, input$cutoffMarker]
			dens.diseased = density(diseased)
			dens.healthy = density(healthy)
			
			xmin = min(healthy, diseased)
			xmax = max(healthy, diseased)
			ymax = max(dens.diseased$y, dens.healthy$y)
			
			inc = round(0.02*ymax, 2)
			return(inc)
		}
		
		else return(0.1)
    })
    
    xIncrement21 <- reactive({
		if(input$valueStatus != "" && !is.null(input$markerInput) && input$showPlots){
			data = dataM()
			diseased = data[data[,input$statusVar] == input$valueStatus, input$cutoffMarker]
			healthy = data[data[,input$statusVar] != input$valueStatus, input$cutoffMarker]

			xmin = min(healthy, diseased)
			xmax = max(healthy, diseased)

			inc = round(0.02*abs(xmax-xmin), 2)
			return(inc)
		}
		
		else return(0.1)
    })
    
    xIncrement12 <- reactive({
		if (input$valueStatus != "" && !is.null(input$markerInput) && input$showPlots){
			results <- mROC(data=dataM(), statusName=input$statusVar, markerName=input$markerInput, event=input$valueStatus, diseaseHigher=input$lowhigh)$plotdata
			data = dataM()
			
			coord = results[results[,"Marker"] == input$cutoffMarker,]
			cutvals = coord[,"CutOff"]
			cutvals = cutvals[cutvals != Inf]
			cutvals = cutvals[cutvals != -Inf]
			
			inc = round(0.02*abs(max(cutvals) - min(cutvals)),2)
			return(inc)
		}
		
		else return(0.1)
	})


	observe({
		updateNumericInput(session, inputId = "legendXpos12", "x", value = 0, step = xIncrement12())
		updateNumericInput(session, inputId = "legendYpos21", "y", value = 0, step = yIncrement21())
		updateNumericInput(session, inputId = "legendXpos21", "x", value = 0, step = xIncrement21())
	})