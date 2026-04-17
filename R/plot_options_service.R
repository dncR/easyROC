empty_to_null <- function(value) {
  if (is.null(value) || value == "") {
    return(NULL)
  }
  value
}

build_roc_plot_options_default <- function(marker_count, legend_names_input) {
  opts <- list()
  opts$fontfamilyRC <- "sans"

  opts$mainRC <- ""
  opts$font.mainRC <- NULL
  opts$cex.mainRC <- NULL
  opts$col.mainRC <- NULL

  opts$legend.namesRC <- strsplit(legend_names_input, split = ",")[[1]]

  opts$ROCcolRC <- seq_len(marker_count)
  opts$ROCltyRC <- 1
  opts$xlabRC <- "1-Specificity"
  opts$xfont.labRC <- 1
  opts$xcol.labRC <- "black"
  opts$xcex.labRC <- 1

  opts$xcol.axisRC <- "black"
  opts$xcex.axisRC <- 1

  opts$ylabRC <- "Sensitivity"
  opts$yfont.labRC <- 1
  opts$ycol.labRC <- "black"
  opts$ycex.labRC <- 1

  opts$ycol.axisRC <- "black"
  opts$ycex.axisRC <- 1

  opts
}

build_roc_plot_options_custom <- function(root_input) {
  opts <- list()
  opts$fontfamilyRC <- root_input$fontfamilyRC

  opts$mainRC <- root_input$mainRC
  opts$font.mainRC <- root_input$font.mainRC
  opts$cex.mainRC <- root_input$cex.mainRC
  opts$col.mainRC <- root_input$col.mainRC

  opts$legend.namesRC <- strsplit(root_input$legend.namesRC, split = ",")[[1]]

  opts$ROCcolRC <- trimws(strsplit(root_input$ROCcolRC, ",")[[1]], "both")
  opts$ROCltyRC <- root_input$ROCltyRC
  opts$xlabRC <- root_input$xlabRC
  opts$xfont.labRC <- root_input$xfont.labRC
  opts$xcol.labRC <- root_input$xcol.labRC
  opts$xcex.labRC <- root_input$xcex.labRC

  opts$xcol.axisRC <- root_input$xcol.axisRC
  opts$xcex.axisRC <- root_input$xcex.axisRC

  opts$ylabRC <- root_input$ylabRC
  opts$yfont.labRC <- root_input$yfont.labRC
  opts$ycol.labRC <- root_input$ycol.labRC
  opts$ycex.labRC <- root_input$ycex.labRC
  opts$ycol.axisRC <- root_input$ycol.axisRC
  opts$ycex.axisRC <- root_input$ycex.axisRC

  opts
}

build_cutoff_plot_options_default <- function(cutoff_marker) {
  opts <- list()
  opts$fontfamily <- "sans"

  opts$main11 <- "ROC Curve"
  opts$main12 <- "Sens. & Spec. Curves"
  opts$main21 <- paste("Distribution of ", cutoff_marker, sep = "")
  opts$main22 <- paste("Distribution of ", cutoff_marker, sep = "")

  opts$font.main11 <- opts$font.main12 <- opts$font.main21 <- opts$font.main22 <- 2
  opts$cex.main11 <- opts$cex.main12 <- opts$cex.main21 <- opts$cex.main22 <- 1.2
  opts$col.main11 <- opts$col.main12 <- opts$col.main21 <- opts$col.main22 <- "black"

  opts$ROCcol11 <- "black"
  opts$ROClty11 <- 1

  opts$sensCol <- "red"
  opts$specCol <- "blue"
  opts$sensType <- opts$specType <- 1

  opts$lineColD <- "red"
  opts$lineColH <- "blue"
  opts$lineTypeD <- opts$lineTypeH <- 1

  opts$xlab11 <- "1-Specificity"
  opts$xlab12 <- opts$xlab21 <- cutoff_marker
  opts$xlab22 <- "Disease Status"

  opts$xfont.lab11 <- opts$xfont.lab12 <- opts$xfont.lab21 <- opts$xfont.lab22 <- 1
  opts$xcol.lab11 <- opts$xcol.lab12 <- opts$xcol.lab21 <- opts$xcol.lab22 <- "black"

  opts$xcex.lab11 <- opts$xcex.lab12 <- opts$xcex.lab21 <- opts$xcex.lab22 <- 1
  opts$xcol.axis11 <- opts$xcol.axis12 <- opts$xcol.axis21 <- opts$xcol.axis22 <- "black"
  opts$xcex.axis11 <- opts$xcex.axis12 <- opts$xcex.axis21 <- opts$xcex.axis22 <- 1

  opts$ylab11 <- "Sensitivity"
  opts$ylab12 <- ""
  opts$ylab22 <- cutoff_marker
  opts$ylab21 <- "Density"

  opts$yfont.lab11 <- opts$yfont.lab12 <- opts$yfont.lab21 <- opts$yfont.lab22 <- 1
  opts$ycol.lab11 <- opts$ycol.lab12 <- opts$ycol.lab21 <- opts$ycol.lab22 <- "black"

  opts$ycex.lab11 <- opts$ycex.lab12 <- opts$ycex.lab21 <- opts$ycex.lab22 <- 1
  opts$ycol.axis11 <- opts$ycol.axis12 <- opts$ycol.axis21 <- opts$ycol.axis22 <- "black"
  opts$ycex.axis11 <- opts$ycex.axis12 <- opts$ycex.axis21 <- opts$ycex.axis22 <- 1

  opts$legendPos12 <- "topright"
  opts$legendXpos12 <- opts$legendYpos12 <- 1
  opts$legendNames12 <- "Sens.,Spec."
  opts$cex.legend12 <- 1
  opts$legendTitle12 <- NULL
  opts$font.legendTitle12 <- 2
  opts$col.legendTitle12 <- "black"
  opts$borderless12 <- FALSE

  opts$legendPos21 <- "topright"
  opts$legendXpos21 <- opts$legendYpos21 <- 1
  opts$legendNames21 <- "Diseased,Healthy"
  opts$cex.legend21 <- 1
  opts$legendTitle21 <- NULL
  opts$font.legendTitle21 <- 2
  opts$col.legendTitle21 <- "black"
  opts$borderless21 <- FALSE

  opts$colPoints <- "black,black"
  opts$pchFill <- "white,white"
  opts$xlabels22 <- "Healthy,Diseased"
  opts$pchPoints <- 1
  opts$pchSize <- 1
  opts$jitterAmount <- 0.05

  opts
}

build_cutoff_plot_options_custom <- function(root_input) {
  opts <- list()
  opts$fontfamily <- root_input$fontfamily
  opts$main11 <- root_input$main11
  opts$main12 <- root_input$main12
  opts$main21 <- root_input$main21
  opts$main22 <- root_input$main22

  opts$font.main11 <- as.numeric(root_input$font.main11)
  opts$font.main12 <- as.numeric(root_input$font.main12)
  opts$font.main21 <- as.numeric(root_input$font.main21)
  opts$font.main22 <- as.numeric(root_input$font.main22)

  opts$cex.main11 <- root_input$cex.main11
  opts$cex.main12 <- root_input$cex.main12
  opts$cex.main21 <- root_input$cex.main21
  opts$cex.main22 <- root_input$cex.main22

  opts$col.main11 <- root_input$col.main11
  opts$col.main12 <- root_input$col.main12
  opts$col.main21 <- root_input$col.main21
  opts$col.main22 <- root_input$col.main22

  opts$ROCcol11 <- root_input$ROCcol11
  opts$ROClty11 <- as.numeric(root_input$ROClty11)

  opts$sensCol <- root_input$sensCol
  opts$specCol <- root_input$specCol
  opts$sensType <- as.numeric(root_input$sensType)
  opts$specType <- as.numeric(root_input$specType)

  opts$lineColD <- root_input$lineColD
  opts$lineColH <- root_input$lineColH
  opts$lineTypeD <- as.numeric(root_input$lineTypeD)
  opts$lineTypeH <- as.numeric(root_input$lineTypeH)

  opts$xlab11 <- root_input$xlab11
  opts$xlab12 <- root_input$xlab12
  opts$xlab21 <- root_input$xlab21
  opts$xlab22 <- root_input$xlab22

  opts$xfont.lab11 <- as.numeric(root_input$xfont.lab11)
  opts$xfont.lab12 <- as.numeric(root_input$xfont.lab12)
  opts$xfont.lab21 <- as.numeric(root_input$xfont.lab21)
  opts$xfont.lab22 <- as.numeric(root_input$xfont.lab22)

  opts$xcol.lab11 <- root_input$xcol.lab11
  opts$xcol.lab12 <- root_input$xcol.lab12
  opts$xcol.lab21 <- root_input$xcol.lab21
  opts$xcol.lab22 <- root_input$xcol.lab22

  opts$xcex.lab11 <- root_input$xcex.lab11
  opts$xcex.lab12 <- root_input$xcex.lab12
  opts$xcex.lab21 <- root_input$xcex.lab21
  opts$xcex.lab22 <- root_input$xcex.lab22

  opts$xcol.axis11 <- root_input$xcol.axis11
  opts$xcol.axis12 <- root_input$xcol.axis12
  opts$xcol.axis21 <- root_input$xcol.axis21
  opts$xcol.axis22 <- root_input$xcol.axis22

  opts$xcex.axis11 <- root_input$xcex.axis11
  opts$xcex.axis12 <- root_input$xcex.axis12
  opts$xcex.axis21 <- root_input$xcex.axis21
  opts$xcex.axis22 <- root_input$xcex.axis22

  opts$ylab11 <- root_input$ylab11
  opts$ylab12 <- root_input$ylab12
  opts$ylab22 <- root_input$ylab22
  opts$ylab21 <- root_input$ylab21

  opts$yfont.lab11 <- as.numeric(root_input$yfont.lab11)
  opts$yfont.lab12 <- as.numeric(root_input$yfont.lab12)
  opts$yfont.lab21 <- as.numeric(root_input$yfont.lab21)
  opts$yfont.lab22 <- as.numeric(root_input$yfont.lab22)

  opts$ycol.lab11 <- root_input$ycol.lab11
  opts$ycol.lab12 <- root_input$ycol.lab12
  opts$ycol.lab21 <- root_input$ycol.lab21
  opts$ycol.lab22 <- root_input$ycol.lab22

  opts$ycex.lab11 <- root_input$ycex.lab11
  opts$ycex.lab12 <- root_input$ycex.lab12
  opts$ycex.lab21 <- root_input$ycex.lab21
  opts$ycex.lab22 <- root_input$ycex.lab22

  opts$ycol.axis11 <- root_input$ycol.axis11
  opts$ycol.axis12 <- root_input$ycol.axis12
  opts$ycol.axis21 <- root_input$ycol.axis21
  opts$ycol.axis22 <- root_input$ycol.axis22

  opts$ycex.axis11 <- root_input$ycex.axis11
  opts$ycex.axis12 <- root_input$ycex.axis12
  opts$ycex.axis21 <- root_input$ycex.axis21
  opts$ycex.axis22 <- root_input$ycex.axis22

  opts$legendPos12 <- root_input$legendPos12
  opts$legendXpos12 <- root_input$legendXpos12
  opts$legendYpos12 <- root_input$legendYpos12
  opts$legendNames12 <- root_input$legendNames12
  opts$cex.legend12 <- root_input$cex.legend12
  opts$borderless12 <- root_input$borderless12
  opts$legendTitle12 <- empty_to_null(root_input$legendTitle12)
  opts$font.legendTitle12 <- as.numeric(root_input$font.legendTitle12)
  opts$col.legendTitle12 <- root_input$col.legendTitle12

  opts$legendPos21 <- root_input$legendPos21
  opts$legendXpos21 <- root_input$legendXpos21
  opts$legendYpos21 <- root_input$legendYpos21
  opts$legendNames21 <- root_input$legendNames21
  opts$cex.legend21 <- root_input$cex.legend21
  opts$borderless21 <- root_input$borderless21
  opts$legendTitle21 <- empty_to_null(root_input$legendTitle21)
  opts$font.legendTitle21 <- as.numeric(root_input$font.legendTitle21)
  opts$col.legendTitle21 <- root_input$col.legendTitle21

  opts$colPoints <- root_input$colPoints
  opts$pchFill <- root_input$pchFill
  opts$xlabels22 <- root_input$xlabels22
  opts$pchPoints <- root_input$pchPoints
  opts$pchSize <- root_input$pchSize
  opts$jitterAmount <- root_input$jitterAmount

  opts
}
