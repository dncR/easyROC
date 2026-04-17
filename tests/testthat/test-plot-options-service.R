test_that("build_roc_plot_options_default returns stable defaults", {
  opts <- build_roc_plot_options_default(
    marker_count = 3,
    legend_names_input = "A,B,C"
  )

  expect_equal(opts$fontfamilyRC, "sans")
  expect_equal(opts$ROCcolRC, 1:3)
  expect_equal(opts$ROCltyRC, 1)
  expect_equal(opts$xlabRC, "1-Specificity")
  expect_equal(opts$ylabRC, "Sensitivity")
  expect_equal(opts$legend.namesRC, c("A", "B", "C"))
})

test_that("build_roc_plot_options_custom parses custom values", {
  root_input <- list(
    fontfamilyRC = "serif",
    mainRC = "ROC",
    font.mainRC = "2",
    cex.mainRC = 1.3,
    col.mainRC = "blue",
    legend.namesRC = "M1,M2",
    ROCcolRC = "red, green",
    ROCltyRC = "2",
    xlabRC = "X",
    xfont.labRC = "1",
    xcol.labRC = "black",
    xcex.labRC = 0.9,
    xcol.axisRC = "gray",
    xcex.axisRC = 0.8,
    ylabRC = "Y",
    yfont.labRC = "3",
    ycol.labRC = "black",
    ycex.labRC = 1.1,
    ycol.axisRC = "gray",
    ycex.axisRC = 1.0
  )

  opts <- build_roc_plot_options_custom(root_input)

  expect_equal(opts$fontfamilyRC, "serif")
  expect_equal(opts$legend.namesRC, c("M1", "M2"))
  expect_equal(opts$ROCcolRC, c("red", "green"))
  expect_equal(opts$ROCltyRC, "2")
})

test_that("build_cutoff_plot_options_default injects selected marker into labels", {
  opts <- build_cutoff_plot_options_default(cutoff_marker = "markerX")

  expect_equal(opts$fontfamily, "sans")
  expect_equal(opts$xlab12, "markerX")
  expect_equal(opts$xlab21, "markerX")
  expect_equal(opts$ylab22, "markerX")
  expect_equal(opts$legendNames12, "Sens.,Spec.")
  expect_equal(opts$legendNames21, "Diseased,Healthy")
})

test_that("build_cutoff_plot_options_custom applies numeric conversion and empty-to-null", {
  root_input <- list(
    fontfamily = "mono",
    main11 = "a", main12 = "b", main21 = "c", main22 = "d",
    font.main11 = "2", font.main12 = "2", font.main21 = "2", font.main22 = "2",
    cex.main11 = 1, cex.main12 = 1, cex.main21 = 1, cex.main22 = 1,
    col.main11 = "black", col.main12 = "black", col.main21 = "black", col.main22 = "black",
    ROCcol11 = "red", ROClty11 = "1",
    sensCol = "red", specCol = "blue", sensType = "1", specType = "2",
    lineColD = "red", lineColH = "blue", lineTypeD = "1", lineTypeH = "2",
    xlab11 = "x1", xlab12 = "x2", xlab21 = "x3", xlab22 = "x4",
    xfont.lab11 = "1", xfont.lab12 = "1", xfont.lab21 = "1", xfont.lab22 = "1",
    xcol.lab11 = "black", xcol.lab12 = "black", xcol.lab21 = "black", xcol.lab22 = "black",
    xcex.lab11 = 1, xcex.lab12 = 1, xcex.lab21 = 1, xcex.lab22 = 1,
    xcol.axis11 = "black", xcol.axis12 = "black", xcol.axis21 = "black", xcol.axis22 = "black",
    xcex.axis11 = 1, xcex.axis12 = 1, xcex.axis21 = 1, xcex.axis22 = 1,
    ylab11 = "y1", ylab12 = "y2", ylab22 = "y3", ylab21 = "y4",
    yfont.lab11 = "1", yfont.lab12 = "1", yfont.lab21 = "1", yfont.lab22 = "1",
    ycol.lab11 = "black", ycol.lab12 = "black", ycol.lab21 = "black", ycol.lab22 = "black",
    ycex.lab11 = 1, ycex.lab12 = 1, ycex.lab21 = 1, ycex.lab22 = 1,
    ycol.axis11 = "black", ycol.axis12 = "black", ycol.axis21 = "black", ycol.axis22 = "black",
    ycex.axis11 = 1, ycex.axis12 = 1, ycex.axis21 = 1, ycex.axis22 = 1,
    legendPos12 = "topright", legendXpos12 = 1, legendYpos12 = 1,
    legendNames12 = "Sens,Spec", cex.legend12 = 1, borderless12 = FALSE,
    legendTitle12 = "", font.legendTitle12 = "2", col.legendTitle12 = "black",
    legendPos21 = "topright", legendXpos21 = 1, legendYpos21 = 1,
    legendNames21 = "D,H", cex.legend21 = 1, borderless21 = FALSE,
    legendTitle21 = "", font.legendTitle21 = "2", col.legendTitle21 = "black",
    colPoints = "black,black", pchFill = "white,white", xlabels22 = "Healthy,Diseased",
    pchPoints = 1, pchSize = 1, jitterAmount = 0.05
  )

  opts <- build_cutoff_plot_options_custom(root_input)

  expect_equal(opts$fontfamily, "mono")
  expect_equal(opts$font.main11, 2)
  expect_equal(opts$specType, 2)
  expect_null(opts$legendTitle12)
  expect_null(opts$legendTitle21)
})
