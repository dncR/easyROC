> **NOTE**: The easyROC server located at Hacettepe University (Old URL: http://www.biosoft.hacettepe.edu.tr/easyROC) has moved to Erciyes University. The URL of new server is given below.

**URL for BioSoft (UPDATED)**: http://biosoft.erciyes.edu.tr/

**URL for easyROC (UPDATED)**: http://biosoft.erciyes.edu.tr/app/easyROC/

*Current Version*: 1.3.1

# easyROC: a web-tool for ROC curve analysis

This application designed as a comprehensive ROC analysis tool. Firstly, it provides fundamental ROC statistics, such as AUC and, its standard error, confidence interval and statistical significance. There are four different methods, including Mann-Whitney, DeLong-DeLong, Binomial and under null hypothesis, for computing standard error and confidence interval for an AUC. Users can select any of the methods in the application and the default method is Delong et al (1988). Secondly, one can compare the overall performances of multiple classifiers using three different multiple comparison methods, including Bonferroni, false discovery rate and ordinary Wald test. Moreover, this tool allows users to calculate pAUC of one or more classifiers, which is useful when one only interested in a specific part of the ROC curve instead of overall performance. Thirdly, one of the most important features of the ROC curve analysis is the identification of the optimal cut-off points especially for a diagnostic test in the medicine and, thanks to the OptimalCutpoints package of R, our tool has ability to identify optimal cut-off point for a certain classifier or diagnostic test using 34 different methods. More detailed information about these methods can be found in work of Lopez-Raton (2014). Fourthly, since sample size determination is one of the crucial steps in any statistical procedure, users can calculate required sample size for ROC curve analysis in under three different circumstances, such as in the case of single diagnostic test, two diagnostic tests and noninferiority of a new test to a standard test, which are introduced by Obuchowski (2005). Lastly, this tool offers a wide range of graphical tools for visual inspection of ROC curves, including ROC curves, sensitivity and specificity curves and distribution plots.

## References:

Goksuluk D, Korkmaz S, Zararsiz G, Karaagaoglu AE (2016). easyROC: An Interactive Web-tool for ROC Curve Analysis Using R Language Environment. R Journal, 8(2):213-230. doi: [10.32614/RJ-2016-042](https://doi.org/10.32614/RJ-2016-042)

DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing the areas under two or more correlated receiver operating characteristic curves: a nonparametric approach. Biometrics, 837-845.

Lopez-Raton, M., Rodrıguez-Alvarez, M. X., Cadarso-Suárez, C., & Gude-Sampedro, F. (2014). OptimalCutpoints: An R Package for Selecting Optimal Cutpoints in Diagnostic Tests. JOURNAL OF STATISTICAL SOFTWARE, 61(8):1-36.

Obuchowski, N. A. (2005). Fundamentals of clinical research for radiologists. Am. J. Roentgenol, 184:364-372.

# Installation

First, download and install R:

    https://cran.rstudio.com

Second, install following R packages:

    install.packages("devtools")
    require(devtools)
    install_version("shiny", version = "0.10.1", repos = "http://cran.us.r-project.org")
    install.packages("OptimalCutpoints")
    install.packages("plyr")
    install.packages("dplyr")
    install.packages("pROC")

Finally, run following code in R console:

    shiny::runGitHub("easyROC", "dncR")

## Docker Image

easyROC is available through Docker Container. The docker image can be pulled from Docker Hub using the code below:

    docker pull dncr/biosoft:app-easyroc-latest

# License

easyROC web-tool as a whole is distributed under GPL-3 (General Public License Version 3). For furter details, see LICENSE.txt
