#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2905510/
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3755824/
#https://www.thieme-connect.com/products/ejournals/pdf/10.1055/s-0041-1734019.pdf
#https://cran.r-project.org/web/packages/DTComPair/DTComPair.pdf
#https://www.indianpediatrics.net/apr2011/277.pdf
#https://www.mdpi.com/1660-4601/18/4/1492/pdf
#https://www.ajronline.org/doi/pdf/10.2214/ajr.184.2.01840364
#https://riffomonas.org/minimalR/
#https://rpubs.com/odenipinedo/multiple-and-logistic-regression-in-R
#https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
#https://www.tandfonline.com/doi/full/10.1080/24754269.2017.1319105
#https://www.jstor.org/stable/pdf/2347731.pdf?refreqid=excelsior%3Aa2c3864e22a09f47bc6f774c6a9b416d&ab_segments=&origin=
#http://biometry.github.io/APES/Stats/stats23-GeneralizedLinearModels-GLM.html
#https://cran.r-project.org/web/packages/LogisticDx/LogisticDx.pdf
#https://www.ajronline.org/doi/pdf/10.2214/ajr.184.2.01840364
#https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-12-77
#https://cran.r-project.org/web/packages/OptimalCutpoints/OptimalCutpoints.pdf
#https://cran.r-project.org/web/packages/ROCit/vignettes/my-vignette.html
#file:///home/lbcb/Downloads/v51i03.pdf
#https://cran.r-project.org/web/packages/pROC/pROC.pdf
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3755824/
#https://towardsdatascience.com/logistic-regression-model-fitting-and-finding-the-correlation-p-value-z-score-confidence-8330fb86db19
#https://cran.r-project.org/web/packages/cutpointr/vignettes/cutpointr.html


install.packages("fastDummies")
library(fastDummies)
install.packages("pROC")
install.packages("OptimalCutpoints")
install.packages("ThresholdROC")
#library(ThresholdROC)

#help(ThresholdROC)
install.packages("cutpointr")
library(OptimalCutpoints)
help(OptimalCutpoints)
library(pROC)
df<-Base_datos_744_v2
View(df)
data <- df
df
# Create dummy variable
#https://www.geeksforgeeks.org/dummy-variables-in-r-programming/
#https://hexcol.com/color/afca00
data <- dummy_cols(df,  select_columns = "CODIGO DEL PACIENTE")

# Print
View(data)
summary(rocobj )

rocobj <- plot.roc(data$`CODIGO DEL PACIENTE_NLA`, data$Albuminuria,
                   main = "Confidence intervals", 
                   percent=TRUE,
                   ci = TRUE,                  # compute AUC (of AUC by default)
                   print.auc = TRUE, col="#1c61b6")           # print the AUC (will contain the CI)
rocobj1 <- lines.roc(data$`CODIGO DEL PACIENTE_CTL`, data$CREATINURIA,
                   main = "Confidence intervals", 
                   percent=TRUE,
                   ci = TRUE,                  # compute AUC (of AUC by default)
                   print.auc = TRUE, col= "#9c000a")           # print the AUC (will contain the CI)

rocobj2 <- lines.roc(data$`CODIGO DEL PACIENTE_CTL`, data$ACR,
                    main = "Confidence intervals", 
                    percent=TRUE,
                    ci = TRUE,                  # compute AUC (of AUC by default)
                    print.auc = TRUE, col="#009c92")           # print the AUC (will contain the CI)

rocobj3 <- plot.roc(data$`CODIGO DEL PACIENTE_CTL`, data$NGAL,
                    main = "Confidence intervals", 
                    percent=TRUE,
                    ci = TRUE,                  # compute AUC (of AUC by default)
                    print.auc = TRUE, col="#afca00")           # print the AUC (will contain the CI)

rocobj4 <- lines.roc(data$`CODIGO DEL PACIENTE_CTL`, data$Clusterina,
																					main = "Confidence intervals", 
																					percent=TRUE,
																					ci = TRUE,                  # compute AUC (of AUC by default)
																					print.auc = TRUE, col="#003d33")         # print the AUC (will contain the CI)
rocobj5 <- lines.roc(data$`CODIGO DEL PACIENTE_CTL`, data$`Cistatina C`,
																					main = "Confidence intervals", 
																					percent=TRUE,
																					ci = TRUE,                  # compute AUC (of AUC by default)
																					print.auc = TRUE, col="#c43e00")           # print the AUC (will contain the CI)

rocobj6 <- lines.roc(data$`CODIGO DEL PACIENTE_CTL`, data$`KIM 1`,
																					main = "Confidence intervals", 
																					percent=TRUE,
																					ci = TRUE,                  # compute AUC (of AUC by default)
																					print.auc = TRUE, col="#76ff03")           # print the AUC (will contain the CI)
rocobj7 <- lines.roc(data$`CODIGO DEL PACIENTE_CTL`, data$Osteopontina,
																					main = "Confidence intervals", 
																					percent=TRUE,
																					ci = TRUE,                  # compute AUC (of AUC by default)
																					print.auc = TRUE, col="#ff3d00")           # print the AUC (will contain the CI)

ciobj <- ci.se(rocobj,                         # CI of sensitivity
               specificities = seq(0, 100, 5)) # over a select set of specificities
ciobj1 <- ci.se(rocobj1,                         # CI of sensitivity
               specificities = seq(0, 100, 5)) # over a select set of specificities

ciobj2 <- ci.se(rocobj2,                         # CI of sensitivity
                specificities = seq(0, 100, 5)) # over a select set of specificities

ciobj3 <- ci.se(rocobj3,                         # CI of sensitivity
                specificities = seq(0, 100, 5)) # over a select set of specificities

ciobj4 <- ci.se(rocobj4,                         # CI of sensitivity
															specificities = seq(0, 100, 5)) # over a select set of specificities
ciobj5 <- ci.se(rocobj5,                         # CI of sensitivity
																specificities = seq(0, 100, 5)) # over a select set of specificities

ciobj6 <- ci.se(rocobj6,                         # CI of sensitivity
																specificities = seq(0, 100, 5)) # over a select set of specificities

ciobj7 <- ci.se(rocobj7,                         # CI of sensitivity
																specificities = seq(0, 100, 5)) # over a select set of specificities

plot(ciobj, type = "shape", col = "#1c61b6AA")     # plot as a blue shape
plot(ci(rocobj, of = "thresholds", thresholds = "best")) # add one threshold

plot(ciobj1, type = "shape", col = "#008600")     # plot as a blue shape
plot(ci(rocobj1, of = "thresholds", thresholds = "best")) # add one threshold


plot(ciobj2, type = "shape", col = "#ffacac")     # plot as a blue shape
plot(ci(rocobj2, of = "thresholds", thresholds = "best")) # add one threshold

plot(ciobj3, type = "shape", col = "#f2ffff")     # plot as a blue shape
plot(ci(rocobj3, of = "thresholds", thresholds = "best")) # add one threshold

plot(ciobj4, type = "shape", col = "#8e99f3")     # plot as a blue shape
plot(ci(rocobj4, of = "thresholds", thresholds = "best")) # add one threshold

plot(ciobj5, type = "shape", col = "#bc5100")     # plot as a blue shape
plot(ci(rocobj5, of = "thresholds", thresholds = "best")) # add one threshold


plot(ciobj6, type = "shape", col = "#c4001d")     # plot as a blue shape
plot(ci(rocobj6, of = "thresholds", thresholds = "best")) # add one threshold

plot(ciobj7, type = "shape", col = "#006db3")     # plot as a blue shape
plot(ci(rocobj7, of = "thresholds", thresholds = "best")) # add one threshold


legend("bottomright", 
       legend = c("Clusterina= AUC:94.65%(91.7%-97.2%)", "CREATINURIA= AUC:52.4%(42.0%-62.1%)",  "ACR=AUC:94.8%(92.2%-97.5%)", "NGAL=AUC:79.1%(73.0%-85.2%)","ACR=AUC:94.8%(92.2%-97.5%)", "NGAL=AUC:79.1%(73.0%-85.2%)"), 
       col = c("#1c61b6", "#9c000a", "#009c92", "#afca00", "#003d33","#c43e00", "#76ff03", "#ff3d00"),
       lwd = 2, cex=0.6)

  

###########################################################
# Youden Index Method ("Youden"): Covariate gender
###########################################################
install.packages("ThresholdROC")


ci(rocobj, of = "thresholds", thresholds = "best")
ci(rocobj1, of = "thresholds", thresholds = "best")
ci(rocobj2, of = "thresholds", thresholds = "best")
ci(rocobj3, of = "thresholds", thresholds = "best")


coords(rocobj, "best")
coords(rocobj, x="best", input="threshold", best.method="youden") # punto de corte y  las cordenado segun el metodo youden
text(x = 97.6, y = 100, 
     labels = "x = 97.6,y = 100", col="#1c61b6")

coords(rocobj1, "best")
coords(rocobj1, x="best", input="threshold", best.method="youden") # punto de corte y  las cordenado segun el metodo youden
coords(rocobj2, "best")
coords(rocobj2, x="best", input="threshold", best.method="youden") # punto de corte y  las cordenado segun el metodo youden
coords(rocobj3, x="best", input="threshold", best.method="youden")
#######################################################
data2 <- subset(data,select=c(4,5,6,7,8,9,10,12,13,15,16,17,18,21,24,25,31,34))
View(data2)
train <- data2[1:160,]
test <- data2[161:225,]
