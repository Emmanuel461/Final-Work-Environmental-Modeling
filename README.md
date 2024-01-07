# Final-Work-Environmental-Modeling

# Code final work module 3 of Enviromental Modeling
### Based on course materials. 

Adapted by Christoph Fischer and Jesús Céspedes Rivera 

## Boosted Regression Tree Model 
*The code can also be accesed by: https://posit.cloud/content/7299327*

```R
# Import data table
data <- read.table("/cloud/project/Dados_Trabalho_Mod3_peixes_2.csv", sep=";", header=T)

View(data)
names(data) 
str(data)

# This file contains the instructions and the detailed names of the variables.
# [Protocolo_Trabalho_Modulo3.pdf](https://fenix.isa.ulisboa.pt/downloadFile/281547991177656/Protocolo_Trabalho_Modulo3.pdf)

# Load packages
library(gbm)
library(dismo)

# Data
binary_data_text <- data

# Binary response
binary_data_text$Quality_Fish[data$Quality_Fish == "Poor"] <- 0
binary_data_text$Quality_Fish[data$Quality_Fish == "Bad"] <- 0
binary_data_text$Quality_Fish[data$Quality_Fish == "Moderate"] <- 0

binary_data_text$Quality_Fish[data$Quality_Fish == "Good"] <- 1
binary_data_text$Quality_Fish[data$Quality_Fish == "Very good"] <- 1

binary_data_text$Quality_Fish <- as.numeric(binary_data_text$Quality_Fish)           

hist(binary_data_text$Quality_Fish)

binary_data_text1 <- binary_data_text[,7:19]

# Run model
set.seed(24)

plot(binary_data_text1$env_Altitude, binary_data_text1$Forest_up,
     xlab = "Altitude (msnm)",
     ylab = "Forest Upstream (%)",
     pch = 16, col = "blue")

# THIS PRODUCES AN IMPORT PLOT (the number of trees)
binary_data_brt <- gbm.step(data=binary_data_text1, gbm.x=names(binary_data_text1[,-1]),
                          gbm.y="Quality_Fish", family="bernoulli", 
                          tree.complexity = 2, learning.rate=0.005, bag.fraction=0.5)

# Variable importance
binary_data_brt_imp <- summary(binary_data_brt)

# Customize font size in the plot
par(cex.axis=0.8, cex.lab=0.8, cex.main=0.8, cex.sub=0.8)

# THIS PLOT IS RELEVANT
barplot(binary_data_brt_imp$rel.inf, names.arg=rownames(binary_data_brt_imp), las=2, col="lightblue", main="Variables Importance", cex.main=1.2)

# Partial responses
plot.gbm(binary_data_brt, i.var = "env_Altitude") # response to elevation

# THIS PLOT PRODUCES A RELEVANT RESULT
# THE RESULTS SHOW that Altitude is the variable with more relevance in the model
help(gbm.plot)

# Adjust font size
par(cex.axis=1.4, cex.lab=1.4, cex.main=1.4, cex.sub=1.4)

# All variables
gbm.plot(binary_data_brt, smooth=T, write.title=FALSE, n.plot=6, 
         plot.layout=c(2,3))

help(gbm.interactions)

# Interactions between variables
binary_data_brt_imp_inter <- gbm.interactions(binary_data_brt)

# THIS PRODUCES AN IMPORTANT TABLE
binary_data_brt_imp_inter$rank.list # ordered list of potential interactions

# RESULTS
# Important relations Forest_up and Agric_up , Agric_up and env_Altitude.

# THIS PLOT PRODUCES A RELEVANT RESULT (SHOW THE MOST IMPORTANT INTERACTIONS)
# Visualize interactions
gbm.perspec(binary_data_brt, x=1,y=6, theta=130,phi=30, shade=0.9, border = NA, col="red", y.label = "Total Phosphorus (mg/l)", x.label = "Altitude", z.label = "Fitted value") 

# theta - horizontal rotation
# phi - vertical rotation
# see ?gbm.perspec for more options

# Quality of fit
# THIS PRODUCES AN EXCELLENT TABLE, 
# INTERPRETATION
# Important values: $correlation, best values are close to 1
# $null is the null-deviation and is compared with $resid, is null-deviation if higher than $resid is better
# $discrimination is 0.98, show high capacity to discriminate the variable categories
binary_data_brt$self.statistics # fit to training data
plot(predict(binary_data_brt, type="response"), binary_data_text$Quality_Fish, xlab="Modeled response", ylab="Observed response")

# THIS PRODUCES TO AN EXCELLENT TABLE (CROSS VALIDATION)
# Model validation
binary_data_brt$cv.statistics # cross-validation of the model

# $correlation.mean here shows a medium-high correlation between observed vs predicted
