## Clear workspace
rm(list=ls())
opar = par() # save original par() settings for future reference

## Set up workspace
# setwd("..."")

library(e1071)
library("ROCR")

hf = read.csv("by_diagnosis/heartFailure2009.txt", colClasses = "character")
hf = rbind(hf, read.csv("by_diagnosis/heartFailure2010.txt", colClasses = "character"))
hf = rbind(hf, read.csv("by_diagnosis/heartFailure2011.txt", colClasses = "character"))

## ------------------------- DATA PRE-PROCESSING ---------------------------------------------

# Add number of additional procedures as a column
source("getOtherProcCounts.R")

# Add number of additional diagnoses as a column
source("getODiagCounts.R")

# hf = hf[,c("proc_p","los","charge","agecat5","sex","race","patcnty","adm_src","pay_cat","dnr","oprocs","odiags","oshpd_id")]
hf = hf[,c("los","charge","agecat5","sex","race","adm_src","pay_cat","dnr","oprocs","odiags","oshpd_id")]

# Filter out missing/masked data
hf[hf ==""] <- NA
hf[hf =="*"] <- NA
hf = na.omit(hf)

# # Filter out records where nAddProcs != 0
# hf = hf[which(hf$nAddProcs==0),]
# hf = hf[,-ncol(hf)] # remove nAddProcs from variable list

# Add column for hospital size, remove hospital id
source("scaleHospitalSize.R")
colHospId = ncol(hf) - 1
hf = hf[,-colHospId]

# Convert data to factors or integers as needed
hf$agecat5 = as.ordered(hf$agecat5)
hf$sex = as.factor(hf$sex)
hf$race = as.factor(hf$race)
# hf$patcnty = as.factor(hf$patcnty)
# hf$adm_src = as.factor(hf$adm_src)
hf$adm_src = as.factor(as.integer(as.integer(hf$adm_src)/100))
hf$pay_cat = as.factor(hf$pay_cat) 
hf$dnr = as.factor(hf$dnr)
# hf$oshpd_id = as.factor(hf$oshpd_id)
hf = na.omit(hf)

# # Combine agecat5 and race into one variable: 1st digit = agecat5, 2nd digit = race
# agerace = as.factor(paste(hf$agecat5, hf$race, sep=""))
# hf = cbind(hf, agerace)

# For predicting charges, use data frame that removes records where:
# - total charges are $0 or $1
# - or LOS is > 365
hf.chargesDF = hf
hf.chargesDF$charge = as.integer(hf.chargesDF$charge)
hf.chargesDF$los = as.integer(hf.chargesDF$los)
hf.chargesDF = hf.chargesDF[-which(hf.chargesDF$charge==0 | hf.chargesDF$charge==1 | hf.chargesDF$los > 365),]

# Bin charges: $0-36,331 is level 0, above $36,331 is level 1
boxCharge = boxplot(hf.chargesDF$charge)
hf.chargesDF$charge = ifelse(hf.chargesDF$charge<boxCharge$stats[2,1], 0, 
                             ifelse(hf.chargesDF$charge<boxCharge$stats[3,1], 1, 
                                    ifelse(hf.chargesDF$charge<boxCharge$stats[4,1], 2, 3)))
hf.chargesDF$charge = as.ordered(hf.chargesDF$charge)
hf.chargesDF = hf.chargesDF[,-1] # remove LOS from this data frame

# Bin LOS: 2 or fewer days is level 0, over 2 days is level 1
hf.losDF = hf[,-2] # drop charge because we don't want it here
hf.losDF$los = as.integer(hf.losDF$los)
boxLOS = boxplot(hf.losDF$los)
hf.losDF$los = ifelse(hf.losDF$los<boxLOS$stats[2,1], 0, 
                             ifelse(hf.losDF$los<boxLOS$stats[3,1], 1, 
                                    ifelse(hf.losDF$los<boxLOS$stats[4,1], 2, 3)))
# hf.losDF$los = ifelse(hf.losDF$los>6, 7,hf.losDF$los)
hf.losDF$los = as.ordered(hf.losDF$los)

# Split into train and test:
set.seed(1234)
ind = sample(nrow(hf.chargesDF), .3*nrow(hf.chargesDF), replace=FALSE)
charges.train = hf.chargesDF[-ind,]
charges.test = hf.chargesDF[ind,]

set.seed(1234)
ind = sample(nrow(hf.losDF), .3*nrow(hf.losDF), replace=FALSE)
los.train = hf.losDF[-ind,]
los.test = hf.losDF[ind,]


# --------------------------------- BUILD AND RUN LOGISTIC REGRESSION MODEL ----------------------------------------
# ----- This section written by Radhika Sundar -----
library(ordinal)
# Charges ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Convert training and test data to model matrices agecat5+race+adm_src+pay_cat+dnr+oprocs+odiags+size+agerace
# x.charges = model.matrix(~-1+agecat5 + adm_src + pay_cat + dnr + odiags + oprocs + size, data=charges.train)
# x.charges = cbind(x.charges, charges.train$oprocs, charges.train$odiags, charges.train$size)
# y.charges = charges.train$charge
# testx.charges = model.matrix(~-1+agecat5 + adm_src + pay_cat + dnr + odiags + oprocs + size, data=charges.test)
# testx.charges = cbind(testx.charges, charges.test$oprocs, charges.test$odiags, charges.test$size)
# Build logistic regression model
# logReg.charges = glmnet(x.charges,y.charges,alpha=1,family = "binomial", standardize=FALSE)
logRegCV.charges = clm(charge ~ ., data=charges.train)
# s=fit1cv$lambda.min
# Run logistic regression model on test data
predictedCharges.logReg = predict(logRegCV.charges, charges.test[,-1], type="class")
charges.test = cbind(charges.test, predictedCharges.logReg)
conf.chargesLogReg = table(charges.test$fit, charges.test$charge)
# Error rate - Charges:
accCharges.LogReg = sum(diag(conf.chargesLogReg))/sum(conf.chargesLogReg)
errorRateCharges.LogReg = 1-accCharges.LogReg
# # Get probability class=1
# pCharge1.logReg = as.numeric(predict(logRegCV.charges, testx.charges, s="lambda.min", type="response"))
# # ROC Curve:
# predCharges.LogReg = prediction(pCharge1.logReg, charges.test[,1])
# perfCharges.LogReg = performance(predCharges.LogReg, "tpr", "fpr")
# # Caluclate area under ROC curve
# aurocCharges.LogReg = attr(performance(predCharges.LogReg, "auc"), "y.values")[[1]]
# # Lift chart:
# perfChargesLift.LogReg = performance(predCharges.LogReg, "lift", "rpp")

# LOS ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Convert training and test data to model matrices
# x.los = model.matrix(~-1+agecat5 + adm_src + pay_cat + dnr + odiags + oprocs + size, data=los.train)
# x.los = cbind(x.los, los.train$oprocs, los.train$odiags, los.train$size)
# y.los = los.train$los
# testx.los = model.matrix(~-1+agecat5 + adm_src + pay_cat + dnr + odiags + oprocs + size, data=los.test)
# testx.los = cbind(testx.los, los.test$oprocs, los.test$odiags, los.test$size)
# Build logistic regression model
# logReg.los = glmnet(x.los,y.los,alpha=1,family = "binomial", standardize=FALSE)
logRegCV.los = clm(los ~ ., data=los.train)
# s=fit1cv$lambda.min
# Run logistic regression model on test data
predictedLos.logReg = predict(logRegCV.los, los.test[,-1], type="class")
los.test = cbind(los.test, predictedLos.logReg)
conf.LosLogReg = table(los.test$fit, los.test$los)
# Error rate - LOS:
accLos.LogReg = sum(diag(conf.LosLogReg))/sum(conf.LosLogReg)
errorRateLos.LogReg = 1-accLos.LogReg
# Get probability class=1
pLos1.logReg = as.numeric(predict(logRegCV.los, testx.los, s="lambda.min", type="response"))
# ROC Curve:
predLos.LogReg = prediction(pLos1.logReg, los.test[,1])
perfLos.LogReg = performance(predLos.LogReg, "tpr", "fpr")
# Caluclate area under ROC curve
aurocLos.LogReg = attr(performance(predLos.LogReg, "auc"), "y.values")[[1]]
# Lift chart:
perfLosLift.LogReg = performance(predLos.LogReg, "lift", "rpp")

# --------------------------------- ROC and LIFT CURVES -------------------------------------------------------------
# ROC for Charges
par(opar)
par(lty=4, cex=1.8)
plot(perfCharges.NB, main='ROC curve for Charges - Heart Failure', xlab = "False Positive Rate", ylab="True Positive Rate", add = FALSE)
par(lty=2, col="red")
plot(perfCharges.LogReg, add=TRUE)
par(lty=3, col="blue")
plot(perfCharges.Any1Prob, add=TRUE)
par(lty=1, col="darkgreen")
plot(perfCharges.GBM, add=TRUE)
par(col="black")
legend(locator(1), legend=c("Naive Bayes", "Logistic Regression", "Ensemble", "GBM"), 
       lty=c(4,2,3,1), col=c("black","red", "blue", "darkgreen"))

# Lift for Charges
par(opar)
par(lty=4, cex=1.8)
plot(perfChargesLift.NB, main='Lift curve for Charges - Heart Failure', xlab="Rate of Positive Predictions", ylab="Lift", add = FALSE)
par(lty=2, col="red")
plot(perfChargesLift.LogReg, add=TRUE)
par(lty=3, col="blue")
plot(perfChargesLift.EnsMajProb, add=TRUE)
par(lty=1, col="darkgreen")
plot(perfChargesLift.GBM, add=TRUE)
par(col="black")
legend(locator(1), legend=c("Naive Bayes", "Logistic Regression", "Ensemble", "GBM"), 
       lty=c(4,2,3,1), col=c("black","red", "blue", "darkgreen"))

# ROC for LOS
par(opar)
par(lty=4, cex=1.8)
plot(perfLos.NB, main='ROC curve for LOS - Heart Failure', xlab = "False Positive Rate", ylab="True Positive Rate", add = FALSE)
par(lty=2, col="red")
plot(perfLos.LogReg, add=TRUE)
par(lty=3, col="blue")
plot(perfLos.EnsMajProb, add=TRUE)
par(lty=1, col="darkgreen")
plot(perfLos.GBM, add=TRUE)
par(col="black")
legend(locator(1), legend=c("Naive Bayes", "Logistic Regression", "Ensemble", "GBM"), 
       lty=c(4,2,3,1), col=c("black","red", "blue", "darkgreen"))

# Lift for LOS
par(opar)
par(lty=4, cex=1.8)
plot(perfLosLift.NB, main='Lift curve for LOS - Heart Failure', xlab="Rate of Positive Predictions", ylab="Lift", add = FALSE)
par(lty=2, col="red")
plot(perfLosLift.LogReg, add=TRUE)
par(lty=3, col="blue")
plot(perfLosLift.EnsMajProb, add=TRUE)
par(lty=1, col="darkgreen")
plot(perfLosLift.GBM, add=TRUE)
par(col="black")
legend(locator(1), legend=c("Naive Bayes", "Logistic Regression", "Ensemble", "GBM"), 
       lty=c(4,2,3,1), col=c("black","red", "blue", "darkgreen"))
