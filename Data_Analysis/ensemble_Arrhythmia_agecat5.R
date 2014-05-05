## Clear workspace
rm(list=ls())

## Set up workspace
# setwd("...")

library(e1071)
library("ROCR")

hf = read.csv("by_diagnosis/arrythmia2009.txt", colClasses = "character")
hf = rbind(hf, read.csv("by_diagnosis/arrythmia2010.txt", colClasses = "character"))
hf = rbind(hf, read.csv("by_diagnosis/arrythmia2011.txt", colClasses = "character"))

## ------------------------- DATA PRE-PROCESSING ---------------------------------------------

# Add number of additional procedures as a column
source("getOtherProcCounts.R")

# Add number of additional diagnoses as a column
source("getODiagCounts.R")

# Keep only records where proc_p = 3734 and agecat5=5
# hf = hf[,c("proc_p","los","charge","agecat5","sex","race","patcnty","adm_src","pay_cat","dnr","oprocs","odiags","oshpd_id")]
hf = hf[,c("proc_p","los","charge","agecat5","agecat20","race","adm_src","pay_cat","dnr","oprocs","odiags","oshpd_id")]
hf$proc_p = as.integer(hf$proc_p)
hf = hf[which(hf$proc_p==3734 & hf$agecat5==5),]
hf = hf[,-1] # remove proc_p from variable list

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
hf$agecat20 = as.ordered(hf$agecat20)
# hf$sex = as.factor(hf$sex)
hf$race = as.factor(hf$race)
# hf$patcnty = as.factor(hf$patcnty)
hf$adm_src = as.factor(hf$adm_src)
# hf$adm_src = as.factor(as.integer(as.integer(hf$adm_src)/100))
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
hf.chargesDF$charge = ifelse(hf.chargesDF$charge<mean(hf.chargesDF$charge), 0, 1)
hf.chargesDF$charge = as.factor(hf.chargesDF$charge)
hf.chargesDF = hf.chargesDF[,-1] # remove LOS from this data frame

# Bin LOS: 2 or fewer days is level 0, over 2 days is level 1
hf.losDF = hf[,-2] # drop charge because we don't want it here
hf.losDF$los = as.integer(hf.losDF$los)
hf.losDF$los = ifelse(hf.losDF$los<mean(hf.losDF$los), 0, 1)
hf.losDF$los = as.factor(hf.losDF$los)

# Split into train and test:
set.seed(1234)
ind = sample(nrow(hf.chargesDF), .3*nrow(hf.chargesDF), replace=FALSE)
charges.train = hf.chargesDF[-ind,]
charges.test = hf.chargesDF[ind,]

set.seed(1234)
ind = sample(nrow(hf.losDF), .3*nrow(hf.losDF), replace=FALSE)
los.train = hf.losDF[-ind,]
los.test = hf.losDF[ind,]

# --------------------------------- BUILD AND RUN NAIVE BAYES MODEL ----------------------------------------
# Charges
nB.charges = naiveBayes(charge ~ agecat20+adm_src+pay_cat+dnr+oprocs+odiags+size, data = charges.train, laplace = 1)
# Make class predictions
predictedCharges.NB = as.integer(predict(nB.charges, charges.test[,-1])) - 1
conf.chargesNB = table(predictedCharges.NB, charges.test$charge)
# Error rate - Charges:
accCharges.NB = sum(diag(conf.chargesNB))/sum(conf.chargesNB)
errorRateCharges.NB = 1-accCharges.NB

# Get probability of each class
pCharge1.NB = predict(nB.charges, charges.test[,-1], type="raw")
pCharge1.NB = pCharge1.NB[,-1] # Keep only p(class = 1)

# ROC Curve:
predCharges.NB = prediction(pCharge1.NB, charges.test[,1])
perfCharges.NB = performance(predCharges.NB, "tpr", "fpr")
# Caluclate area under ROC curve
aurocCharges.NB = attr(performance(predCharges.NB, "auc"), "y.values")[[1]]
# Lift chart:
perfChargesLift.NB = performance(predCharges.NB, "lift", "rpp")

# LOS -----------------------------
nB.los = naiveBayes(los ~ agecat20+adm_src+pay_cat+dnr+oprocs+odiags+size, data = los.train, laplace = 1)
# Make class predictions
predictedLos.NB = as.integer(predict(nB.los, los.test[,-1])) - 1
conf.LosNB = table(predictedLos.NB, los.test$los)

# Error rate - LOS:
accLos.NB = sum(diag(conf.LosNB))/sum(conf.LosNB)
errorRateLos.NB = 1-accLos.NB

# Get probability of each class
pLos1.NB = predict(nB.los, los.test[,-1], type="raw")
pLos1.NB = pLos1.NB[,-1] # Keep only p(class = 1)

# ROC and Lift Charts:
predLos.NB = prediction(pLos1.NB, los.test[,1])
perfLos.NB = performance(predLos.NB, "tpr", "fpr")
# Caluclate area under ROC curve
aurocLos.NB = attr(performance(predLos.NB, "auc"), "y.values")[[1]]
# Lift chart:
perfLosLift.NB = performance(predLos.NB, "lift", "rpp")

# --------------------------------- BUILD AND RUN LOGISTIC REGRESSION MODEL ----------------------------------------
# ----- This section written by Radhika Sundar -----
library(glmnet)
# Charges ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Convert training and test data to model matrices
x.charges = model.matrix(~-1+agecat20+adm_src+pay_cat+dnr, data=charges.train)
x.charges = cbind(x.charges, charges.train$oprocs, charges.train$odiags, charges.train$size)
y.charges = charges.train$charge
testx.charges = model.matrix(~-1+agecat20+adm_src+pay_cat+dnr, data=charges.test)
testx.charges = cbind(testx.charges, charges.test$oprocs, charges.test$odiags, charges.test$size)
# Build logistic regression model
# logReg.charges = glmnet(x.charges,y.charges,alpha=1,family = "binomial", standardize=FALSE)
logRegCV.charges = cv.glmnet(x.charges, y.charges, alpha=1,family="binomial", standardize=FALSE)
# s=fit1cv$lambda.min
# Run logistic regression model on test data
predictedCharges.logReg = as.integer(predict(logRegCV.charges, testx.charges, s="lambda.min", type="class"))
conf.chargesLogReg = table(predictedCharges.logReg, charges.test$charge)
# Error rate - Charges:
accCharges.LogReg = sum(diag(conf.chargesLogReg))/sum(conf.chargesLogReg)
errorRateCharges.LogReg = 1-accCharges.LogReg
# Get probability class=1
pCharge1.logReg = as.numeric(predict(logRegCV.charges, testx.charges, s="lambda.min", type="response"))
# ROC Curve:
predCharges.LogReg = prediction(pCharge1.logReg, charges.test[,1])
perfCharges.LogReg = performance(predCharges.LogReg, "tpr", "fpr")
# Caluclate area under ROC curve
aurocCharges.LogReg = attr(performance(predCharges.LogReg, "auc"), "y.values")[[1]]
# Lift chart:
perfChargesLift.LogReg = performance(predCharges.LogReg, "lift", "rpp")

# LOS ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Convert training and test data to model matrices
x.los = model.matrix(~-1+agecat20+adm_src+pay_cat+dnr, data=los.train)
x.los = cbind(x.los, los.train$oprocs, los.train$odiags, los.train$size)
y.los = los.train$los
testx.los = model.matrix(~-1+agecat20+adm_src+pay_cat+dnr, data=los.test)
testx.los = cbind(testx.los, los.test$oprocs, los.test$odiags, los.test$size)
# Build logistic regression model
# logReg.los = glmnet(x.los,y.los,alpha=1,family = "binomial", standardize=FALSE)
logRegCV.los = cv.glmnet(x.los, y.los, alpha=1,family="binomial", standardize=FALSE)
# s=fit1cv$lambda.min
# Run logistic regression model on test data
predictedLos.logReg = as.integer(predict(logRegCV.los, testx.los, s="lambda.min", type="class"))
conf.LosLogReg = table(predictedLos.logReg, los.test$los)
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

--------------------------------- BUILD AND RUN GBM MODEL ----------------------------------------
# Charges
source("ensembleGBM_Charge.R")
# Class predictions
predictedCharges.GBM = as.integer(predictedCharges.GBM) - 1
conf.chargesGBM = table(predictedCharges.GBM, charges.test$charge)
# Error rate - Charges:
accCharges.GBM = sum(diag(conf.chargesGBM))/sum(conf.chargesGBM)
errorRateCharges.GBM = 1-accCharges.GBM

# ROC Curve:
predCharges.GBM = prediction(pCharge1.GBM, charges.test[,1])
perfCharges.GBM = performance(predCharges.GBM, "tpr", "fpr")
# Caluclate area under ROC curve
aurocCharges.GBM = attr(performance(predCharges.GBM, "auc"), "y.values")[[1]]
# Lift chart:
perfChargesLift.GBM = performance(predCharges.GBM, "lift", "rpp")

# LOS -----------------------------
# Make class predictions
source("ensembleGBM_LOS.R")
predictedLos.GBM = as.integer(predictedLos.GBM) - 1
conf.LosGBM = table(predictedLos.GBM, los.test$los)

# Error rate - LOS:
accLos.GBM = sum(diag(conf.LosGBM))/sum(conf.LosGBM)
errorRateLos.GBM = 1-accLos.GBM

# ROC Curve:
predLos.GBM = prediction(pLos1.GBM, los.test[,1])
perfLos.GBM = performance(predLos.GBM, "tpr", "fpr")
# Caluclate area under ROC curve
aurocLos.GBM = attr(performance(predLos.GBM, "auc"), "y.values")[[1]]
# Lift chart:
perfLosLift.GBM = performance(predLos.GBM, "lift", "rpp")

# --------------------------------- COMBINE THE MODELS TO MAKE A FINAL DECISION -------------------------------------
# Based on majority class prediction: ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
predictedCharges.EnsMaj = ifelse((predictedCharges.NB + predictedCharges.logReg + predictedCharges.GBM)/3 < 0.5, 0, 1)
# seePreds = cbind(predictedCharges.NB, predictedCharges.logReg, predictedCharges.GBM, predictedCharges.EnsMaj)
conf.chargesEnsMaj = table(predictedCharges.EnsMaj, charges.test$charge)
accCharges.EnsMaj = sum(diag(conf.chargesEnsMaj))/sum(conf.chargesEnsMaj)
errorRateCharges.EnsMaj = 1-accCharges.EnsMaj

predictedLos.EnsMaj = ifelse((predictedLos.NB + predictedLos.logReg + predictedLos.GBM)/3 < 0.5, 0, 1)
# seePreds = cbind(predictedLos.NB, predictedLos.logReg, predictedLos.GBM, predictedLos.EnsMaj)
conf.LosEnsMaj = table(predictedLos.EnsMaj, los.test$los)
accLos.EnsMaj = sum(diag(conf.LosEnsMaj))/sum(conf.LosEnsMaj)
errorRateLos.EnsMaj = 1-accLos.EnsMaj

# If anyone thinks it's a 1, call it a 1. Otherwise it's a 0. +++++++++++++++++++++++++++++++++++++++++++++++++++++
predictedCharges.EnsAny1 = ifelse(predictedCharges.NB == 1 | predictedCharges.logReg == 1 | predictedCharges.GBM == 1, 1, 0)
# seePreds = cbind(predictedCharges.NB, predictedCharges.logReg, predictedCharges.GBM, predictedCharges.EnsAny1)
conf.chargesEnsAny1 = table(predictedCharges.EnsAny1, charges.test$charge)
accCharges.EnsAny1 = sum(diag(conf.chargesEnsAny1))/sum(conf.chargesEnsAny1)
errorRateCharges.EnsAny1 = 1-accCharges.EnsAny1

predictedLos.EnsAny1 = ifelse(predictedLos.NB == 1 | predictedLos.logReg == 1 | predictedLos.GBM == 1, 1, 0)
# seePreds = cbind(predictedLos.NB, predictedLos.logReg, predictedLos.GBM, predictedLos.EnsAny1)
conf.LosEnsAny1 = table(predictedLos.EnsAny1, los.test$los)
accLos.EnsAny1 = sum(diag(conf.LosEnsAny1))/sum(conf.LosEnsAny1)
errorRateLos.EnsAny1 = 1-accLos.EnsAny1

# Average the probability class = 1 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pCharge1.EnsMaj = (pCharge1.NB + pCharge1.logReg + pCharge1.GBM)/3
pCharge1Pred.EnsMaj = ifelse(pCharge1.EnsMaj < 0.5, 0, 1)
seePreds = cbind(pCharge1.NB, pCharge1.logReg, pCharge1.GBM, pCharge1.EnsMaj, pCharge1Pred.EnsMaj)
conf.chargesProbEnsMaj = table(pCharge1Pred.EnsMaj, charges.test$charge)
accCharges.ProbEnsMaj = sum(diag(conf.chargesProbEnsMaj))/sum(conf.chargesProbEnsMaj)
errorRateCharges.ProbEnsMaj = 1-accCharges.ProbEnsMaj
# ROC Curve:
predCharges.EnsMajProb = prediction(pCharge1.EnsMaj, charges.test[,1])
perfCharges.EnsMajProb = performance(predCharges.EnsMajProb, "tpr", "fpr")
# Caluclate area under ROC curve
aurocCharges.EnsMajProb = attr(performance(predCharges.EnsMajProb, "auc"), "y.values")[[1]]
# Lift chart:
perfChargesLift.EnsMajProb = performance(predCharges.EnsMajProb, "lift", "rpp")

pLos1.EnsMaj = (pLos1.NB + pLos1.logReg + pLos1.GBM)/3
pLos1Pred.EnsMaj = ifelse(pLos1.EnsMaj < 0.5, 0, 1)
seePreds = cbind(pLos1.NB, pLos1.logReg, pLos1.GBM, pLos1.EnsMaj, pLos1Pred.EnsMaj)
conf.losProbEnsMaj = table(pLos1Pred.EnsMaj, los.test$los)
accLos.ProbEnsMaj = sum(diag(conf.losProbEnsMaj))/sum(conf.losProbEnsMaj)
errorRateLos.ProbEnsMaj = 1-accLos.ProbEnsMaj
# ROC Curve:
predLos.EnsMajProb = prediction(pLos1.EnsMaj, los.test[,1])
perfLos.EnsMajProb = performance(predLos.EnsMajProb, "tpr", "fpr")
# Caluclate area under ROC curve
aurocLos.EnsMajProb = attr(performance(predLos.EnsMajProb, "auc"), "y.values")[[1]]
# Lift chart:
perfLosLift.EnsMajProb = performance(predLos.EnsMajProb, "lift", "rpp")

# If anyone's p(class=1) > .5, use the max p(class=1); else, use min ++++++++++++++++++++++++++++++++++++++++++++++++++++
pCharge1.Any1Prob = ifelse(pCharge1.NB >= .5 | pCharge1.logReg >= .5 | pCharge1.GBM >= .5, 
                           max(pCharge1.NB, pCharge1.logReg, pCharge1.GBM),
                           (pCharge1.NB + pCharge1.logReg + pCharge1.GBM)/3)
pCharge1Pred.Any1Prob = ifelse(pCharge1.Any1Prob < 0.5, 0, 1)
# seePreds = cbind(pCharge1.NB, pCharge1.logReg, pCharge1.GBM, pCharge1.Any1Prob, pCharge1Pred.Any1Prob)
conf.chargesAny1Prob = table(pCharge1Pred.Any1Prob, charges.test$charge)
accCharges.Any1Prob = sum(diag(conf.chargesAny1Prob))/sum(conf.chargesAny1Prob)
errorRateCharges.Any1Prob = 1-accCharges.Any1Prob
# ROC Curve:
predCharges.Any1Prob = prediction(pCharge1.Any1Prob, charges.test[,1])
perfCharges.Any1Prob = performance(predCharges.Any1Prob, "tpr", "fpr")
# Caluclate area under ROC curve
aurocCharges.Any1Prob = attr(performance(predCharges.Any1Prob, "auc"), "y.values")[[1]]
# Lift chart:
perfChargesLift.Any1Prob = performance(predCharges.Any1Prob, "lift", "rpp")

pLos1.Any1Prob = ifelse(pLos1.NB >= .5 | pLos1.logReg >= .5 | pLos1.GBM >= .5, 
                           max(pLos1.NB, pLos1.logReg, pLos1.GBM),
                           (pLos1.NB + pLos1.logReg + pLos1.GBM)/3)
pLos1Pred.Any1Prob = ifelse(pLos1.Any1Prob < 0.5, 0, 1)
# seePreds = cbind(pLos1.NB, pLos1.logReg, pLos1.GBM, pLos1.Any1Prob, pLos1Pred.Any1Prob)
conf.losAny1Prob = table(pLos1Pred.Any1Prob, los.test$los)
accLos.Any1Prob = sum(diag(conf.losAny1Prob))/sum(conf.losAny1Prob)
errorRateLos.Any1Prob = 1-accLos.Any1Prob
# ROC Curve:
predLos.Any1Prob = prediction(pLos1.Any1Prob, los.test[,1])
perfLos.Any1Prob = performance(predLos.Any1Prob, "tpr", "fpr")
# Caluclate area under ROC curve
aurocLos.Any1Prob = attr(performance(predLos.Any1Prob, "auc"), "y.values")[[1]]
# Lift chart:
perfLosLift.Any1Prob = performance(predLos.Any1Prob, "lift", "rpp")

# --------------------------------- ROC and LIFT CURVES -------------------------------------------------------------
# ROC for Charges
par(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="white")
plot(perfCharges.NB, main='ROC curve for Charges - Arrhythmia', add = FALSE)
par(col="yellow")
plot(perfCharges.LogReg, add=TRUE)
par(col="thistle")
plot(perfCharges.Any1Prob, add=TRUE)
par(col="aquamarine")
plot(perfCharges.GBM, add=TRUE)

# Lift for Charges
par(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="white")
plot(perfChargesLift.NB, main='Lift curve for Charges - Arrhythmia', add = FALSE)
par(col="yellow")
plot(perfChargesLift.LogReg, add=TRUE)
par(col="thistle")
plot(perfChargesLift.EnsMajProb, add=TRUE)
par(col="aquamarine")
plot(perfChargesLift.GBM, add=TRUE)

# ROC for LOS
par(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="white")
plot(perfLos.NB, main='ROC curve for LOS - Arrhythmia', add = FALSE)
par(col="yellow")
plot(perfLos.LogReg, add=TRUE)
par(col="thistle")
plot(perfLos.EnsMajProb, add=TRUE)
par(col="aquamarine")
plot(perfLos.GBM, add=TRUE)

# Lift for LOS
par(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="white")
plot(perfLosLift.NB, main='Lift curve for LOS - Arrhythmia', add = FALSE)
par(col="yellow")
plot(perfLosLift.LogReg, add=TRUE)
par(col="thistle")
plot(perfLosLift.EnsMajProb, add=TRUE)
par(col="aquamarine")
plot(perfLosLift.GBM, add=TRUE)
