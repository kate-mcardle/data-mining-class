## Clear workspace
rm(list=ls())

## Set up workspace
# setwd("...")

library(e1071)
library("ROCR")

hf = read.csv("by_diagnosis/psych2009.txt", colClasses = "character")
hf = rbind(hf, read.csv("by_diagnosis/psych2010.txt", colClasses = "character"))
hf = rbind(hf, read.csv("by_diagnosis/psych2011.txt", colClasses = "character"))

## ------------------------- DATA PRE-PROCESSING ---------------------------------------------

# Add number of additional procedures as a column
source("getOtherProcCounts.R")

# Add number of additional diagnoses as a column
source("getODiagCounts.R")

# hf = hf[,c("proc_p","los","charge","agecat5","sex","race","patcnty","adm_src","pay_cat","dnr","oprocs","odiags","oshpd_id")]
hf = hf[,c("los","charge","agecat5","adm_src","pay_cat","dnr","oprocs","odiags","oshpd_id")]

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
# hf$sex = as.factor(hf$sex)
# hf$race = as.factor(hf$race)
# hf$patcnty = as.factor(hf$patcnty)
hf$adm_src = as.factor(hf$adm_src)
# hf$adm_src = as.factor(as.integer(as.integer(hf$adm_src)/100))
hf$pay_cat = as.factor(hf$pay_cat)
hf$dnr = as.factor(hf$dnr)
# hf$oshpd_id = as.factor(hf$oshpd_id)
hf = na.omit(hf)

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
nB.charges = naiveBayes(charge ~ ., data = charges.train, laplace = 1)
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
nB.los = naiveBayes(los ~ ., data = los.train, laplace = 1)
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
x.charges = model.matrix(~-1+agecat5+adm_src+pay_cat+dnr, data=charges.train)
x.charges = cbind(x.charges, charges.train$oprocs, charges.train$odiags, charges.train$size)
y.charges = charges.train$charge
testx.charges = model.matrix(~-1+agecat5+adm_src+pay_cat+dnr, data=charges.test)
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
x.los = model.matrix(~-1+agecat5+adm_src+pay_cat+dnr, data=los.train)
x.los = cbind(x.los, los.train$oprocs, los.train$odiags, los.train$size)
y.los = los.train$los
testx.los = model.matrix(~-1+agecat5+adm_src+pay_cat+dnr, data=los.test)
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

# --------------------------------- BUILD AND RUN GBM MODEL ----------------------------------------
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