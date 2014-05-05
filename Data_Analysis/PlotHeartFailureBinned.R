## Clear workspace
rm(list=ls())

## Set up workspace
# setwd("...")

hf = read.csv("by_diagnosis/heartFailure2009.txt", colClasses = "character")
hf = rbind(hf, read.csv("by_diagnosis/heartFailure2010.txt", colClasses = "character"))
hf = rbind(hf, read.csv("by_diagnosis/heartFailure2011.txt", colClasses = "character"))

# Filter data
hf = hf[,c("disp", "los", "charge", "agecat5","sex","race","patcnty","adm_src","pay_cat")]
hf[hf ==""] <- NA
hf[hf =="*"] <- NA
hf = na.omit(hf)

hf$disp = as.integer(hf$disp)
hf$los = as.integer(hf$los)
hf$charge = as.integer(hf$charge)

# +++++++++++++++++++++++++++++++ OUTCOME: CHARGES +++++++++++++++++++++++++++++++++++++++++++++++
# For predicting charges, removes records where:
# - total charges are $0 or $1
# - or LOS is > 365
hf.charges = hf
hf.charges = hf.charges[-which(hf.charges$charge==0 | hf.charges$charge==1 | hf.charges$los > 365),]
# Bin charges as decided: $0-5k is level 1, $5k-10k is level 2, $10k-30k is level 3, $30k-70k is level 4, and above $70k is level 5
hf.charges$charge = ifelse(hf.charges$charge<5001, 1, ifelse(hf.charges$charge<10001, 2, ifelse(hf.charges$charge<30001, 3, ifelse(hf.charges$charge<70001, 4, 5))))
hf.charges$charge = as.factor(hf.charges$charge)

plot(hf.charges$charge, main = "Histogram for charges, heart failure", xlab = "charge bin", ylab = "count")
summary(hf.charges$charge)

# +++++++++++++++++++++++++++++++ OUTCOME: DISPOSITION +++++++++++++++++++++++++++++++++++++++++++
# Bin dispositions 02-07 together, call it level 2; all others remain unchanged
hf.disp = hf[,c("disp", "agecat5","sex","race","patcnty","adm_src","pay_cat")]
hf.disp$disp = ifelse(hf.disp$disp>1 & hf.disp$disp<8, 2, hf.disp$disp)
hf.disp$disp = as.factor(hf.disp$disp)

plot(hf.disp$disp, main = "Histogram for disposition, heart failure", xlab = "disp (binned)", ylab = "count")
summary(hf.disp$disp)

# +++++++++++++++++++++++++++++++ OUTCOME: LOS +++++++++++++++++++++++++++++++++++++++++++++++++++
# Bin LOS as decided: 0-5 days is level 1, 6-10 days is level 2, 11-15 days is level 3, and over 15 days is level 4
hf.los = hf[,c("los", "agecat5","sex","race","patcnty","adm_src","pay_cat")]
# hf.los$los = ifelse(hf.los$los<6, 1, ifelse(hf.los$los<11, 2, ifelse(hf.los$los<16, 3, 4)))
hf.los$los = ifelse(hf.los$los>10, 11, hf.los$los)
hf.los$los = as.factor(hf.los$los)

plot(hf.los$los, main = "Histogram for LOS (binned), heart failure", xlab = "LOS bin", ylab = "count")
summary(hf.los$los)