## Clear workspace
rm(list=ls())

## Set up workspace
# setwd("...")

hf = read.csv("by_diagnosis/heartFailure2009.txt", colClasses = "character")
hf = rbind(hf, read.csv("by_diagnosis/heartFailure2010.txt", colClasses = "character"))
hf = rbind(hf, read.csv("by_diagnosis/heartFailure2011.txt", colClasses = "character"))

# Filter out missing/masked data
hf = hf[,c("disp", "los", "charge", "agecat5","sex","race","patcnty","adm_src","pay_cat")]
hf[hf ==""] <- NA
hf[hf =="*"] <- NA
hf = na.omit(hf)

# Convert admission source to only contain the first significant digit
hf$adm_src = as.factor(as.integer(as.integer(hf$adm_src)/100))

# For predicting charges, use separate data frame that removes records where:
# - total charges are $0 or $1
# - or LOS is > 365
hf.chargesDF = hf # Be sure that "hf$disp" has not been binned (as below) at the time you run this particular line of code
hf.chargesDF$charge = as.integer(hf.chargesDF$charge)
hf.chargesDF$los = as.integer(hf.chargesDF$los)
hf.chargesDF = hf.chargesDF[-which(hf.chargesDF$charge==0 | hf.chargesDF$charge==1 | hf.chargesDF$los > 365),]
# Bin charges as decided: $0-5k is level 1, $5k-10k is level 2, $10k-30k is level 3, $30k-70k is level 4, and above $70k is level 5
hf.chargesDF$charge = ifelse(hf.chargesDF$charge<5001, 1, ifelse(hf.chargesDF$charge<10001, 2, ifelse(hf.chargesDF$charge<30001, 3, ifelse(hf.chargesDF$charge<70001, 4, 5))))

# Bin dispositions 02-07 together, call it level 2; all other dispositions remain unchanged
hf$disp = as.integer(hf$disp)
hf$disp = ifelse(hf$disp>1 & hf$disp<8, 2, hf$disp)
hf$disp = as.factor(hf$disp)

# Bin LOS as decided: 0-5 days is level 1, 6-10 days is level 2, 11-15 days is level 3, and over 15 days is level 4
hf$los = as.integer(hf$los)
hf$los = ifelse(hf$los<6, 1, ifelse(hf$los<11, 2, ifelse(hf$los<16, 3, 4)))
hf$los = as.factor(hf$los)
