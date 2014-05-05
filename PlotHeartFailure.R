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

# ------------------------------------ DISPOSITION -----------------------------------------------------------
par(mfrow=c(1,1))
# BIN 2-7 !!!!!!!!!!
# Histogram of disposition - absolute occurrences:
hf.disp = as.factor(hf$disp)
plot(hf.disp, main = "Histogram for disposition, heart failure", xlab = "disp", ylab = "count")
summary(hf.disp)

# Histogram of disposition - relative frequency
hf.disp.counts = summary(hf.disp)
hf.disp.pct = hf.disp.counts/nrow(hf)
plot(hf.disp.pct, type="h", main = "Histogram for disposition, heart failure", xlab = "disp", ylab = "%")

# Histogram of disposition with 01 removed - relative frequency
hf.disp.no01 = as.factor(hf$disp[-which(hf$disp=="01")])
hf.disp.no01.counts = summary(hf.disp.no01)
hf.disp.no01.pct = hf.disp.no01.counts/sum(hf.disp.no01.counts)
plot(hf.disp.no01.pct, type="h", main = "Histogram for disposition with 01 removed, heart failure", xlab = "disp", ylab = "%")

# ------------------------------------ LENGTH OF STAY --------------------------------------------------------

hf.los = as.numeric(hf$los)
hf.los30 = hf.los[which(hf.los<30)]

# Density plot of LOS:
plot(density(hf.los30), main="Density plot for length of stay less than 30 days, heart failure", xlab = "los", ylab = "Density")

# Boxplot of LOS:
hf.losBox = boxplot(hf.los)
summary(hf.losBox)
hf.losBox$stats

# ------------------------------------ TOTAL CHARGES --------------------------------------------------------_

hf.chargesDF = hf[,c("charge", "los")]
hf.chargesDF$charge = as.numeric(hf.chargesDF$charge)
hf.chargesDF$los = as.numeric(hf.chargesDF$los)

hf.chargesDF = hf.chargesDF[-which(hf.chargesDF$charge==0 | hf.chargesDF$charge==1 | hf.chargesDF$los > 365),]

hf.charges = hf.chargesDF$charge

hf.charges100K = hf.charges[which(hf.charges<100000)]
hf.charges10K = hf.charges[which(hf.charges<10000)]
hf.charges1020K = hf.charges[which(hf.charges<20000 & hf.charges>10000)]
hf.charges2030K = hf.charges[which(hf.charges<30000 & hf.charges>20000)]

# Density plot of Charges:
par(mfrow=c(3,1))
# plot(density(hf.charges), main="Density plot for Total Charges, heart failure", xlab = "Total Charges", ylab = "Density")
plot(density(hf.charges100K), main="Density plot for Total Charges < 100K, heart failure", xlab = "Total Charges", ylab = "Density")
plot(density(hf.charges10K), main="Density plot for Total Charges < 10K, heart failure", xlab = "Total Charges", ylab = "Density")
plot(density(hf.charges1020K), main="Density plot for Total Charges between 10K and 20K, heart failure", xlab = "Total Charges", ylab = "Density")
plot(density(hf.charges2030K), main="Density plot for Total Charges between 20K and 30K, heart failure", xlab = "Total Charges", ylab = "Density")

# Boxplot of Charges:
par(mfrow=c(1,1))
hf.chargesBox = boxplot(hf.charges)
summary(hf.chargesBox)
hf.chargesBox$stats