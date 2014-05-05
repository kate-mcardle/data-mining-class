hf.chargesDF = hf
hf.chargesDF$charge = as.integer(hf.chargesDF$charge)
hf.chargesDF$los = as.integer(hf.chargesDF$los)
hf.chargesDF = hf.chargesDF[-which(hf.chargesDF$charge==0 | hf.chargesDF$charge==1 | hf.chargesDF$los > 365),]

# Bin charges and LOS based on spread (see section below) ------------------------------
# Bin charges: $0-36,331 is level 0, above $36,331 is level 1
hf.chargesDF$charge = ifelse(hf.chargesDF$charge<mean(hf.chargesDF$charge), 0, 1)
hf.chargesDF$charge = as.factor(hf.chargesDF$charge)
#hf.chargesDF = hf.chargesDF[,-c("los","proc_p")] # remove LOS from this data frame

# Bin LOS: 2 or fewer days is level 0, over 2 days is level 1
hf.losDF = subset(hf,select=-(charge)) # drop charge because we don't want it here
hf.losDF$los = as.integer(hf.losDF$los)
#hf.losDF$los = hf.losDF[hf.losDF!=0,] 
hf.losDF$los = ifelse(hf.losDF$los<mean(hf.losDF$los), 0, 1)
hf.losDF$los = as.factor(hf.losDF$los)

hf.chargesDF = subset(hf.chargesDF,select=-(oshpd_id)) 
hf.losDF = subset(hf.losDF,select=-(oshpd_id)) 
hf.chargesDF = subset(hf.chargesDF,select=-(proc_p)) 
hf.losDF = subset(hf.losDF,select=-(proc_p)) 

# # ----------------------------------------------------------------------------------------------
# # To take a look at the spread of LOS and charges, uncomment this section AND
# # comment the above section (because you don't want the outcome variables already binned!)
# hf.los = as.numeric(hf$los)
# hf.los30 = hf.los[which(hf.los<30)]
# 
# # Density plot of LOS:
# par(mfrow=c(2,1))
# plot(density(hf.los), main="Density plot for length of stay, heart failure, proc_p = 3995", xlab = "los", ylab = "Density")
# plot(density(hf.los30), main="Density plot for length of stay less than 30 days, heart failure, proc_p = 3995", xlab = "los", ylab = "Density")
# 
# # Boxplot of LOS:
# hf.losBox = boxplot(hf.los)
# summary(hf.losBox)
# hf.losBox$stats
# 
# # Charges:
# hf.chargesDF = hf[,c("charge", "los")]
# hf.chargesDF$charge = as.numeric(hf.chargesDF$charge)
# hf.chargesDF$los = as.numeric(hf.chargesDF$los)
# 
# hf.chargesDF = hf.chargesDF[-which(hf.chargesDF$charge==0 | hf.chargesDF$charge==1 | hf.chargesDF$los > 365),]
# 
# hf.charges = hf.chargesDF$charge
# hf.charges100K = hf.charges[which(hf.charges<100000)]
# 
# # Density plot of Charges:
# par(mfrow=c(2,1))
# plot(density(hf.charges), main="Density plot for Total Charges, heart failure, proc_p = 3995", xlab = "Total Charges", ylab = "Density")
# plot(density(hf.charges100K), main="Density plot for Total Charges < 100K, heart failure, proc_p = 3995", xlab = "Total Charges", ylab = "Density")
# 
# # Boxplot of Charges:
# par(mfrow=c(1,1))
# hf.chargesBox = boxplot(hf.charges)
# summary(hf.chargesBox)
# hf.chargesBox$stats
# 
# # Just for fun, let's see if the spread of dispositions has changed at all
# # Bin dispositions 02-07 together, call it level 2; all others remain unchanged
# # hf.disp = hf[,c("disp", "agecat5","sex","race","patcnty","adm_src","pay_cat")]
# # hf.disp$disp = ifelse(hf.disp$disp>1 & hf.disp$disp<8, 2, hf.disp$disp)
# hf.disp = as.factor(hf$disp)
# par(mfrow=c(1,1))
# plot(hf.disp, main = "Histogram for disposition, heart failure, proc_p = 3995", xlab = "disp", ylab = "count")
# summary(hf.disp)
# ## NOPE, they haven't!
