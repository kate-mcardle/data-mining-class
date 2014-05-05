## Clear workspace
rm(list=ls())

## Set up workspace
# setwd("...")

#print("HF Counts per year 2009, 2010, 2011")
hf09 = read.csv("by_diagnosis/heartFailure2009.txt", colClasses = "character")
hf10 = read.csv("by_diagnosis/heartFailure2010.txt", colClasses = "character")
hf11 = read.csv("by_diagnosis/heartFailure2011.txt", colClasses = "character")
hf = rbind(hf09,hf10,hf11)
#print(nrow(hf09))
#print(nrow(hf10))
#print(nrow(hf11))

# Filter out records where proc_p != 3995
hf = hf[,c("proc_p","los","charge","age_yrs","agecat5","sex","race","patcnty","adm_src","pay_cat","oshpd_id")]
hf$los = as.integer(hf$los)
hf$age_yrs = as.numeric(hf$age_yrs)
hf$agecat5 = as.factor(hf$agecat5)
hf$sex = as.factor(hf$sex)
hf$race = as.factor(hf$race)
hf$patcnty = as.factor(hf$patcnty)
hf$oshpd_id = as.factor(hf$oshpd_id)
#hf$dnr = as.factor(hf$dnr)
hf$pay_cat = as.factor(hf$pay_cat)
hf$adm_src = as.factor(hf$adm_src)
#hf$proc_p = substr(hf$proc_p,1,2)
hf$proc_p = as.factor(hf$proc_p)
hf$charge = as.integer(hf$charge)

# # Plot spread of charges
# par(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="white")
# boxplot(hf$charge, ylab="Total Charges")

# hf.sex = hf
# hf.race = hf
# hf.patcnty = hf
# hf.admsrc = hf
# hf.paycat = hf
# hf.los = hf
# hf.charge = hf
# hf.charge = hf.charge[hf.charge$charge>1,]
# hf.charge = hf.charge[hf.charge$charge<10000000,]
#hf = hf[,-c(2,3)]
#hf = hf[which(hf$proc_p==3995),]
#hf = hf[,-1] # remove proc_p from variable list
la_pop=9889056 
la_county=19
sfo_pop=825863 
sfo_county=38
scla_pop=1809378 
scla_county=43
hf.la = hf[hf$patcnty==la_county,]
hf.sfo = hf[hf$patcnty==sfo_county,]
hf.scla = hf[hf$patcnty==scla_county,]
print("Heart Failure Per Capita : LA, SFO, SCLA")
print(nrow(hf.la)/la_pop)
print(nrow(hf.sfo)/sfo_pop)
print(nrow(hf.scla)/scla_pop)
print("Heart Failure Mean Charges : LA, SFO, SCLA")
# hf.la.less = hf.la[which(hf.la$charge>1 & hf.la$charge<1000000),]
print(mean(hf.la[which(hf.la$charge>1 & hf.la$charge<1000000),]$charge))
print(mean(hf.sfo[which(hf.sfo$charge>1 & hf.sfo$charge<1000000),]$charge))
print(mean(hf.scla[which(hf.scla$charge>1 & hf.scla$charge<1000000),]$charge))

# print("Heart Failure Mean Age : LA, SFO, SCLA")
# # hf.la.less = hf.la[which(hf.la$charge>1 & hf.la$charge<1000000),]
# hf.la.age = na.omit(hf.la$age_yrs)
# hf.sfo.age = na.omit(hf.sfo$age_yrs)
# hf.scla.age = na.omit(hf.scla$age_yrs)
# print(mean(hf.la.age))
# print(mean(hf.sfo.age))
# print(mean(hf.scla.age))

print("Heart Failure Mean Charges for pay_cat : LA, SFO, SCLA")
print(mean(hf.la[which(hf.la$charge>1 & hf.la$charge<1000000 & hf.la$pay_cat=="08"),]$charge))
print(mean(hf.sfo[which(hf.sfo$charge>1 & hf.sfo$charge<1000000 & hf.sfo$pay_cat=="08"),]$charge))
print(mean(hf.scla[which(hf.scla$charge>1 & hf.scla$charge<1000000 & hf.scla$pay_cat=="08"),]$charge))

pay.cat = read.csv("pay_cat_plot.csv")
par(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="white")
hist(pay.cat[1,-1])
par(col="red")
plot(pay.cat[,3], add=TRUE)

print("Heart Failure Mean Charges for 3 biggest hospitals: 190555, 190148, 100899")
print(mean(hf[which(hf$charge>1 & hf$charge<1000000),]$charge))
print(mean(hf[which(hf$charge>1 & hf$charge<1000000 & hf$oshpd_id=="190555"),]$charge))
print(mean(hf[which(hf$charge>1 & hf$charge<1000000 & hf$oshpd_id=="190148"),]$charge))
print(mean(hf[which(hf$charge>1 & hf$charge<1000000 & hf$oshpd_id=="100899"),]$charge))