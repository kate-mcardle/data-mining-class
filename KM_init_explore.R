## Clear workspace
rm(list=ls())

## Set working directory.
# setwd("...")

## Load data into R
hf = read.csv("HF/2009_LA.txt", colClasses = "character")
hf = rbind(hf, read.csv("HF/2009_North.txt", colClasses = "character"))
hf = rbind(hf, read.csv("HF/2009_South.txt", colClasses = "character"))
hf = rbind(hf, read.csv("HF/2010_LA.txt", colClasses = "character"))
hf = rbind(hf, read.csv("HF/2010_North.txt", colClasses = "character"))
hf = rbind(hf, read.csv("HF/2010_South.txt", colClasses = "character"))
hf = rbind(hf, read.csv("HF/2011_LA.txt", colClasses = "character"))
hf = rbind(hf, read.csv("HF/2011_North.txt", colClasses = "character"))
hf = rbind(hf, read.csv("HF/2011_South.txt", colClasses = "character"))

hd = read.csv("HD/2009_LA.txt", colClasses = "character")
hd = rbind(hd, read.csv("HD/2009_North.txt", colClasses = "character"))
hd = rbind(hd, read.csv("HD/2009_South.txt", colClasses = "character"))
hd = rbind(hd, read.csv("HD/2010_LA.txt", colClasses = "character"))
hd = rbind(hd, read.csv("HD/2010_North.txt", colClasses = "character"))
hd = rbind(hd, read.csv("HD/2010_South.txt", colClasses = "character"))
hd = rbind(hd, read.csv("HD/2011_LA.txt", colClasses = "character"))
hd = rbind(hd, read.csv("HD/2011_North.txt", colClasses = "character"))
hd = rbind(hd, read.csv("HD/2011_South.txt", colClasses = "character"))

ht = read.csv("HT/2009_LA.txt", colClasses = "character")
ht = rbind(ht, read.csv("HT/2009_North.txt", colClasses = "character"))
ht = rbind(ht, read.csv("HT/2009_South.txt", colClasses = "character"))
ht = rbind(ht, read.csv("HT/2010_LA.txt", colClasses = "character"))
ht = rbind(ht, read.csv("HT/2010_North.txt", colClasses = "character"))
ht = rbind(ht, read.csv("HT/2010_South.txt", colClasses = "character"))
ht = rbind(ht, read.csv("HT/2011_LA.txt", colClasses = "character"))
ht = rbind(ht, read.csv("HT/2011_North.txt", colClasses = "character"))
ht = rbind(ht, read.csv("HT/2011_South.txt", colClasses = "character"))
# remove garbage rows
# ht = ht[-15669,]
# ht = ht[-13575,]
# hf = hf[-184377,]

## Display histograms for payer category:
hd_pay_cat = as.factor(hd$pay_cat)
hf_pay_cat = as.factor(hf$pay_cat)
ht_pay_cat = as.factor(ht$pay_cat)

par(mfrow=c(3,1))
plot(hd_pay_cat, main = "Histogram for pay_cat, heart disease", xlab = "pay_cat", ylab = "count")
plot(hf_pay_cat, main = "Histogram for pay_cat, heart failure", xlab = "pay_cat", ylab = "count")
plot(ht_pay_cat, main = "Histogram for pay_cat, hypertension", xlab = "pay_cat", ylab = "count")

## Display histograms for source of admission:
hd_adm_src = as.numeric(hd$adm_src)
for(i in 1:length(hd_adm_src)) {
  if (hd_adm_src[i] < 100) { hd_adm_src[i] = 0 }
  else if (hd_adm_src[i] < 200) { hd_adm_src[i] = 1 }
  else if (hd_adm_src[i] < 300) { hd_adm_src[i] = 2 }
  else if (hd_adm_src[i] < 400) { hd_adm_src[i] = 3 }
  else if (hd_adm_src[i] < 500) { hd_adm_src[i] = 4 }
  else if (hd_adm_src[i] < 600) { hd_adm_src[i] = 5 }
  else if (hd_adm_src[i] < 700) { hd_adm_src[i] = 6 }
  else if (hd_adm_src[i] < 800) { hd_adm_src[i] = 7 }
  else if (hd_adm_src[i] < 900) { hd_adm_src[i] = 8 }
  else { hd_adm_src[i] = 9 }  
}
hd_adm_src = as.factor(hd_adm_src)

hf_adm_src = as.numeric(hf$adm_src)
for(i in 1:length(hf_adm_src)) {
  if (hf_adm_src[i] < 100) { hf_adm_src[i] = 0 }
  else if (hf_adm_src[i] < 200) { hf_adm_src[i] = 1 }
  else if (hf_adm_src[i] < 300) { hf_adm_src[i] = 2 }
  else if (hf_adm_src[i] < 400) { hf_adm_src[i] = 3 }
  else if (hf_adm_src[i] < 500) { hf_adm_src[i] = 4 }
  else if (hf_adm_src[i] < 600) { hf_adm_src[i] = 5 }
  else if (hf_adm_src[i] < 700) { hf_adm_src[i] = 6 }
  else if (hf_adm_src[i] < 800) { hf_adm_src[i] = 7 }
  else if (hf_adm_src[i] < 900) { hf_adm_src[i] = 8 }
  else { hf_adm_src[i] = 9 }  
}
hf_adm_src = as.factor(hf_adm_src)

ht_adm_src = as.numeric(ht$adm_src)
for(i in 1:length(ht_adm_src)) {
  if (ht_adm_src[i] < 100) { ht_adm_src[i] = 0 }
  else if (ht_adm_src[i] < 200) { ht_adm_src[i] = 1 }
  else if (ht_adm_src[i] < 300) { ht_adm_src[i] = 2 }
  else if (ht_adm_src[i] < 400) { ht_adm_src[i] = 3 }
  else if (ht_adm_src[i] < 500) { ht_adm_src[i] = 4 }
  else if (ht_adm_src[i] < 600) { ht_adm_src[i] = 5 }
  else if (ht_adm_src[i] < 700) { ht_adm_src[i] = 6 }
  else if (ht_adm_src[i] < 800) { ht_adm_src[i] = 7 }
  else if (ht_adm_src[i] < 900) { ht_adm_src[i] = 8 }
  else { ht_adm_src[i] = 9 }  
}
ht_adm_src_fac = as.factor(ht_adm_src)

par(mfrow=c(3,1))
plot(hd_adm_src_fac, main = "Histogram for adm_src, heart disease", xlab = "adm_src", ylab = "count")
plot(hf_adm_src_fac, main = "Histogram for adm_src, heart failure", xlab = "adm_src", ylab = "count")
plot(ht_adm_src_fac, main = "Histogram for adm_src, hypertension", xlab = "adm_src", ylab = "count")

## Scatterplots for payer category
hd_pay_cat_num = as.numeric(hd$pay_cat)
hf_pay_cat_num = as.numeric(hf$pay_cat)
ht_pay_cat_num = as.numeric(ht$pay_cat)

# LOS
hd_los = as.numeric(hd$los)
hf_los = as.numeric(hf$los)
ht_los = as.numeric(ht$los)

par(mfrow=c(1,1))
plot(hd_pay_cat_num, hd_los, main = "Heart Disease: pay_cat vs los")
plot(hf_pay_cat_num, hf_los, main = "Heart Failure: pay_cat vs los")
plot(ht_pay_cat_num, ht_los, main = "Hypertension: pay_cat vs los")

# Total Charges
hd_charges = as.numeric(hd$charge)
hf_charges = as.numeric(hf$charge)
ht_charges = as.numeric(ht$charge)

par(mfrow=c(1,1))
plot(hd_pay_cat_num, hd_charges, main = "Heart Disease: pay_cat vs charge")
plot(hf_pay_cat_num, hf_charges, main = "Heart Failure: pay_cat vs charge")
plot(ht_pay_cat_num, ht_charges, main = "Hypertension: pay_cat vs charge")

## Scatterplots for admission source
# LOS
par(mfrow=c(1,1))
plot(hd_adm_src, hd_los, main = "Heart Disease: adm_src vs los")
plot(hf_adm_src, hf_los, main = "Heart Failure: adm_src vs los")
plot(ht_adm_src, ht_los, main = "Hypertension: adm_src vs los")

# Total Charges
plot(hd_adm_src, hd_charges, main = "Heart Disease: adm_src vs charge")
plot(hf_adm_src, hf_charges, main = "Heart Failure: adm_src vs charge")
plot(ht_adm_src, ht_charges, main = "Hypertension: adm_src vs charge")

## Get table counts for Disposition
hd_disp = as.factor(hd$disp)
hf_disp = as.factor(hf$disp)
ht_disp = as.factor(ht$disp)

# adm_src
HD_admsrcVScharge = table(hd_adm_src, hd_disp)
HF_admsrcVScharge = table(hf_adm_src, hf_disp)
HT_admsrcVScharge = table(ht_adm_src, ht_disp)

print(HD_admsrcVScharge)
print(HF_admsrcVScharge)
print(HT_admsrcVScharge)

# pay_cat
HD_paycatVScharge = table(hd_pay_cat, hd_disp)
HF_paycatVScharge = table(hf_pay_cat, hf_disp)
HT_paycatVScharge = table(ht_pay_cat, ht_disp)

print(HD_paycatVScharge)
print(HF_paycatVScharge)
print(HT_paycatVScharge)

## Get table counts for Treatment
hd_proc = as.factor(hd$proc_p)
hf_proc = as.factor(hf$proc_p)
ht_proc = as.factor(ht$proc_p)

# adm_src
HD_admsrcVSproc = table(hd_adm_src, hd_proc)
HF_admsrcVSproc = table(hf_adm_src, hf_proc)
HT_admsrcVSproc = table(ht_adm_src, ht_proc)

# print(HD_admsrcVSproc)
# print(HF_admsrcVSproc)
# print(HT_admsrcVSproc)

# pay_cat
HD_paycatVSproc = table(hd_pay_cat, hd_proc)
HF_paycatVSproc = table(hf_pay_cat, hf_proc)
HT_paycatVSproc = table(ht_pay_cat, ht_proc)

# print(HD_paycatVSproc)
# print(HF_paycatVSproc)
# print(HT_paycatVSproc)