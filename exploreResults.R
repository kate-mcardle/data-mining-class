# To be run after ensemble_***.R has been run
results.los = cbind(los.test, predictedLos.GBM)
ClassLabel = paste("predicted ",results.los$predictedLos.GBM,", actual ",results.los$los, sep="")
results.los = cbind(results.los,ClassLabel)
# nrow(results.los[which(results.los$adm_src==131 & (results.los$ClassLabel=="p0a1" | results.los$ClassLabel=="p1a0")),])
# nrow(results.los[which(results.los$adm_src==131),])

# C.pred0act1 = charges.test[which(predictedCharges.GBM==0 & charges.test$charge==1),]
# C.pred0act0 = charges.test[which(predictedCharges.GBM==0 & charges.test$charge==0),]
# C.pred1act1 = charges.test[which(predictedCharges.GBM==1 & charges.test$charge==1),]
# C.pred1act0 = charges.test[which(predictedCharges.GBM==1 & charges.test$charge==0),]
# 
# LOS.pred0act1 = los.test[which(predictedLos.GBM==0 & los.test$los==1),]
# LOS.pred0act0 = los.test[which(predictedLos.GBM==0 & los.test$los==0),]
# LOS.pred1act1 = los.test[which(predictedLos.GBM==1 & los.test$los==1),]
# LOS.pred1act0 = los.test[which(predictedLos.GBM==1 & los.test$los==0),]
par(opar)
library(ggplot2)
ggplot(results.los, aes(agecat5, fill=ClassLabel)) + geom_bar(position="dodge")+scale_fill_grey()+theme_set(theme_bw(24))+ylab("Number of records")+theme(axis.title.x = element_blank())
ggplot(results.los, aes(race, fill=ClassLabel)) + geom_bar(position="dodge")
ggplot(results.los, aes(adm_src, fill=ClassLabel)) + geom_bar(position="dodge")+scale_fill_grey()+theme_set(theme_bw(21))+ylab("Number of records")+theme(legend.position="none")+theme(axis.title.x = element_blank())
ggplot(results.los, aes(pay_cat, fill=ClassLabel)) + geom_bar(position="dodge")+scale_fill_grey()+theme_set(theme_bw(24))+ylab("Number of records")+theme(legend.position="none")+theme(axis.title.x = element_blank())
ggplot(results.los, aes(dnr, fill=ClassLabel)) + geom_bar(position="dodge")
ggplot(results.los, aes(oprocs, fill=ClassLabel)) + geom_bar(position="dodge")
ggplot(results.los, aes(odiags, fill=ClassLabel)) + geom_bar(position="dodge")+scale_fill_grey()+theme_set(theme_bw(24))+ylab("Number of records")+theme(legend.position="none")+theme(axis.title.x = element_blank())
ggplot(results.los, aes(agerace, fill=ClassLabel)) + geom_bar(position="dodge")