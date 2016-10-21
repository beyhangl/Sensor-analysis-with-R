elec=read.csv('/home/beyhan/DataScience/alex/Bimbo/R/enbead_main.raw_electricity.csv',header=T,sep=",")

colnames(elec)=c("customerid","location","created","active","cos","fkdevice","hz","reactive","s")

kapalikutu <- subset(analyzeData, analyzeData$fkdevice=='14095312')

#kapalikutu <- subset(analyzeData, analyzeData$fkdevice=='15840379')
kapalikutu=kapalikutu[order(kapalikutu$created,kapalikutu$fkdevice),]

kapalikutu=kapalikutu[ , -which(names(kapalikutu) %in% c("fkdevice","created"))]



#korelasyonlar
library(corrplot)
kapalikutu
correlations <- cor(kapalikutu[,1:5])
correlations
corrplot(correlations, method="circle")


#yoğunluk(density)
par(mfrow=c(1,5))
for(i in 1:5) {
  plot(density(elec[,i]), main=names(elec)[i])
}


#histogram--kullanışsız
for(i in 1:5) {
  hist(elec[,i], main=names(elec)[i])
}
#scatter(dağılımları)
pairs(elec)










