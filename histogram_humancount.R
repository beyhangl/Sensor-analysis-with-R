library(ggplot2)
library(easyGgplot2)
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
humann=read.csv('/home/beyhan/Downloads/enbead-api/Analyze/R/StandarDDeviation/humanpass.csv',header=T,sep=",")
colnames(humann)=c("customerid","location","sublokasyon","fkdevice","year","month","created","group","human")

#tarihi +000 ları sildim
humann[,"created"]=as.character(humann[,"created"])

humann$created=strtrim(c(humann$created), c(16))

earlier <- strptime("2016-04-18 17:00:00","%Y-%m-%d %H:%M:%S")

humann$created <- as.POSIXct(humann$created , "%Y-%m-%d %h:%m:%s")

humann <-subset(humann,humann$fkdevice=='8498152')

par(mfrow=c(10,1))
humannn<-subset(humann,humann$created>earlier)
earlier=earlier+minutes(15)
humannn<-subset(humannn,humannn$created<earlier)
dev.off()
hist(humannn$human)
humannn<-subset(humann,humann$created>earlier)
earlier=earlier+minutes(15)
humannn<-subset(humannn,humannn$created<earlier)
hist(humannn$human)
histogramList <- vector('list', 11)

col=c()
dev.off()
x11()
for (i in c(rep(1:10)))
{ humannn<-subset(humann,humann$created>earlier)
  earlier=earlier+minutes(15)
  humannn<-subset(humannn,humannn$created<earlier)
 
  Sys.sleep(0) 
  plot(humannn$human)
  
}
plot(humannn$human)


write.csv(humann,"/home/beyhan/pozi/R/tests.csv",row.names=F)

