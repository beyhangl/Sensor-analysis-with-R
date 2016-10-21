energy=read.csv('/home/beyhan/pozi/R/Datasets/electricity.csv',header=T,sep=",")


cnames=c("custom","lok","kat","fk","year","created","temperature")
names(energy)=make.names(cnames)
energy <- subset(energy, energy$fk=='972973')
energy
head(energy)
#part 1 --data preparation

#plot(energy$created,energy$temperature,type = "l",xlab="Günler",ylab="Sıcaklık vs CO2",col="red",lwd=4)

indexRow=which(energy[,1]=="created")
energy[,c(1:7)]=sapply(energy[,c(1:7)],as.character)

energy[,c(1:7)]=sapply(energy[,c(1:7)],as.numeric)
d <- as.POSIXct(energy$created , "%Y-%m-%d %h:%m:%s")

year = as.numeric(format(d, format = "%Y"))
month = as.numeric(format(d, format = "%m"))

day = as.numeric(format(d, format = "%d"))
hour = as.numeric(format(d, format = "%H"))
minute = as.numeric(format(d, format = "%M"))
second = as.numeric(format(d, format = "%S"))
energy=cbind(energy,year,month,day,hour,minute,second,d)
s=energy[order(d),]
head(s)

library(AnomalyDetection)
d

anomalousDataset=data.frame(s$d,s$temperature)
anomalousDataset
names(anomalousDataset)=c("timestamp","count")

anomalous=AnomalyDetectionTs(anomalousDataset, max_anoms=0.1, direction='both', plot=TRUE)

anomalous$plot
