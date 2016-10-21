ppmdata=read.csv('/home/beyhan/Downloads/ppm_hourly_stats.csv',header=T,sep=",")
tempdata=read.csv('/home/beyhan/Downloads/temperature_hourly_stats.csv',header=T,sep=",")
#datayı okuyorum
colnames(ppmdata)=c("customerid","location","year","sublokasyon","fkdevice","group","created","avg")
colnames(tempdata)=c("customerid","location","year","fkdevice","sublokasyon","group","created","avg")
#tarihi +000 ları sildim
ppmdata[,"created"]=as.character(ppmdata[,"created"])
ppmdata$created=strtrim(c(ppmdata$created), c(16))

tempdata[,"created"]=as.character(tempdata[,"created"])
tempdata$created=strtrim(c(tempdata$created), c(16))

ppmdata$created <- as.POSIXct(ppmdata$created , "%Y-%m-%d %h:%m:%s")
tempdata$created <- as.POSIXct(tempdata$created , "%Y-%m-%d %h:%m:%s")


ppmdata=ppmdata[order(ppmdata$created),]
tempdata=tempdata[order(tempdata$created),]


#0 olanları siliyoruz
ppmdata=ppmdata[ ! ppmdata$avg %in% c(0), ]
ppmdata <- subset(ppmdata, ppmdata$fkdevice=='15840379')


tempdata=tempdata[ ! tempdata$avg %in% c(0), ]

tempdata <- subset(tempdata, tempdata$fkdevice=='15840379')

total <- merge(ppmdata,tempdata,by=c("created","fkdevice"))


write.csv(ppmdata,"/home/beyhan/pozi/R/ppm_hourly.csv",row.names=F)
write.csv(tempdata,"/home/beyhan/pozi/R/temp_hourly.csv",row.names=F)
write.csv(total,"/home/beyhan/pozi/R/total.csv",row.names=F)


plot(tempdata$created,tempdata$avg,type = "l",xlab="Günler",ylab="Sıcaklık vs CO2",col="red",lwd=4)
par(new=T)
plot(ppmdata$created,ppmdata$avg,type = "l",col="green",xlab="",ylab="", side=4,lwd=4)
axis(4)
c("Sıcaklık","CO2") # puts text in the legend

lty=c(1,1) # gives the legend appropriate symbols (lines)

lwd=c(2.5,2.5)
col=c("red","green")










