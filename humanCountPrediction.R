analyzeData<-read.csv('/home/beyhan/lib/humanpass.csv',header=T,sep=",")
#read

analyzeData<-analyzeData[,1:9]
head(analyzeData)

#give column names

colnames(analyzeData)=c("customerid","location","sublokasyon","fkdevice","year","month","created","group","human")

#except some column
analyzeData=analyzeData[ , -which(names(analyzeData) %in% c("year","month","customerid","location","sublokasyon","group"))]
analyzeData=analyzeData[order(analyzeData$created),]

#based on one fkdevice
fkdev<-unique(analyzeData$fkdevice)

analyzeData[,"created"]=as.character(analyzeData[,"created"])
analyzeData$created=strtrim(c(analyzeData$created), c(16))
analyzeData$created <- as.POSIXct(analyzeData$created , "%Y-%m-%d %h:%m:%s")
withouthour <- as.POSIXlt(analyzeData$created,format="%Y-%m-%d")

#take hour format
withhour=data.frame(
  time=format(analyzeData$created, "%H:%M")
)

#dayofweek
dayofweek=withouthour$wday

#hourofday
hourofday=substr(withhour$time,0, 2)

#identify new columns to main data frame
analyzeData$hourofday=hourofday
analyzeData$dayofweek=dayofweek


humanpred<-analyzeData


cihazlar<-unique(humanpred$fkdevice)
saatler<-unique(humanpred$hourofday)
haftagunleri<-unique(humanpred$dayofweek)


humanpred[with(humanpred, order(humanpred$hourofday,humanpred$dayofweek)), ]
fkdevice=c()
dayofwek=c()
hourofdayy=c()
myprediction=c()



library("forecast")
for (cihaz in cihazlar) {
  for (hafta in haftagunleri ) {
    for (saat in saatler) {
      analiz <- subset(humanpred, humanpred$fkdevice==cihaz)
      analiz <- subset(analiz, analiz$dayofweek==hafta)
      analiz <- subset(analiz, analiz$hourofday==saat)
      
      if(nrow(analiz)>2)
      {
        rainseries <- ts(analiz$human,start=c(1813))
        
        rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
        HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=NULL)
        Rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=1)
        fkdevice=c(fkdevice,cihaz)
        dayofwek=c(dayofwek,hafta)
        hourofdayy=c(hourofdayy,saat)
        myprediction=c(myprediction,Rainseriesforecasts2$mean)
      }
    }
  }
}


df <- data.frame(fkdevice,dayofwek,hourofdayy,myprediction)

write.csv(df,"/home/beyhan/pozi/R/humanPred.csv",row.names=F)




is.null(analiz$human)

