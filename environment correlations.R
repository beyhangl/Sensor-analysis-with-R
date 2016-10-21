analyzeData=read.csv('/home/beyhan/pozi/R/Datasets/analyze.csv',header=T,sep=",")
#datayı okuyorum
colnames(analyzeData)=c("lux","ppm","pressure","fkdevice","temperature","humidity","created")
#tarihi +000 ları sildim
analyzeData[,"created"]=as.character(analyzeData[,"created"])
analyzeData$created=strtrim(c(analyzeData$created), c(16))
#R tarih olarak görüyor.
#analyzeData$created <- as.POSIXct(analyzeData$created , "%Y-%m-%d %h:%m:%s")
#başlataki datalar kötü olduğu için bazılarını almadım
#startDate = as.POSIXct("2016-03-26 00:00:00");
#tarih bazlı sıralıyoruz
#analyzeData=analyzeData[order(analyzeData$created,analyzeData$fkdevice),]

#cihaz bazlı sıralıyoruz
analyzeData=analyzeData[order(analyzeData$fkdevice),]
#korelasyona bakmak için matris şeklinde görüyorum
as.matrix(sapply(analyzeData, as.numeric))
#0 olanları ve seçtiğim tarih içerisinde olanları almadım.
analyzeData<-analyzeData[!(analyzeData$ppm=="0"),]
analyzeData<-analyzeData[!(analyzeData$created<startDate),]
#sadece unique dataları alıyoruz ve dizi şeklinde tuttum.
devices=unique(analyzeData$fkdevice)


#sonraki yere -1 eklemek için fonksiyon
insertRowToDF<-function(X,index_after,vector_to_insert){
  stopifnot(length(vector_to_insert) == ncol(X));
  X<-rbind(X[1:index_after,],vector_to_insert,X[(index_after+1):nrow(X),]);
  row.names(X)<-1:nrow(X);
  return (X);
}


before=analyzeData$fkdevice[1]
#device değiştikten sonra -1 yazıyor.
for(r  in 1:nrow(analyzeData))
   {
    if(before==analyzeData$fkdevice[r])
    { before=analyzeData$fkdevice[r]}
      else if (analyzeData$fkdevice[r]!='-1')
          {
            analyzeData<-insertRowToDF(analyzeData,r-1,c(-1,-1,-1,-1,-1,-1,-1));
            before=analyzeData$fkdevice[r+1]
           }
}
dev_cor=c()
cor=c()
merge=c()

for(each_dev in devices)
{
    mydata <- subset(analyzeData, analyzeData$fkdevice==each_dev)
    if(nrow(mydata)>0)
    {
      
      correlation=cor(mydata$temperature, mydata$humidity)
      dev_cor=c(dev_cor,each_dev)
      cor=c(cor,correlation)
    }
}

cor_result = data.frame(FKDevice=dev_cor, Correlation=cor)


cor_result

#plot(x$FKDevice,x$Correlation)
#lines(lowess(analyzeData$created,analyzeData$temperature),col="blue")
#plot(analyzeData$created,analyzeData$temperature)
#lines(lowess(analyzeData$created,analyzeData$temperature),col="red")
#plot(analyzeData$temperature ~ analyzeData$created, analyzeData)
#require(ggplot2)
#ggplot( data = analyzeData, aes( created,temperature  )) + geom_line() 

#x <- cbind(analyzeData$humidity,analyzeData$pressure,analyzeData$temperature,analyzeData$lux)
#x
#y <- cbind(analyzeData$ppm,analyzeData$ppm,analyzeData$ppm,analyzeData$ppm)
#y
#plot(x,y)


#temizlenmiş datayı yazdırma
write.csv(cor_result,"/home/beyhan/pozi/R/CorrelationsOFFKDevices.csv",row.names=F)
  

