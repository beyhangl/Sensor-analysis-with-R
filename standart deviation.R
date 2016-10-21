analyzeData=read.csv('/root/Scripts/semana9.csv',header=T,sep=",")

colnames(analyzeData)=c("customerid","location","sublokasyon","fkdevice" ,"year" ,"group","created","electricity")

analyzeData[,"created"]=as.character(analyzeData[,"created"])

#lag function

lagpad <- function(x, k) 
{
  c(rep(NA, k), x)[1 : length(x)]
}

analyzeData=analyzeData[order(analyzeData$created,analyzeData$fkdevice),]

sd=c()

#lag data
analyzeData[,"compareValue"]=lagpad(analyzeData$electricity, 1440)

#remove nan values
analyzeData=analyzeData[!is.na(analyzeData$compareValue),]

as.matrix(sapply(analyzeData, as.numeric))

for(i in (1:nrow(analyzeData))) #nrow-->>satır sayısı .ncol'da kolon sayısı 
{
  sd_row=sd(analyzeData[i,8:dim(analyzeData)[2]])
  sd=c(sd,sd_row)
}

analyzeData[,"sd"]=sd

write.csv(analyzeData,"/enbead-api/Analyze/R/elec_Daily_sd.csv",row.names=F)

