library(RJDBC)
library(DBI)
library(rJava)
library(RJDBC)
library(RCassandra)

connect.handle<-RC.connect(host = "127.0.0.1",port=9160)


RC.cluster.name(connect.handle)
RC.describe.keyspaces(connect.handle,'enbead_test')
RC.use(connect.handle, "enbead_test", cache.def = TRUE)
RC.get(c, "liste", "3", c("tc","ad", "soyad"))

RC.get.range(c, "temperature", "1")

cars_slice <- RC.get.range.slices(connect.handle, "temperature")



mycars <- RC.read.table(connect.handle, "temperature")
head(mycars)

write.csv(mycars,"/home/beyhan/pozi/R/test.csv",row.names=F)


library(datasets)
head(mtcars, 3)


library(datasets)
head(mtcars, 3)
RC.use(connect.handle, "enbead_test", cache.def = TRUE)
RC.write.table(connect.handle, "cars", mtcars)





