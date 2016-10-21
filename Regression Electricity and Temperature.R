X<-read.csv('/home/beyhan/Downloads/electricity.csv',header=T,sep=",")
y<-read.csv('/home/beyhan/lib/temperature.csv',header=T,sep=",")
colnames(X)=c("customerid","location","sublokasyon","fkdevice","year","month","created","elec")

colnames(y)=c("customerid","location","sublokasyon","fkdevice","year","created","group","temperature")

elan<-X[with(X, order(X$elec)), ]
tempa<-y[with(y, order(y$temperature)), ]

elan[,"created"]=as.character(elan[,"created"])
elan$created=strtrim(c(elan$created), c(16))
elan$created <- as.POSIXct(elan$created , "%Y-%m-%d %h:%m")
withouthour <- as.POSIXlt(elan$created,format="%Y-%m-%d")

tempa[,"created"]=as.character(tempa[,"created"])
tempa$created=strtrim(c(tempa$created), c(16))
tempa$created <- as.POSIXct(tempa$created , "%Y-%m-%d %h:%m")
withouthour <- as.POSIXlt(y$created,format="%Y-%m-%d")

merged <- merge(elan,tempa,by="created")

head(merged)
merged=merged[ , -which(names(merged) %in% c("year","customerid.x","location.x","sublokasyon.x","fkdevice.x","year.x","month","customerid.y","location.y","sublokasyon.y","fkdevice.y","group","year.y"))]
head(merged)

mydata<-merged[sample(nrow(merged), 500), ]

plot(mydata$elec,mydata$temperature)

x<-mydata$elec
y<-mydata$temperature

#random data ile
x <- runif(1000, -5, 5)
y <- x + rnorm(1000) + 3


# fit a linear model
res <- lm( y ~ x )

# plot the data and the model
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression')

abline(res, col='blue')

cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}
alpha <- 0.01
num_iters <- 1000

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

theta <- matrix(c(0,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))

#gradient
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}
#fonks.içi.
#tahmin etttiği x*theta dan asıl olanı çıakrtıyor error bu.sonra bunu gradient hesabına göre yeni theta değeri buluyor,ondan soonra onunla 
#çarpacakki bir sonrakinde daha düşük error alacak
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')

for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col=rgb(0.1,0,0,0.7))
}
abline(coef=theta, col="blue")

cost_history[seq(1,num_iters, by=100)]
plot(cost_history, type='l', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')



