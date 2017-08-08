glob<-read.csv("/Users/Dhan/Desktop/GlobalTemperatures1.csv")
#read file from local directory
dim(glob)
globa<-ts(glob, start = c(1904), frequency =1)
dim(globa)
plot(globa[,2])
names(glob)
x<-globa[,2]
#n<-length(x)
#x<-x[n:1]
ts.plot(x)
#par(mfrow=c(1,2))


# auto co relation funtion
acf(x)
# partial auto corelation for the given data 
pacf(x)

# calclutation ARIMa with maximum likelihood 
arima.x<-arima(x,order=c(4,0,0), method = "ML")
print(summary(arima.x))
arima.x
arima.x1<-arima(x,order=c(4,0,2), method = "ML")
arima.x1
tsdiag(arima.x1)
tsdiag(arima.x)
xd<-diff(x)

arima.xd<-arima(xd,order = c(4,0,0))
arima.xd
tsdiag(arima.xd)

xx<-auto.arima(x)
xx
tsdiag(xx)
order.arima <- matrix(c(1, 0, 0,
                        1, 1, 0,
                        0, 0, 1,
                        0, 1, 1,
                        2, 0, 0,
                        2, 1, 0,
                        0, 0, 2,
                        0, 1, 2,
                        1, 0, 1,
                        1, 1, 1,
                        2, 0, 1, 
                        2, 1, 1,
                        1, 0, 2, 
                        1, 1, 2, 
                        3, 0, 0,
                        3, 1, 0,
                        0, 0, 3, 
                        0, 1, 3,
                        4, 0, 0,
                        4, 1, 0, 
                        0, 0, 4,
                        0, 1, 4,
                        2, 0, 2, 
                        2, 1, 2, 
                        3, 0, 1,
                        3, 1, 1,
                        1 ,0, 3,
                        1 ,1, 3,
                        5, 0, 0,
                        5, 1, 0,
                        0, 0, 5,
                        0, 1, 5,
                        1, 0, 4, 
                        1, 1, 4,
                        4, 0, 1, 
                        4, 1, 1,
                        2, 0, 3,
                        2, 1, 3,
                        3, 0, 2,
                        3, 1, 2,
                        6, 0, 0,
                        6, 1, 0,
                        0, 0, 6, 
                        0, 1, 6,
                        5, 0, 1,
                        5, 1, 1,
                        1, 0, 5,
                        1, 1, 5,
                        4, 0, 2,
                        4, 1, 2,
                        2, 0, 4,
                        2, 1, 4,
                        3, 0, 3,
                        3, 1, 3),
                      ncol=3, 
                      byrow=T)

complexity <- numeric(nrow(order.arima))
#x <- ((x[length(x):1])^(1/1))
# Checking the BIC value 

for(j in 1:nrow(order.arima))
{
  p <- order.arima[j,1]
  d <- order.arima[j,2]
  q <- order.arima[j,3]
  arima.x <- arima(x, order=c(p,d,q))
  complexity[j] <- BIC(arima.x)
}   


# storing the results in a data frame 
results           <- data.frame(order.arima, complexity, rowSums(order.arima))
colnames(results) <- c('p','d','q', 'BIC', 'size')
print(results)
mod.code <- NULL
for(j in 1:nrow(order.arima))
{
  mod.code <- c(mod.code, paste(order.arima[j,1],order.arima[j,2], order.arima[j,3], sep=''))
} 

#x11()
#par(mfrow=c(1,2))

plot(results[,5], results[,4])
text(results[,5], results[,4], labels=mod.code, pos=4)


BIC <- numeric(7)



# Building the forecasting model below 

for(l in 1:7)
{ 
  BIC[l]<-min(complexity[which((order.arima[,1]+order.arima[,2]+order.arima[,3])==l)])
}
plot(1:7, BIC, type='b', col='red')

best <- min(which(results[,4]==min(results[,4])))
best
library(forecast)
arima.x <- Arima(x, order=order.arima[best, ])
#x11()
ts.plot(x, col='green', lty=1)
lines(fitted(arima.x), col='red', lty=2)
legend('top', c('original series','estimated series'), lty=c(1,2), col=c('green','red'), inset=0.02)

#x11()
plot(forecast(Arima(x, order=order.arima[best, ])))
plot(forecast(Arima(x, order=c(1,1,2))))

#x11()
pred <- predict(arima.x, n.ahead = 10)
pred
plot(pred)
plot(x,type='l',xlim=c(2015,2025),ylim=c(8,10),xlab = 'Year',ylab = 'Average Temperature')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')
ts.plot(x, pred$pred, lty = c(1,2), col=c('green','red'))
legend('bottom',c('Given','Predicted'),lty = c(1,2),col=c('green','red'))

tsdiag(Arima(x, order=order.arima[best, ]))
fit<-nnetar(x)
fcast<-forecast(fit)
plot(fcast)
fcast$residuals
predi<-predict(fit)
predi
ts.plot(x,predi$mean,lty=c(1,2))

hw<-HoltWinters(x, beta = FALSE, gamma=FALSE)
hw
hw$SSE
plot(hw, lty=c(1,2),col=c('green','red'), inset=0.03)
fcast<-forecast.HoltWinters(hw, h=10)
fpred<-predict(hw,n.ahead = 10)
plot(hw,predicted.values=fpred)
plot.forecast(fcast)
fcast$residuals
plot.ts(fcast$residuals)
fres<-na.omit(fcast$residuals)
acf(fres)
Box.test(fres, lag=20, type="Ljung-Box")

hw$SSE
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="green", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="red", lwd=2)
}
plotForecastErrors(fres)



fit <- tbats(x)
seasonal <- !is.null(fit$seasonal)
seasonal
