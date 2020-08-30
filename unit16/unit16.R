library(astsa)

beer<-scan("beer.txt")

plot.ts(beer, type="b", main="Beer Production in Australia")

##look at series after removing a linear trend, and after removing a seasonal component
par(mfrow=c(1,2))
plot.ts(diff(beer,1), main="First Difference (Non-Seasonal)")
plot.ts(diff(beer, lag=4), main="Difference with lag 4 (Seasonal)")


##look at series after removing linear trend and seasonal component
plot.ts(diff(diff(beer),lag=4), main="Time Series Plot with both non-seasonal and seasonal diff")

##acf and pacf plot of series to decide on p, q, P, Q for SARIMA model
acf2(diff(diff(beer),lag=4),20, main="ACF/PACF with both differencing")

fit1<-sarima(beer,2,1,0,0,1,1,4) ## sig acf at lag 9

fit2<-sarima(beer,0,1,1,0,1,1,4) ##sig acf at lag 1. pvalue not good

fit3<-sarima(beer,1,1,1,0,1,1,4) ##ok

##check estimates
fit3



