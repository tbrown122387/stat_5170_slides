library(astsa) ##for gnp dataset, acf2 function, and sarima function

##generate time series and acf plot
par(mfrow=c(1,2))
plot(gnp, main="US GNP")
acf(gnp, main="ACF of US GNP")

##transform the data so we look at growth rate
d.log.gnp<-diff(log(gnp))

##generate time series and acf plot of growth rate
plot(d.log.gnp, main="Growth Rate of US GNP")
acf(d.log.gnp, main="Growth Rate of US GNP")

##generate acf and pacf plot of growth rate
acf2(d.log.gnp, 20, xlim=c(1,20), ylim=c(-0.2,0.4), main="ACF/PACF of Growth Rate")

##fit an AR(1) to growth rate
mod.ar1<-sarima(d.log.gnp,1,0,0)

mod.ar1 ##obtain estimated coefficients

##fit an MA(2) to growth rate
mod.ma2<-sarima(d.log.gnp,0,0,2)

mod.ma2 ##obtain estimated coefficients

##fit an ARMA(2,2) to growth rate
mod.arma22<-sarima(d.log.gnp,2,0,2)

mod.arma22 ##obtain estimated coefficients

