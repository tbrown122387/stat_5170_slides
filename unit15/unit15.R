library(astsa)

##simulate seasonal ma1
seasonal.ma1.sim <- arima.sim(list(order = c(0,0,12), ma = c(rep(0,11),0.5)), n = 1000)

##simulate seasonal ar1
seasonal.ar1.sim <- arima.sim(list(order = c(12,0,0), ar = c(rep(0,11),0.5)), n = 1000)

##acf and pacf plots of seasonal ma1
acf2(seasonal.ma1.sim, 50, main="ACF of Seasonal MA(1)")

##acf and pacf plots of seasonal ar1
acf2(seasonal.ar1.sim, 50, main="ACF of Seasonal AR(1)")

##simulate arma(0,1)x(1,0)_12
phi<-c(rep(0,11),0.8)
ACF<-ARMAacf(ar=phi, ma=-0.5, 50)[-1]
PACF<-ARMAacf(ar=phi, ma=-0.5, 50, pacf=TRUE)

##acf and pacf plots of arma(0,1)x(1,0)_12
par(mfrow=c(1,2))
plot(ACF, type="h", xlab="lag", ylim=c(-0.4,0.8), main=expression("ACF of ARMA(0,1)x(1,0)"[12]))
plot(PACF, type="h", xlab="lag", ylim=c(-0.4,0.8), main=expression("PACF of ARMA(0,1)x(1,0)"[12]))


