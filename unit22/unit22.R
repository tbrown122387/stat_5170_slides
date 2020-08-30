###############################################
##power transfer function of first difference##
###############################################

omega<-seq(0,0.5, length=1000)

FR.diff<-abs(1-exp(2i*pi*omega))^2
plot(omega,FR.diff, type="l", main="Power Transfer Function of First Difference Filter")

#############################################
##power transfer function of moving average##
#############################################

FR.MA<-abs((1/12)*(1+cos(12*pi*omega)+2*cos(2*pi*omega)+2*cos(4*pi*omega)+2*cos(6*pi*omega)+2*cos(8*pi*omega)+2*cos(10*pi*omega)))^2
plot(omega,FR.MA, type="l", main="Power Transfer Function of Moving Average Filter")

#####################
##worked example II##
#####################

library(astsa)

##plot of SOI data, with first difference applied, with MA applied
par(mfrow=c(3,1))
plot.ts(soi, main="Time Series Plot of SOI")
plot.ts(diff(soi), main="First Difference Filter of SOI")
k<-kernel("modified.daniell", 6)
soif<-kernapply(soi,k)
plot.ts(soif, main="Moving Average Filter of SOI")

##produce plot of power transfer function with moving average, periodogram of SOI, periodogram of SOI with smoothing with moving average
par(mfrow=c(3,1))
FR.MA<-abs((1/12)*(1+cos(12*pi*omega)+2*cos(2*pi*omega)+2*cos(4*pi*omega)+2*cos(6*pi*omega)+2*cos(8*pi*omega)+2*cos(10*pi*omega)))^2
plot(omega,FR.MA, type="l", main="Power Transfer Function of Moving Average Filter")
mvspec(soi)
spectrum(soif,spans=9, log="no")
abline(v=12/52, lty=2)



