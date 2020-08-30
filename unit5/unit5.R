##Read data in and only use for the years 1900 to 1997

globtemp<-scan("global.txt")
x<-globtemp[45:142]
t<- 1900:1997

#########################################################
##Fit the temperature against time using OLS to detrend##
#########################################################

fit<-lm(x~t)

##plot of data
plot(t,x, type="o", ylab="global temp", xlab="year", main="Global Temp")

##acf plot of data
acf(x, main="ACF for Global Temp")

##residual plot
plot(t,fit$res, type="o", ylab="detrended global temp", xlab="year", main="Detrended Data")

##acf plot of residuals, i.e. after detrending
acf(fit$res, main="ACF for Detrended Data")

####################################################################
##difference the data in attempt to make resulting data stationary##
####################################################################

y<-globtemp[44:142] ##note I start from index 44 instead of 45 here. Why?
##plot of differenced data
plot(t,diff(y), type="o", ylab="differenced global temp", xlab="year", main="Differenced Data")

##acf plot of differenced data
acf(diff(y), main="ACF for Differenced Data")

mean(diff(y)) ##estimate for delta
sd(diff(y)) ##estimate for sigma_w

sd(diff(y))/sqrt(length(diff(y)))

t<-mean(diff(y))/(sd(diff(y))/sqrt(length(diff(y))))
t

