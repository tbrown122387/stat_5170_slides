##read in Australian unemployment data
auemp<-ts(scan("unemploy.dat", skip=1))

##plot of data
plot(auemp, ylab="Unemployment in thousands", main="Australian Unemployment Feb 1978-Aug 1995")

##difference data
dauemp<-ts(diff(auemp))

##plot differenced data
plot(dauemp, ylab="Differenced Unemployment in thousands", main="Australian Unemployment Feb 1978-Aug 1995 (Differenced)")

##acf plot of differenced data
acf(dauemp,50, main="ACF for Differenced Data")

##lag plot of differenced data
lag.plot(dauemp, lags=12, diag=F, main="Lag Plot for Differenced Data")

##periodogram of differenced data
temp<-spec.pgram(dauemp, taper=0, log="no")

##identify the frequencies with high values from the periodogram
freq<-temp$freq[temp$spec>5e9]
freq

##perform regression by setting up 5 periodic functions with the 5 frequencies found previously

t=1:length(dauemp)
c1<-cos(2*pi*t*freq[1])
s1<-sin(2*pi*t*freq[1])
c2<-cos(2*pi*t*freq[2])
s2<-sin(2*pi*t*freq[2])
c3<-cos(2*pi*t*freq[3])
s3<-sin(2*pi*t*freq[3])
c4<-cos(2*pi*t*freq[4])
s4<-sin(2*pi*t*freq[4])
c5<-cos(2*pi*t*freq[5])
s5<-sin(2*pi*t*freq[5])

##fit regression model with the periodic functions
fit<-lm(dauemp~c1+s1+c2+s2+c3+s3+c4+s4+c5+s5)

##obtain fitted values from regression
fitted<-ts(fit$coef[2]*c1+fit$coef[3]*s1+fit$coef[4]*c2+fit$coef[5]*s2+fit$coef[6]*c3+fit$coef[7]*s3+fit$coef[8]*c4+fit$coef[9]*s4+fit$coef[10]*c5+fit$coef[11]*s5)

par(mfrow=c(3,1))
##set limits for y-axis of plots
miny=min(dauemp)-100
maxy=max(dauemp)+100
##fitted values
plot(fitted, ylim=c(miny,maxy), main="Fitted Values")
##residual plot
plot(ts(fitted-dauemp), ylim=c(miny,maxy), main="Residuals")
##acf plot of residuals
acf(ts(fitted-dauemp), main="ACF for Residuals")














