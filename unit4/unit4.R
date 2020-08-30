mortality<-scan("yule1.dat", skip=12) #ignore the first 12 rows
mortality<-ts(mortality, start=1866)
marriages<-scan("yule2.dat", skip=12)
marriages<-ts(marriages, start=1866)
time<-1866:1911

########################################
##Regression of marriages against time##
########################################

##make marriages plot
plot(marriages, ylab="% of marriages in Church", main="Plot of Marriages by Year")

#do regression for marriages
timefit<-lm(marriages~time)
summary(timefit)
AIC(timefit)

##store residuals and fitted values to create residual plot later
res<-timefit$residuals
yhat<-timefit$fitted.values

##create residual plot with loess curve
plot(yhat,res,main="Residuals against fitted values")
abline(h=0, col="red")
loess.fit<-loess(y~x, data.frame(x=yhat,y=res)) ##compute loess fit
yhat.grid<-seq(from=min(yhat),to=max(yhat),length=200) ##generate a sequence for fitted values, ranging from min to max, 200 points
tmp<-predict(loess.fit,newdata=data.frame(x=yhat.grid),
se=T) ##compute the predicted values from loess fit based on values of yhat above. se=T means to compute standard errors for loess fit.
lines(yhat.grid,tmp$fit,lwd=3, col="blue")
lines(yhat.grid,tmp$fit-2*tmp$se.fit, lwd=2, lty=2, col="blue")
lines(yhat.grid,tmp$fit+2*tmp$se.fit, lwd=2, lty=2, col="blue")

##################################################################
##Add squared term for time, based on the previous residual plot##
##################################################################

##consider adding squared term for time
timesq<-time^2

#do regression
timefitsq<-lm(marriages~time+timesq)
anova(timefitsq) 
summary(timefitsq)
AIC(timefitsq)

##residual plot for model with squared term

res<-timefitsq$residuals
yhat<-timefitsq$fitted.values

plot(yhat,res,main="Residuals against fitted values")
abline(h=0, col="red")
loess.fit<-loess(y~x, data.frame(x=yhat,y=res)) ##compute loess fit
yhat.grid<-seq(from=min(yhat),to=max(yhat),length=200) ##generate a sequence for fitted values, ranging from min to max, 200 points
tmp<-predict(loess.fit,newdata=data.frame(x=yhat.grid),
se=T) ##compute the predicted values from loess fit based on values of yhat above. se=T means to compute standard errors for loess fit.
lines(yhat.grid,tmp$fit,lwd=3, col="blue")
lines(yhat.grid,tmp$fit-2*tmp$se.fit, lwd=2, lty=2, col="blue")
lines(yhat.grid,tmp$fit+2*tmp$se.fit, lwd=2, lty=2, col="blue")

##acf plot of residuals
acf(residuals(timefitsq))

##qq plot of residuals
qqnorm(residuals(timefitsq))
qqline(residuals(timefitsq), col="red")

##################################################
##Regression of marriages against mortality rate##
##################################################

comparefit<-lm(marriages~mortality)
anova(comparefit)
summary(comparefit)
AIC(comparefit)

##residual plot

res<-comparefit$residuals
yhat<-comparefit$fitted.values

plot(yhat,res,main="Residuals against fitted values")
abline(h=0, col="red")
loess.fit<-loess(y~x, data.frame(x=yhat,y=res)) ##compute loess fit
yhat.grid<-seq(from=min(yhat),to=max(yhat),length=200) ##generate a sequence for fitted values, ranging from min to max, 200 points
tmp<-predict(loess.fit,newdata=data.frame(x=yhat.grid),
se=T) ##compute the predicted values from loess fit based on values of yhat above. se=T means to compute standard errors for loess fit.
lines(yhat.grid,tmp$fit,lwd=3, col="blue")
lines(yhat.grid,tmp$fit-2*tmp$se.fit, lwd=2, lty=2, col="blue")
lines(yhat.grid,tmp$fit+2*tmp$se.fit, lwd=2, lty=2, col="blue")

##acf plot
acf(residuals(comparefit))

##qq plot
qqnorm(residuals(comparefit))
qqline(residuals(comparefit), col="red")

#######################################
##Add time to the previous regression##
#######################################

comparetimefit<-lm(marriages~mortality+time)
anova(comparetimefit)
summary(comparetimefit)
AIC(comparetimefit)


