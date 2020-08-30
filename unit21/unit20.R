library(astsa) ##needed for mvspec function to produce periodogram

##time series plots of SOI and Recruit data
par(mfrow=c(2,1))
plot.ts(soi, main="Time Series Plot of SOI")
plot.ts(rec, main="Time Series Plot of Recruitment")

##periodogram of SOI and Recruit data
par(mfrow=c(2,1))
soi.per<-mvspec(soi, log="no")
rec.per<-mvspec(rec, log="no")
##note the horizontal axis is in terms of multiples of 1/12. So when the value on the x-axis is 1, it means omega is 1/12

##overlay vertical lines at omega = 1/48 and 1/12
par(mfrow=c(2,1))
soi.per<-mvspec(soi, log="no")
abline(v=1, col="red") ##red vertical line at omega = 1/12
abline(v=1/4, col="blue") ##blue vertical line at omega = 1/4 * 1/12 = 1/48

rec.per<-mvspec(rec, log="no")
abline(v=1, col="red") ##red vertical line at omega = 1/12
abline(v=1/4, col="blue") ##blue vertical line at omega = 1/48

n<-length(rec) ##number of observations
nextn(n) ##see number of observations with padding. Padding is used in the Fast Fourier transform algorithm which speeds up the calculation of the Discrete Fourier transform.

soi.per$spec[40] ##since nextn(n) is 480, the 40th value on the periodogram corresponds to a frequency of 40/480 or 1/12
soi.per$spec[10] ##the 10th value on the periodogram corresponds to a frequency of 10/480, or 1/48.


##quantiles of chi square distribution for 95% confidence
upper<-qchisq(0.025,2) 
lower<-qchisq(0.975,2)

##95% CI for f(omega) when omega is 1/48 for SOI data
c(2*soi.per$spec[10]/lower, 2*soi.per$spec[10]/upper)
##95% CI for f(omega) when omega is 1/12 for SOI data
c(2*soi.per$spec[40]/lower, 2*soi.per$spec[40]/upper)



