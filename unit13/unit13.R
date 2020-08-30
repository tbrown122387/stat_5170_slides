######################
##simulation example##
######################

##download the file armasim.R, which contains a function to simulate ARMA processes

source("armasim.R")

##set seed so results are reproducible

set.seed(123)

##simulate ar(2) with specified values of parameters

path<-armasim(c(1.5,-0.75),0,1,1000)

##store the autocovariance values
gamma<-acf(path, type="covariance", plot=FALSE)

##store the autocorrelation values
rho<-acf(path, type="correlation", plot=FALSE)

##-Rhat matrix
Rhat<-matrix(c(1,rho$acf[2],rho$acf[2],1),c(2,2))

##rho-hat vector
rhohat<-matrix(c(rho$acf[2],rho$acf[3]),c(2,1))

##solve for phi-hat
phihat<-solve(Rhat)%*%rhohat
phihat

##solve for sigsq-hat
sigsqhat<-gamma$acf[1]*(1-t(rhohat)%*%phihat)
sigsqhat

##variance covariance matrix
varmat<-(1/1000)*as.numeric((sigsqhat/gamma$acf[1]))*solve(Rhat)

##95% CIs for phi1-hat, phi2-hat
c(phihat[1] - 1.96*sqrt(varmat[1,1]), phihat[1] + 1.96*sqrt(varmat[1,1]))
c(phihat[2] - 1.96*sqrt(varmat[1,1]), phihat[2] + 1.96*sqrt(varmat[1,1]))

###########
##recruit##
###########

library(astsa)

acf2(rec, 20, main="Recruit Data")
##looks like ar(2) is appropriate

##fit ar(2) to data
rec.yw<-ar.yw(rec, order=2)
rec.yw$x.mean ##find the mean of this ar(2)
rec.yw$ar ##find the estimated coefficients

sqrt(diag(rec.yw$asy.var.coef)) ##find the standard error for estimated coefficients

##predict 24 months ahead based on this ar(2) model
rec.pred <- predict(rec.yw, n.ahead=24)

##create plot of data, with 24 months ahead predictions and prediction intervals overlaid. 
ts.plot(rec, rec.pred$pred, col=1:2, main="Recruit Data with 24 Month Predictions")
lines(rec.pred$pred - rec.pred$s, col=4, lty=2)
lines(rec.pred$pred + rec.pred$s, col=4, lty=2)
