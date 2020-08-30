mort<-scan("cmort.dat")

plot(mort,xlab="Week",ylab="Mortality", main="Cardiovascular Mortality in LA")

##################
##moving average##
##################

plot(mort,xlab="Week",ylab="Mortality", main="Cardiovascular Mortality in LA, with Moving Average Filter")
ma4<-filter(mort,sides=2,c(0.5, rep(1,3), 0.5)/4)
ma52<-filter(mort,sides=2,c(0.5, rep(1,51), 0.5)/52)
lines(ma4, col="red")
lines(ma52, col="blue")
legend("topright", c("monthly","yearly"), lty=c(1,1), col=c("red","blue"))

#########################################
##kernal smoothing, normal distribution##
#########################################

t<-1:length(mort)
plot(mort,xlab="Week",ylab="Mortality", main="Cardiovascular Mortality in LA, with Normal Kernel Smoothing")
lines(ksmooth(t,mort, "normal", bandwidth=10), col="red")
lines(ksmooth(t,mort, "normal", bandwidth=104), col="blue")
legend("topright", c("b=10","b=104"), lty=c(1,1), col=c("red","blue"))

##########################################
##kernal smoothing, uniform distribution##
##########################################

t<-1:length(mort)
plot(mort,xlab="Week",ylab="Mortality", main="Cardiovascular Mortality in LA, with Uniform Kernel Smoothing")
lines(ksmooth(t,mort, "box", bandwidth=10), col="red")
lines(ksmooth(t,mort, "box", bandwidth=104), col="blue")
legend("topright", c("b=10","b=104"), lty=c(1,1), col=c("red","blue"))

################################
##nearest neighbors regression##
################################

plot(mort,xlab="Week",ylab="Mortality", main="Cardiovascular Mortality in LA, with Nearest Neighbor")
lines(lowess(t,mort,f=0.05),col="red")
lines(lowess(t,mort,f=0.20),col="blue")
legend("topright", c("f=0.05","f=0.20"), lty=c(1,1), col=c("red","blue"))

