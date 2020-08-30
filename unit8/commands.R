arsim<-function(phis, sigsq, T){
p<-length(phis) #find the number of lags in our AR
noise<-rnorm(T+p, sd=sqrt(sigsq)) #generate the white noise plus a few to get started
x<-c(noise[1:p],rep(0,T)) #put the initial noise terms in and set the rest to zero
for (i in (p+1):(T+p)){ #this loop generates the AR series with the recursive formula
x[i]<-phis %*% x[i-(1:p)] +noise[i]
}
x<-x[(p+1):(T+p)] #throw away those initial starting points
x #return
}

x1<-arsim(c(0.5), 1,200)

pdf("ar1s.pdf")
par(mfrow=c(1,2))
plot.ts(x1, ylab="x", main="Time Series Plot of AR(1), phi=0.5")
acf(x1, main="ACF Plot of AR(1), phi=0.5")
dev.off()

x2<-arsim(c(1.1), 1,200)

pdf("ar1ns.pdf")
par(mfrow=c(1,2))
plot.ts(x2, ylab="x", main="Time Series Plot of AR(1), phi=1.1")
acf(x2, main="ACF Plot of AR(1), phi=1.1")
dev.off()

