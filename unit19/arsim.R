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