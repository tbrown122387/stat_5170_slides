armasim<-function(phis, thetas, sigsq, T){
p<-length(phis) 
q<-length(thetas)
m<-max(p,q)
noise<-rnorm(T+m, sd=sqrt(sigsq))
x<-c(noise[1:m],rep(0,T))
for (i in (m+1):(T+m)){
x[i]<-phis %*% x[i-(1:p)] + thetas %*% noise[i-(1:q)] + noise[i]
}
x<-x[(m+1):(T+m)]
x
}
