masim<-function(thetas, sigsq, T){
q<-length(thetas)
noise<-rnorm(T+q, sd=sqrt(sigsq))
x<-c(noise[1:q],rep(0,T))
for (i in (q+1):(T+q)){
x[i]<-thetas %*% noise[i-(1:q)] +noise[i]
}
x<-x[(q+1):(T+q)]
x
}