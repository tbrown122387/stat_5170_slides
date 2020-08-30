library(astsa) ##for acf2 and sarima functions

data<-read.table("company.txt", header=FALSE,sep="")
colnames(data)<-c("company","industry")
attach(data)

##step 1: fit using OLS
result<-lm(company~industry)
summary(result)

res<-result$residuals

##examine residuals to see AR structure
plot(ts(res), ylab="Residuals", main="Plot of residuals against time") ##slight curve? maybe need quadratic term for predictor
acf2(res) ##ar1 seems reasonable

##fit regression with AR1 errors
sarima(company, 1, 0, 0, xreg=cbind(industry)) ##response, p, d, q, then a cbind for the predictors
##diagnostics are OK

##add quadratic term for predictor
industry2<-industry^2

##fit model with quadratic term
result2<-lm(company~industry+industry2)
summary(result2) ##quadratic term insignificant



