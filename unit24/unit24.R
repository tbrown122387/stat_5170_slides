library(astsa) 
library(TSA) ##For prewhiten() function. This comes from another popular time series textbook

##CCF of SOI & recruit after prewhitening. This takes care of steps 1, 2, 3.
prewhiten(ts(soi),ts(rec), main="CCF of SOI & Recruit")

##create a new object by putting recruit, lagged recruit (1), and lagged soi (5) in three columns
data<-cbind(rec, lag(rec,-1), lag(soi,-5))

##Step 4. Fit OLS based on CCF plot
result<-lm(data[,1]~data[,2]+data[,3])

##Step 5 examine residuals from step 4 to determine ARMA structure for noise term
acf2(result$residuals, main="Residuals in Step 4") ##looks like AR(1)

##Fit lagged regression model, plus ARMA noise
sarima(data[,1], 1, 0, 0, xreg=data[,2:3])
##notice ACF of residuals are not white. Ljung Box statistic significant also. Diagnostics fail. Try other ARMA.

##After some trial an error, an ARMA(3,5) seems to work
sarima(data[,1], 3, 0, 5, xreg=data[,2:3])


