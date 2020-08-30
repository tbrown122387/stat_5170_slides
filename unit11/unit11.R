##download the file armasim.R, which contains a function to simulate ARMA processes

source("armasim.R")

##simulate ar(2)
path.ar<-armasim(c(1.5,-0.75),0,1,1000)

##simulate ma(2)
path.ma<-armasim(0,c(-1.5,0.75),1,1000)

##simulate arma(2,2)
path.arma<-armasim(c(1.5,-0.75),c(0.8,0.5),1,1000)

##acf2 function comes from astsa package, so does the time series rec
library(astsa)

##generate acf and pacf plots of simulated data
acf2(path.ar, 20, main="AR(2)")

acf2(path.ma, 20, main="MA(2)")

acf2(path.arma, 20, main="ARMA(2,2)")

##generate acf and pacf plot for recruit data
##Description of rec time series: Recruitment (index of the number of new fish) 
##for a period of 453 months ranging over the years 1950-1987. Recruitment is 
##loosely defined as an indicator of new members of a population to the
##first life stage at which natural mortality stabilizes near adult levels.

acf2(rec, 20, main="Recruitment Series")

