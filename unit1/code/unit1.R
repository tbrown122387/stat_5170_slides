##################################
##Monthly Sales at Souvenir Shop##
##################################

shop<-scan("souvenir.csv")
sales<-ts(shop)

##time series plot of sales
plot(sales)

##consider a regression of sales against index for month

time<-seq(1,84,1)

result<-lm(shop~time)

##overlay regression line over the plot of sales
plot(shop~time, ylab="sales", main="Regress sales on time index")
abline(result, col="red")


#################
##Munich summer##
#################

summer<-scan("summer.dat", skip=1) ##skip the first line which is some description
summer<-ts(summer, start=1781) ##start tells what is the starting point

plot(summer)

##acf plot of data
acf(summer)

#######################
##Dubuque temperature##
#######################

dubuque<-scan("dubuque.dat", skip=2)

dubuque<-ts(dubuque, start=1964, frequency=12)

plot(dubuque,ylab="Temp (F)")

############
##Marriage##
############

marriage<-scan("yule2.dat",skip=12)
marriage<-ts(marriage, start=1866)


plot(marriage, ylab="Percent")

##consider regression of marriages against an index of time
time<-1866:(1865+length(marriage))

fit<-lm(marriage ~ time)

##ACF plot of residuals
acf(fit$resid)


#############
##IBM stock##
#############

ibm<-scan("dailyibm.dat", skip=1)
ibm<-ts(ibm)

par(mfrow=c(1,2))
plot(ibm, main="Daily closing price", ylab="Price in dollars")

##transform the data
ibmreturn<-diff(log(ibm))
plot(ibmreturn, main="Daily returns", ylab="Returns")



