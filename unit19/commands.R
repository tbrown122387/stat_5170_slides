source("arsim.R")
source("masim.R")

frequency<-seq(0,0.5,by=0.01)

power.ma1<-function(theta,sigsq,omega)

{

  power<-sigsq*(2*theta*cos(2*pi*omega) + 1 + theta^2)

}

power.ar1<-function(phi,sigsq,omega)

{

  power<-sigsq/(1 + phi^2 - 2 * phi * cos(2*pi*omega))

}

##all the examples have sigsq=1

######################
##AR(1) with phi=0.9##
######################

power<-power.ar1(0.9,1,frequency)

pdf("ar1_1power.pdf")
plot(frequency,power, type="l", main="Power spectrum of AR(1) with phi=0.9")
dev.off()

#######################
##AR(1) with phi=-0.9##
#######################

power<-power.ar1(-0.9,1,frequency)

pdf("ar1_2power.pdf")
plot(frequency,power, type="l", main="Power spectrum of AR(1) with phi=-0.9")
dev.off()

########################
##MA(1) with theta=0.5##
########################

power<-power.ma1(0.5,1,frequency)

pdf("ma1_1power.pdf")
plot(frequency,power, type="l", main="Power spectrum of MA(1) with theta=0.5")
dev.off()

#########################
##MA(1) with theta=-0.5##
#########################

power<-power.ma1(-0.5,1,frequency)

pdf("ma1_2power.pdf")
plot(frequency,power, type="l", main="Power spectrum of MA(1) with theta=-0.5")
dev.off()

##AR(2)##

power.ar2<-function(sigsq,omega)

{

  power<-1/(2.81 - 3.8*cos(2*pi*omega) + 1.8*cos(4*pi*omega))

}

power<-power.ar2(1,frequency)

pdf("ar2power.pdf")
plot(frequency, power, type="l", main="Power spectrum of AR(2) with phi = c(-1,0.9)")
dev.off()

