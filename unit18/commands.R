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

##MA(1) with theta=0.5

power<-power.ma1(0.5,1,frequency)

pdf("ma1_1power.pdf")
plot(frequency,power, type="l", main="Power spectrum of MA(1) with theta=0.5")
dev.off()

x2<-masim(c(0.5),1,10000)

pdf("ma1_1ts.pdf")
plot.ts(x2[1:50], main="MA(1) with theta=0.5")
dev.off()

##MA(1) with theta=-0.5

power<-power.ma1(-0.5,1,frequency)

pdf("ma1_2power.pdf")
plot(frequency,power, type="l", main="Power spectrum of MA(1) with theta=-0.5")
dev.off()

x3<-masim(c(-0.5),1,10000)

pdf("ma1_2ts.pdf")
plot.ts(x3[1:50], main="MA(1) with theta=-0.5")
dev.off()