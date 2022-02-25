#last updated 08/03/2021 at 6:59pm by Jessica Romero

#plot for theory
#look at how increasing d affects ACF
h = seq(0, 1000, 1)

par(mfrow=c(1,4))
d = .001
plot(h, h^(2*d-1), type="l", ylab= "ACF", main="d = 0.001", ylim=c(0,1), col="#2471A3",lwd=3, cex.main=2, cex.axis=2, cex.lab = 1.5)

d = .20
plot(h, h^(2*d-1), type="l", ylab=" ", ylim=c(0,1), main= "d = 0.20", col="#2471A3", lwd=3, cex.main=2, cex.axis=2, cex.lab = 1.5)

d = .40
plot(h, h^(2*d-1), type="l", ylab=" ", ylim=c(0,1), main= "d = 0.40", col="#2471A3", lwd=3, cex.main=2, cex.axis=2, cex.lab = 1.5)

d = .499
plot(h, h^(2*d-1), type="l", ylab= "", ylim=c(0,1), main= "d = 0.499", col="#2471A3", lwd=3, cex.main=2, cex.axis=2, cex.lab = 1.5)

#code for warmup plot
library(astsa)
par(mfrow=c(1,2))
acf(log(varve), 100, main = "ACF of log(varve) (Figure 5.1)")
acf(gnp, lag = 50, main = "ACF of GNP (Figure 3.13)")



# Code for creating plots for  DJI example
fulldata= read.csv("dji.csv",h=TRUE)
length(fulldata[,6])

stocks <- ts(data = fulldata, start = 2010+5/12+7/30, frequency = 255)
ts.plot(stocks[,6],ylab="Trade Volume", xlab="Year", main = "Time Series Plot of DJI Volume")
abline(v=c(2017,2019+9.25/12),col = 'red')


#Romero did this to see what effect a difference of 1 had on our data
#Sara had done the same thing. This just helped JR see it. 
#take a full difference of 1 to see what happens
# d.1 = diff(fulldata$Volume, differences=1)
# par(mfrow=c(2,3))
# ts.plot(fulldata$Volume, ylab ="d = 0")
# acf(fulldata$Volume)
# pacf(fulldata$Volume)
# ts.plot(d.1, ylab="d = 1")
# acf(d.1)
# pacf(d.1)


acf(fulldata[,6],main="ACF of DJI Volume")
acf(stocks[,6],main="ACF of DJI Volume")

par(mfrow=c(1,2))
acf(diff(fulldata[,6]), main="ACF of 1st differenced of DJI volume") #Regular differencing (d=1) 
pacf(diff(fulldata[,6]), main="PACF of 1st differenced of DJI volume")


#install.packages("fracdiff")
library(fracdiff)
aa= fulldata[,6] 
bb=fracdiff(aa)
data.fd = diffseries(aa,bb$d)
bb$d
#[1] 0.4273039 # estimate of d based on full data

library(astsa)
par(mfrow=c(2,1))

acf2(data.fd, main="ACF and PACF of May 2010 - July 2021")


dev.off()
DD1=fracdiff(aa, nar=1, nma=1) 

DD2=fracdiff(aa, nar=0, nma=1)

par(mfrow=c(2,1))
acf(DD1$resid, main="ACF of ARMA(1,1) fitted model")
acf(DD2$resid, main="ACF of MA(1) fitted model")


#########
aaa=aa[1:1625]
bb=fracdiff(aaa)

data.fd = diffseries(aaa,bb$d)
bb$d
acf2(data.fd)

#residuals
plot(bb$residuals)
qqnorm(bb$residuals)

DD5=fracdiff(aaa, nar=1, nma=1) 
DD6=fracdiff(aaa, nar=0, nma=1)

par(mfrow=c(2,1))
acf(DD5$resid)
acf(DD6$resid)

par(mfrow=c(3,1))
acf(aa, main="ACF of May 2010 - July 2021 (the whole series)") 
acf(aa[1:2430], main="ACF of Pre-COVID19 data") 
acf(aa[1:1625], main="ACF of data prior to 2017")


