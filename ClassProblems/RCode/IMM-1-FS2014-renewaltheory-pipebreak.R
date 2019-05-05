#This program is coded by Nam Lethanh @IBI, ETH Z?rich.
#it serves as learning material for the course infrastructure maintenance management (IMM), which is designed for master students taking their study in civil engineering and management.

#NOTE: BEFORE USING THIS CODE, YOU NEED TO INSTALL THE PACKAGE 'MASS' and 'survival' FROM CTAN LIBRARY.

#students take their own risk to use this code.

#STEP 1: ESTIMATE THE MODEL'S PARAMETER and Draw the graph

library(MASS)
library(survival)

data <- read.csv("IMM-1-FS2014-renewaltheory-pipebreak.csv",header=TRUE,sep=",") 
attach(data)
cat('Cast iron pipe - model parameters \n')
sr.fit_c = survreg(Surv(time_c,delta_c)~1,dist='weibull')
summary(sr.fit_c)
cat('Ductile iron pipe - model parameters \n')
sr.fit_d = survreg(Surv(time_d,delta_d)~1,dist='weibull')
summary(sr.fit_d)

YearMax=1000 # this is the time frame of investigation

cost_C=100000
cost_D=90000

plot.new()
#par(mar=c(5, 4, 4, 1) + 0.1)

x=seq(0,YearMax,length=200)
curve(pweibull(x, scale=exp(sr.fit_c$icoef[1]), shape=1/sr.fit_c$scale[1], lower.tail=FALSE), from=0, to=100, col='red', lwd=2, ylab='', xlab='',bty='n',ylim=c(0,1),axes=FALSE)
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext(expression(paste("Reliability")),side=2,col="black",line=2.2) 
axis(1,c(seq(1,YearMax)))
mtext(expression(paste("Time (years)")),side=1,col="black",line=2.2) 
x=seq(0,YearMax,length=200)
par(new=TRUE)
curve(pweibull(x, scale=exp(sr.fit_d$icoef[1]), shape=1/sr.fit_d$scale[1], lower.tail=FALSE), from=0, to=100, col='blue', lwd=2, ylab='', xlab='',bty='n',ylim=c(0,1),axes=FALSE)
x=seq(0,YearMax,length=200)

#creating legend for the graph
legend("topright",inset=.05,legend=c("Cast iron pipe","Ductile pipe"),lty=c(1,1),, lwd=c(2,2),col=c("red","blue"), horiz=F)

#draw the grid on the chart
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()

#STEP 2: Estimating the mean time of survival

intC<-matrix(nrow=YearMax) 
intD<-matrix(nrow=YearMax)

#cdfweib<-function(x,a,b){(1-exp((-(x*b)^(a))))}  
cdfweib<-function(x,a,b){(1-exp(-(b*x)^a))}     	#Weibull CDF
edweibull<-function(x,shape,scale){(1-cdfweib(x,shape,scale))}  #extended Weibull CDF

for (t in 1:YearMax){
     intC[t]<-integrate(edweibull,0,t,shape=1/sr.fit_c$scale[1],scale=1/exp(sr.fit_c$icoef[1]))$val
     intD[t]<-integrate(edweibull,0,t,shape=1/sr.fit_d$scale[1],scale=1/exp(sr.fit_d$icoef[1]))$val
}
print(intC[1000])
print(intD[1000])
cat("Cost per unit of time-cast iron\n")
print(cost_C/intC[1000])
cat("Cost per unit of time-ductile iron\n")
print(cost_D/intD[1000])



