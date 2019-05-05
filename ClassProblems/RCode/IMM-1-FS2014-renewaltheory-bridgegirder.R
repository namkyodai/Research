#This program is coded by Nam Lethanh @IBI, ETH Zürich.
#it serves as learning material for the course infrastructure maintenance management (IMM), which is designed for master students taking their study in civil engineering and management.


#students take their own risk to use this code.

#STEP 1: DEFINE THE PARAMETER OF THE MODEL
#define dataframe
obj=c(1,2)
Ci=c(100000,100000)
Pi=c(3000,3000)
alphaexp=c(1.128379,2.256758)
alphaweib=c(1,2) #scale parameter
betaweib=c(2,2) #shape parameter
data=data.frame(obj,Ci,Pi,alphaexp,alphaweib,betaweib)
objnumber=length(data$obj)


YearMax=50 # this is the time frame of investigation

#STEP 2: DEFINE THE DISTRIBUTION FUNCTION AND ESTIMATE THE CONVERGENT VALUE OF MEAN TIME TO FAILURE.

#define the functional form for cummulative distribution function
cdfexp<-function(x,r){1-exp(-(r*x))}    						#exponential CDF
edexp<-function(x,rate){(1-cdfexp(x,rate))}  
cdfweib<-function(x,a,b){(1-exp((-(x*b)^(a))))}   		#Weibull CDF
edweibull<-function(x,shape,scale){(1-cdfweib(x,shape,scale))}  #extended Weibull CDF
ratef<-function(x,shape,scale){shape*scale*x^(shape-1)} 
#here x is the variable, r is rate parameter, a is the 

int_exp<-matrix(nrow=YearMax,ncol=objnumber) 
int_weib<-matrix(nrow=YearMax,ncol=objnumber) 
intweib<-matrix(nrow=YearMax)
intexp<-matrix(nrow=YearMax)

zetexp=function(x,b1,b2){exp(-(x*b1))*exp(-(x*b2))} 
zetweib=function(x,b1,a1,b2,a2){exp(-(x*b1)^a1)*exp(-(x*b2)^a2)} 

for (t in 1:YearMax){
  for (n in 1:objnumber){
      int_exp[t,n]<-integrate(edexp,0,t,rate=data$alphaexp[n])$val	
      int_weib[t,n]<-integrate(edweibull,0,t,shape=data$betaweib[n],scale=data$alphaweib[n])$val
           
    }
  intexp[t]=integrate(zetexp,0,t,alphaexp[1],alphaexp[2])$val
  intweib[t]=integrate(zetweib,0,t,alphaweib[1],betaweib[1],alphaweib[2],betaweib[2])$val
}

print(intexp)
print(intweib)


cat("convergent value of expected mean time to failure - Exponential \n") 
print(int_exp)

cat("convergent value of expected mean time to failure - Weibull \n") 
print(int_weib)

#STEP 3: CALCULATE THE COST FOR EACH STRATEGY UNDER EACH DISTRIBUTION MODEL

eta<-matrix(nrow=2,ncol=2)  #eta[1] is for strategy 1 and eta[2] is for strategy 2.

cat("Estimation results-Exponential distribution function\n")
eta[1,1]=data$Ci[1]*(1/int_exp[YearMax,1]+1/int_exp[YearMax,2])
eta[1,2]=(data$Ci[1]+data$Pi[1])*(1/(intexp[YearMax]))

print(eta[1,])

cat("Estimation results-Weibull distribution function\n")

eta[2,1]=data$Ci[1]*(1/int_weib[YearMax,1]+1/int_weib[YearMax,2])

eta[2,2]=(data$Ci[1]+data$Pi[1])*(1/(intweib[YearMax]))

print(eta[2,])

#STEP 4: DRAW THE DETERIORATION CURVES TO COMPARE THE DETERIORATION OF THE TWO DISTRIBUTION FUNCTIONS USED IN THE EXAMPLE

plot.new()
par(mar=c(5, 4, 4, 6) + 0.1)


#for exponential distribution
x=seq(0,4,length=200)
#y=1-exp(-(data$alphaexp[1]*x))
plot(x,1-exp(-(data$alphaexp[1]*x)),type="l",lwd=3,col="red",ylab="p",axes=FALSE,lty=1) #plot the cdf of a

axis(2, ylim=c(0,0.2),col="black",las=1)  ## las=1 makes horizontal labels

axis(1,c(seq(1:4)))

par(new=TRUE)
plot(x,1-exp(-(data$alphaexp[2]*x)),type="l",lwd=3,col="red",ylab="p",axes=FALSE,lty=2) #plot the cdf of b


par(new=TRUE)
#for Weibull distribution
x=seq(0,4,length=200)
#y=1-exp(-(data$alphaexp[1]*x))
plot(x,1-exp(-(x*data$alphaweib[1])^(data$betaweib[1])),type="l",lwd=3,col="blue",ylab="p",axes=FALSE,lty=1) #plot the cdf of a

par(new=TRUE)
plot(x,1-exp(-(x*data$alphaweib[2])^(data$betaweib[2])),type="l",lwd=3,col="blue",ylab="p",axes=FALSE,lty=2) #plot the cdf of b

#plot the pdf

par(new=TRUE)
plot(x,dexp(x,data$alphaexp[1]),type="l",lwd=1,col="cyan4",ylab="p",axes=FALSE,lty=3)#plot the pdf of a - exponential
par(new=TRUE)
plot(x,dexp(x,data$alphaexp[2]),type="l",lwd=1,col="cyan4",ylab="p",axes=FALSE,lty=5)#plot the pdf of b - exponential
par(new=TRUE)
plot(x,dweibull(x, data$betaweib[1],data$alphaweib[1]),type="l",lwd=1,col="violet",ylab="p",axes=FALSE,lty=1) #plot the pdf of a - weibull

par(new=TRUE)
plot(x,dweibull(x, data$betaweib[2],data$alphaweib[2]),type="l",lwd=1,col="black",ylab="p",axes=FALSE,lty=1) #plot the pdf of b - weibull

#creating legend for the graph
legend("topright",inset=.05,legend=c("cdf-exp-a","cdf-exp-b","cdf-Weib-a","cdf-Weib-b", "pdf-exp-a","pdf-exp-b","pdf-Weib-a","pdf-Weib-b"),lty=c(1,2,1,2,3,5,1,1),, lwd=c(3,3,3,3,1,1,1,1),col=c("red","red","blue","blue","cyan4","cyan4","violet","black"), horiz=F)

#draw the grid on the chart
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()