#This program is coded by Nam Lethanh @IBI, ETH Z?rich.
#it serves as learning material for the course infrastructure maintenance management (IMM), which is designed for master students taking their study in civil engineering and management.

#------POTHOLE OCCURRENCE - POISSON PROCESSS-

#students take their own risk to use this code.


data <- read.csv("IMM-1-FS2014-renewaltheory-potholeoccurrence.csv",header=TRUE,sep=",") 
attach(data)

YearMax=50
#model <- glm(data$A~1, family="poisson")
#lambda=exp(coef(model))

#Step 1: Estimate the pothole occurrent rate of each road section

cat("QUESTION A - pothole occurrence rate at each road section \n")

cat("Pothole occurrence rate (lambda) of section A \n")
print(mean(data$A))
cat("Pothole occurrence rate (lambda) of section B \n")
print(mean(data$B))
cat("Pothole occurrence rate (lambda) of section C \n")
print(mean(data$C))

cat("Pothole occurrence rate (lambda) of town \n")
print(mean(data$A)+mean(data$B)+mean(data$C))


cat("QUESTION b - probability that N(t)=15 \n")

poissonf=function(lambda,n,t){exp(-lambda*t)*((lambda*t)^n)/factorial(n)}

n=20
lambda=mean(data$A)+mean(data$B)+mean(data$C)
  
P<-matrix(nrow=12) 

for (t in 1: 12){
  P[t]=poissonf(lambda,n,t)
  }
print(P)

plot.new()
par(mar=c(5, 4, 4, 6) + 0.1)


x <- seq(1, 12, 0.25)

plot(x, ppois(x, mean(data$A)), type = "s", ylab = "F(x)", main = "",axes=FALSE,lwd=1,col="red")


axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels

axis(1,c(seq(1:12)))

par(new=TRUE)
plot(x, ppois(x, mean(data$B)), type = "s", ylab = "", main = "",axes=FALSE,lwd=1,col="blue")

par(new=TRUE)
plot(x, dpois(x, mean(data$C)), type = "s", ylab = "", main = "",axes=FALSE,lwd=1,col="black",lty=2)


#creating legend for the graph
legend("topright",inset=.05,legend=c("A","B","C"),lty=c(1,1,2),, lwd=c(1,1,1),col=c("red","blue","black"), horiz=F)


grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
