# This program was coded by  Nam Lethanh (lethanh@ibi.baug.ethz.ch)
#Multiple system - INTERVENTION ON OIL FIELD 
#------------------------------------------------------
rm(list=ls()) #clear the memory and objects in R console
###            INPUT      
# define the function to estimate the efficiency of the maintenance rule by ration chi .....this following function is the function of 4 parameters 
chifunction<-function(n,N,L,lambda){
for (k in 1:n){
W<-matrix(nrow=k) #total system operation time during 1 operation-repair cycle
tau<-matrix(nrow=k) #failure-free operation time of line i
t<-matrix(nrow=k) #
v<-matrix(nrow=k) #
chi<-matrix(nrow=k) #
A<-matrix(nrow=k) #

W[k]<-k/lambda
for (i in 1:k){
  if (i==1){
    A[i]=1/(n-i+1)
     } else {
  A[i]=A[i-1]+1/(n-i+1)
}
}
  tau<-A
}
for (k in 1:n){
  t[k]=N+L*k
  chi[k]=(1/n)*k/(tau[k]+t[k]*lambda)
}
cat('.............ESTIMATION RESULTS \n')
cat('value of t \n')
print(t)
cat('value of the sum in denominator \n')
print(tau)
cat('value of efficiency \n')
print(chi)
cat('value of k when efficiency ratio is optimal (maximum) \n')
print(which.max(chi))
cat('value of max efficiency ratio  \n')
print(max(chi))
return(chi)
}
#now time to plot the function to see how it looks :) pretty easy right.
plot.new()
par(mar=c(5, 4, 4, 6) + 0.3)
cat('------------------oil field A------------- \n')
n=40
N=5
L=1
lambda=0.002
cat('CASE 1-value of parameter, n, N, L, and lambda \n')
print(c(n,N,L,lambda))
a=chifunction(n,N,L,lambda)
plot(a,type="b",lwd=1,col="red",ylab="",xlab="",xlim=c(0,120),ylim=c(0,1),axes=FALSE,lty=1,pch=1,cex=0.3)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
axis(1,c(seq(0,120,by=10)),c(seq(0,120,by=10)))
mtext(expression(paste("k")),side=1,col="black",line=2.2,cex=1.2) 
mtext(expression(paste(chi[k])),side=2,col="black",line=2.2,cex=1.2) 
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
par(new=TRUE)
n=60
a=chifunction(n,N,L,lambda)
plot(a,type="b",lwd=1,col="brown",ylab="",xlab="",xlim=c(1,120),ylim=c(0,1),axes=FALSE,lty=1,pch=2,cex=0.3)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)
par(new=TRUE)
n=80
a=chifunction(n,N,L,lambda)
plot(a,type="b",lwd=1,col="blue",ylab="",xlab="",xlim=c(1,120),ylim=c(0,1),axes=FALSE,lty=1,pch=3,cex=0.3)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)
par(new=TRUE)
n=120
a=chifunction(n,N,L,lambda)
plot(a,type="b",lwd=1,col="cyan4",ylab="",xlab="",xlim=c(1,120),ylim=c(0,1),axes=FALSE,lty=1,pch=4,cex=0.3)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)
legend("topright",inset=.03,legend=c("A","B","C","D"),lty=c(1,1,1,1),pch=c(1,2,3,4),col=c("red","brown","blue","cyan4"),cex=1.1,y.intersp=1,box.lwd='1')
cat('END ----------------------------- \n')
plot.new()
par(mar=c(5, 4, 4, 6) + 0.3)
cat('--------Sensitivity analysis for oil field A---------------------------------------- \n')
n=40
N=5
L=1
lambda=0.002
cat('CASE 2-value of parameter, n, L, and lambda \n')
print(c(n,L,lambda))
cat('Value of parameter N \n')
a=chifunction(n,N,L,lambda)
plot(a,type="b",lwd=2,col="red",ylab="",xlab="",xlim=c(0,20),ylim=c(0,1),axes=FALSE,lty=1,pch=10,cex=0.3)
points(which.max(a),max(a),pch=23,bg="black",lwd=3)
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
axis(1,c(seq(0,20,by=1)),c(seq(0,20,by=1)))
mtext(expression(paste("k")),side=1,col="black",line=2.2,cex=1.2) 
mtext(expression(paste(chi[k])),side=2,col="black",line=2.2,cex=1.2) 
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
N=6
par(new=TRUE)
a=chifunction(n,N,L,lambda)
plot(a,type="l",lwd=1,col="blue",ylab="",xlab="",xlim=c(0,20),ylim=c(0,1),axes=FALSE,lty=1,pch=2,cex=0.3)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)
N=8
par(new=TRUE)
a=chifunction(n,N,L,lambda)
plot(a,type="l",lwd=1,col="cyan4",ylab="",xlab="",xlim=c(0,20),ylim=c(0,1),axes=FALSE,lty=1,pch=2,cex=0.3)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)
legend("topright",inset=.03,legend=c("N=5","N=6","N=8"),lty=c(1,1,1),pch=c(1,NA,NA),col=c("red","blue","cyan4"),cex=1.1,y.intersp=1,box.lwd='1')
results=a[seq(0,n,by=1)]
results=data.frame(results)
file.remove("results.csv")
file.create("results.csv")
write.table(results, file="results.csv", sep = ",", append = TRUE,col.names = FALSE)
