Time = 30
T=seq(1,Time, by =0.01)
v=10000
d=2
c= 20000
i=0.2
tu=10

S<-matrix(double(1),nrow=1,ncol=Time)# setup cost
P<-matrix(double(1),nrow=1,ncol=Time)# penalty cost
E<-matrix(double(1),nrow=1,ncol=Time)# total cost
Q<-matrix(double(1),nrow=1,ncol=Time)# purchasing cost
x<-matrix(double(1),nrow=1,ncol=Time)# total cost

for (t in (1:length(T))){
  x[t]=T[t]*d/tu
  S[t]=v*d/x[t]
  P[t]=c*x[t]*i/2
  Q[t]=d*c
  E[t]=S[t]+P[t] +Q[t]
}
plot.new()
par(mar=c(5, 4, 4, 6) + 0.3)
plot(E,type="l",lwd=3,col="red",ylab="",xlab="",xlim=c(0,length(T)),ylim=c(0,max(E)),axes=FALSE,lty=1,pch=4) #plot the cost per unit of time curve

axis(2,c(seq(0,max(E),by=10000)),c(seq(0,max(E)/1000,by=10)), las=1)
axis(1,c(seq(0,length(T),by=100)),c(seq(1,length(T)/100+1,by=1)))

mtext(expression(paste("T (time unit)")),side=1,col="black",line=2.2) 
mtext(expression(paste(Z, '(x1000 mu)')),side=2,col="black",line=2.5)
points(which.min(E),min(E),pch=23,bg="black",lwd=3)
segments(which.min(E),min(E),which.min(E),0, col= 'darkviolet',lty=1,lwd=1)
segments(0,min(E),which.min(E),min(E), col= 'darkviolet',lty=1,lwd=1)


par(new=TRUE)
plot(S,type="l",lwd=2,col="blue",ylab="",xlab="",xlim=c(0,length(T)),ylim=c(0,max(E)),axes=FALSE,lty=1) #plot the cost per unit of time curve

par(new=TRUE)
plot(P,type="l",lwd=2,col="cyan4",ylab="",xlab="",xlim=c(0,length(T)),ylim=c(0,max(E)),axes=FALSE,lty=1) #plot the cost per unit of time curve

par(new=TRUE)
plot(Q,type="l",lwd=2,col="black",ylab="",xlab="",xlim=c(0,length(T)),ylim=c(0,max(E)),axes=FALSE,lty=1) #plot the cost per unit of time curve

legend("topright",inset=.06,legend=c("Setup","Penalty","Intervention","Total"),lty=c(1,1,1,1), lwd=c(2,2,2,2),col=c("blue","cyan4","black","red"),cex=1.2,y.intersp=1,box.lwd='1')

grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()




