# This program was coded by  Nam Lethanh (lethanh@ibi.baug.ethz.ch)
#Multiple system - INTERVENTION ON A SYSTEM OF WINDMILLS
#------------------------------------------------------
rm(list=ls()) #clear the memory and objects in R console
###            INPUT      
# define the function to estimate the efficiency of the maintenance rule by ration chi .....this following function is the function of 4 parameters 
chifunction<-function(n,N,L,lambda,I){
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
  v[k]=I*k/(tau[k]+t[k]*lambda)
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
return(v)
}


obj<-function(n,N,L,lambda,I){
  for (k in 1:n){
    W<-matrix(nrow=k) #total system operation time during 1 operation-repair cycle
    tau<-matrix(nrow=k) #failure-free operation time of line i
    t<-matrix(nrow=k) #
    v<-matrix(nrow=k) #
    chi<-matrix(nrow=k) #
    A<-matrix(nrow=k) #
 #   W[k]<-k/lambda
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
    v[k]=I*k/(tau[k]+t[k]*lambda)
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

cat('----------------------WIND PARK B------------------------------ \n')
n=1*10
I=100 #Money received if the sytem in operation
N=2 # weeks on the site (fix)
L=1 #additional time L
lambda=0.02
cat('CASE 1-value of parameter, n, N, L, and lambda \n')
print(c(n,N,L,lambda))
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=2,col="red",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=4,cex=1)
points(which.max(a),max(a),pch=23,bg="black",lwd=3)
axis(2, ylim=c(0,1000),col="black",las=1)  ## las=1 makes horizontal labels
axis(1,c(seq(1,n,by=1)),c(seq(1,n,by=1)))
mtext(expression(paste("k")),side=1,col="black",line=2.2,cex=1.2) 
mtext(expression(paste(eta[k], ' (mu)')),side=2,col="black",line=2.2,cex=1) 
segments(which.max(a),max(a),which.max(a),0, col= 'darkviolet',lty=1,lwd=1)
segments(0,max(a),which.max(a),max(a), col= 'darkviolet',lty=1,lwd=1)
text(which.max(a)*1,max(a)*1.04,expression(paste(eta[k],'=688, k=3')),pos=4,cex = 1.2, srt = 0)

grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()

legend("topright",inset=.03,legend=c("n=10","N=2","L=1",expression(paste(theta,'=0.02'))),cex=1.2,y.intersp=1.2,box.lwd='')

cat('END OF CASE 1--------------------------------------------------------------- \n')


cat('----------------------WIND PARK B------------------------------ \n')
n=1*5
I=100 #Money received if the sytem in operation
N=2 # weeks on the site (fix)
L=1 #additional time L
lambda=0.02
cat('CASE 1-value of parameter, n, N, L, and lambda \n')
print(c(n,N,L,lambda))
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=2,col="blue",ylab="",xlab="",xlim=c(1,10),ylim=c(0,1000),axes=FALSE,lty=1,pch=25,cex=1)
points(which.max(a),max(a),pch=23,bg="black",lwd=3)
axis(2, ylim=c(0,1000),col="black",las=1)  ## las=1 makes horizontal labels
axis(1,c(seq(1,10,by=1)),c(seq(1,10,by=1)))
mtext(expression(paste("k")),side=1,col="black",line=2.2,cex=1.2) 
mtext(expression(paste('Impact   ',eta[k], ' (mu)')),side=2,col="black",line=2.2,cex=1) 
segments(which.max(a),max(a),which.max(a),0, col= 'darkviolet',lty=1,lwd=1)
segments(0,max(a),which.max(a),max(a), col= 'darkviolet',lty=1,lwd=1)
text(which.max(a)*1,max(a)*1.1,expression(paste(eta[k]^A,'=385, k=1')),pos=4,cex = 1.2, srt = 0)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()

par(new=TRUE)
b=obj(n,N,L,lambda,I)
plot(b,type="l",lwd=2,col="blue",ylab="",xlab="",xlim=c(1,10),ylim=c(0,0.8),axes=FALSE,lty=2,pch=25,cex=1)
points(which.max(b),max(b),pch=10,bg="black",lwd=3)
axis(4, ylim=c(0,0.8),col="black",las=1)  ## las=1 makes horizontal labels
text(which.max(b)*1,max(b)*1.03,expression(paste(chi[k]^A,'=0.769, k=1')),pos=4,cex = 1.2, srt = 0)


n=10
par(new=TRUE)
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=2,col="red",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=4,cex=1)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)
segments(which.max(a),max(a),which.max(a),0, col= 'darkviolet',lty=1,lwd=1)
segments(0,max(a),which.max(a),max(a), col= 'darkviolet',lty=1,lwd=1)
text(which.max(a)*1,max(a)*1.04,expression(paste(eta[k]^B,'=688, k=3')),pos=4,cex = 1.2, srt = 0)

par(new=TRUE)
b=obj(n,N,L,lambda,I)
plot(b,type="l",lwd=2,col="red",ylab="",xlab="",xlim=c(1,10),ylim=c(0,0.8),axes=FALSE,lty=2,pch=25,cex=1)
points(which.max(b),max(b),pch=10,bg="darkmagenta",lwd=3)
axis(4, ylim=c(0,0.8),col="black",las=1)  ## las=1 makes horizontal labels
text(which.max(b)*1,max(b)*1.03,expression(paste(chi[k]^B,'=0.688, k=3')),pos=4,cex = 1.2, srt = 0)
mtext(expression(paste('efficiency   ', chi[k])),side=4,col="black",line=2.2,cex=1) 



legend("topright",inset=.03,legend=c(expression(paste(eta[k]^A)),expression(paste(eta[k]^B)),expression(paste(chi[k]^A)),expression(paste(chi[k]^B))),lty=c(1,1,2,2),pch=c(25,4,NA,NA),lwd=c(2,2,2,2),col=c("blue","red","blue","red"),cex=1.2,y.intersp=1.2,box.lwd='')

cat('END OF CASE 2--------------------------------------------------------------- \n')

#--------------Sensitivity analysis

#Change the value of N while keeping other constants
plot.new()
par(mar=c(5, 4, 4, 6) + 0.3)
n=10
I=100 #Money received if the sytem in operation
N=0 # weeks on the site (fix)
L=1 #additional time L
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=1,col="blue",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=1,cex=1)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)
axis(2, ylim=c(0,1000),col="black",las=1)  ## las=1 makes horizontal labels
axis(1,c(seq(1,n,by=1)),c(seq(1,n,by=1)))
mtext(expression(paste("k")),side=1,col="black",line=2.2,cex=1.2) 
mtext(expression(paste(eta[k], ' (mu)')),side=2,col="black",line=2.2,cex=1)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()


#--------------------




N=1
par(new=TRUE)
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=1,col="cyan4",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=2,cex=0.3)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)


#--------------------

N=2
par(new=TRUE)
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=2,col="red",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=3,cex=0.3)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)


#--------------------

N=3
par(new=TRUE)
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=1,col="chocolate",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=4,cex=0.5)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)


#--------------------

N=4
par(new=TRUE)
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=1,col="brown4",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=5,cex=0.5)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)



#--------------------

N=5
par(new=TRUE)
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=1,col="darkmagenta",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=6,cex=0.5)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)

#--------------------

#N=6
#par(new=TRUE)
#a=chifunction(n,N,L,lambda,I)
#plot(a,type="b",lwd=1,col="coral4",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=7,cex=0.5)
#points(which.max(a),max(a),pch=23,bg="black",lwd=1)


#--------------------

#N=7
#par(new=TRUE)
#a=chifunction(n,N,L,lambda,I)
#plot(a,type="b",lwd=1,col="azure4",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=8,cex=0.5)
#points(which.max(a),max(a),pch=23,bg="black",lwd=1)


#N=8
#par(new=TRUE)
#a=chifunction(n,N,L,lambda,I)
#plot(a,type="b",lwd=1,col="aquamarine",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=9,cex=0.5)
#points(which.max(a),max(a),pch=23,bg="black",lwd=1)




#N=9
#par(new=TRUE)
#a=chifunction(n,N,L,lambda,I)
#plot(a,type="b",lwd=1,col="grey",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000#),axes=FALSE,lty=1,pch=10,cex=0.5)
#points(which.max(a),max(a),pch=23,bg="black",lwd=1)

#N=10
#par(new=TRUE)
#a=chifunction(n,N,L,lambda,I)
#plot(a,type="b",lwd=1,col="chartreuse3",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=11,cex=0.5)
#points(which.max(a),max(a),pch=23,bg="black",lwd=1)


#N=15
#par(new=TRUE)
#a=chifunction(n,N,L,lambda,I)
#plot(a,type="b",lwd=1,col="black",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=12,cex=0.5)
#points(which.max(a),max(a),pch=23,bg="black",lwd=1)

#legend("topright",inset=.03,legend=c("n=80","L=0.1",expression(paste(lambda,'=0.1'))),cex=1.1,y.intersp=1,box.lwd='1')
legend("topright",inset=.03,legend=c("N=0","N=1","N=2","N=3","N=4","N=5"),lwd=c(1,1,1),col=c("blue","cyan4","red","chocolate","brown4","darkmagenta"),pch=c(1,2,3,4,5,6),cex=1,y.intersp=1,box.lwd='1')
cat('CHANGE N----------------------------------------------- \n')




#--------------Sensitivity analysis

#Change the value of N while keeping other constants
plot.new()
par(mar=c(5, 4, 4, 6) + 0.3)
n=10
I=100 #Money received if the sytem in operation
N=2 # weeks on the site (fix)
L=0 #additional time L
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=1,col="blue",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=1,cex=1)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)
axis(2, ylim=c(0,1000),col="black",las=1)  ## las=1 makes horizontal labels
axis(1,c(seq(1,n,by=1)),c(seq(1,n,by=1)))
mtext(expression(paste("k")),side=1,col="black",line=2.2,cex=1.2) 
mtext(expression(paste(eta[k], ' (mu)')),side=2,col="black",line=2.2,cex=1)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()




#--------------------

N=2 # weeks on the site (fix)
L=0.5 #additional time L
par(new=TRUE)
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=2,col="cyan4",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=2,cex=0.5)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)



#--------------------

N=2 # weeks on the site (fix)
L=1 #additional time L
par(new=TRUE)
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=1,col="red",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=3,cex=0.3)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)


#--------------------

N=2 # weeks on the site (fix)
L=2 #additional time L
par(new=TRUE)
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=1,col="chocolate",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=4,cex=0.5)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)




#--------------------

N=2 # weeks on the site (fix)
L=3 #additional time L
par(new=TRUE)
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=1,col="brown4",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=5,cex=0.5)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)


#--------------------

N=2 # weeks on the site (fix)
L=4 #additional time L
par(new=TRUE)
a=chifunction(n,N,L,lambda,I)
plot(a,type="b",lwd=1,col="darkmagenta",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=6,cex=0.5)
points(which.max(a),max(a),pch=23,bg="black",lwd=1)


results=a[seq(1,n,by=1)]
results=data.frame(results)

file.remove("results.csv")
file.create("results.csv")
write.table(results, file="results.csv", sep = ",", append = TRUE,col.names = FALSE)

#--------------------

#N=2 # weeks on the site (fix)
#L=6 #additional time L
#par(new=TRUE)
#a=chifunction(n,N,L,lambda,I)
#plot(a,type="b",lwd=1,col="coral4",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=7,cex=0.5)
#points(which.max(a),max(a),pch=23,bg="black",lwd=1)


#--------------------

#N=2 # weeks on the site (fix)
#L=7 #additional time L
#par(new=TRUE)
#a=chifunction(n,N,L,lambda,I)
#plot(a,type="b",lwd=1,col="azure4",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=8,cex=0.5)
#points(which.max(a),max(a),pch=23,bg="black",lwd=1)


#N=2 # weeks on the site (fix)
#L=8 #additional time L
#par(new=TRUE)
#a=chifunction(n,N,L,lambda,I)
#plot(a,type="b",lwd=1,col="aquamarine",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=9,cex=0.5)
#points(which.max(a),max(a),pch=23,bg="black",lwd=1)




#N=2 # weeks on the site (fix)
#L=9 #additional time L
#par(new=TRUE)
#a=chifunction(n,N,L,lambda,I)
#plot(a,type="b",lwd=1,col="grey",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=10,cex=0.5)
#points(which.max(a),max(a),pch=23,bg="black",lwd=1)

#N=2 # weeks on the site (fix)
#L=10 #additional time L
#par(new=TRUE)
#a=chifunction(n,N,L,lambda,I)
#plot(a,type="b",lwd=1,col="chartreuse3",ylab="",xlab="",xlim=c(1,n),ylim=c(0,1000),axes=FALSE,lty=1,pch=11,cex=0.5)
#points(which.max(a),max(a),pch=23,bg="black",lwd=1)




#legend("topright",inset=.03,legend=c("n=80","L=0.1",expression(paste(lambda,'=0.1'))),cex=1.1,y.intersp=1,box.lwd='1')
legend("topright",inset=.03,legend=c("L=0","L=0.5","L=1","L=2","L=3","L=4"),lwd=c(1,1,1),col=c("blue","cyan4","red","chocolate","brown4","darkmagenta"),pch=c(1,2,3,4,5,6),cex=1,y.intersp=1,box.lwd='1')

cat('CHANGE L----------------------------------------------- \n')






