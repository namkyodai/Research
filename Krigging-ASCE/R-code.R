#--INPUT
data<-read.csv("i0_20042009.csv",header=TRUE,sep = ",")
attach(data)
data=data.frame(data)
count=max(data$road)
require(geoR) #external R package from CTAN
library(MASS) #external R package from CTAN
lim =11
maxdistance=1000
no.road=count
no.section=length(data$Axis)
d=100
plot.new()
#define colour
file.remove("residual.csv")
file.remove("summary.csv")
file.create("residual.csv")
file.create("summary.csv")
colors=c("blue","coral3","coral4","cyan1","red","green", "black","gold3","darkseagreen3","deeppink") #this should be equal to number of step (distance) t
maxno=1 #this define the max distance of investigation
netve=c(1:maxno)
tongket=matrix(nrow=2, ncol=maxno) #turning into matrix
#t=3
for (t in maxno:maxno){
  for (r in 1: no.section){
   kc2=krige.control(type.krige="ok",cov.model="exp",cov.pars=c(0.02,650),nugget = 0.001)
    }
    solieu=subset(data, road ==r)
    no.data=length(solieu$road)
     residual=matrix(nrow=no.data, ncol=1) #turning into matrix
    for (i in 1:no.data){
      loci=solieu[solieu$id %in% c(i), ]
      locivalue=loci[4]
      dataset <- subset(solieu, id >= i-t & id <= i+t & id !=i)
      loci=c(loci[2],loci[3])
      loci <- matrix(loci, ncol = 2)
      emac=as.geodata(dataset,coords.col=2:3,data.col=4,data.names=NULL,covar.names = "obj.names",units.m.col = NULL, realisations = NULL)
      kc <- krige.conv(emac, locations = loci, krige = kc2)
      residual[i]=kc$predict-locivalue
     }
    residual=data.matrix(residual)
    id=cbind(1:no.data)
    residual=cbind(id,residual)
    write.table(residual, file="residual.csv", sep = ",", append = TRUE,col.names = FALSE) 
  }
   residual<-read.csv("residual.csv",header=FALSE,sep = ",")
  attach(residual)
  residual=data.frame(residual)
  par(new=TRUE)
   if (t==1){
    plot(density(residual$V3),axes=TRUE,xlim=c(-0.5,0.5),ylim=c(0,5),xlab="",ylab="",main="",lwd=1,lty=netve[t],col=colors[t])
    }
  else {
    lines(density(residual$V3),lwd=1,lty=netve[t],col=colors[t])
      }
   tongket[1,t]=sd(residual$V3)
    tongket[2,t]=mean(residual$V3)
    write.table(c(sd(residual$V3),mean(residual$V3)), file="summary.csv", sep = ",", append = TRUE,col.names = FALSE)
 }
mtext(expression(paste("Density")),side=2,line=2)
mtext(expression(paste("Residuals")),side=1,col="black",line=3) 
box()
legend("topright",inset=.05,legend=c(1:maxno),lty=c(1:maxno),col=colors, horiz=F)
plot.new()
par(mar=c(5, 4, 4, 6) + 0.1)
plot(c(seq(1:maxno)), tongket[1,], pch=2, axes=FALSE, ylim=c(0,0.4), xlab="", ylab="", type="b",col="blue", lwd=2)
axis(2, ylim=c(0,0.2),col="black",las=1)  ## las=1 makes horizontal labels
mtext(expression(paste("Standard deviation  ", sigma)),side=2,col="black",line=3.2) 
axis(1,c(seq(1:maxno)))
mtext(expression(paste("Inspection interval length")),side=1,col="black",line=2.5)
par(new=TRUE)
plot(c(seq(1:maxno)), tongket[2,], pch=4, axes=FALSE, ylim=c(-0.01,0.01), xlab="", ylab="", type="b",col="coral1", lwd=2)
axis(4, ylim=c(-0.01,0.01),col="black",las=1)  ## las=1 makes horizontal labels
mtext(expression(paste("mean of residual")),side=4,col="black",line=3.7)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
## Add Legend
legend("topright",inset=.04,legend=c("Standard deviation","Mean"), text.col=c("blue","coral1"),lty=c(1,1),pch=c(2,4),col=c("blue","coral1"),horiz=F,cex=0.9)
#----the END---