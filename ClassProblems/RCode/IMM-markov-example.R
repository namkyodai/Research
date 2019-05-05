# This program was coded by  Nam Lethanh (lethanh@ibi.baug.ethz.ch)
#Steady state Markov model
#Intervention strategy for bridge with Markov model
#------------------------------------------------------
rm(list=ls()) #clear the memory and objects in R console
###            INPUT      
data1 <- read.csv("IMM-markov-example-P.csv",header=FALSE,sep=",") 
attach(data1)
data2 <- read.csv("IMM-markov-example-R.csv",header=TRUE,sep=",") 
attach(data2)
Nmax<-length(data1[,1]) #maximum number of condition states
Rmax<-length(data2[,1])
P<-array(dim=c(Nmax,Nmax)) #define the dimension for transition matrix P
#intervention effectiveness vector
R<-array(dim=c(Rmax,Nmax))
#cost of each IS
C<-matrix(nrow=Rmax)
YearMax=15 #no of year

cat('# -------------------INPUT ------------------------------- \n')

#read value of transition matrix from csv file
for (i in 1:Nmax){
  for (j in 1:Nmax){
    P[i,j]=data1[i,j]
  }
}
  #reading the strategy vector from csv file
 for (k in 1:Rmax){
   for (i in 1:Nmax){
     R[k,i]=data2[k,i+1]
     C[k]=data2$cost[k]
   }
 }
cat('value of transition matrix P \n')
print(P)
cat('value of intervention vector R \n')
print(R)
cat('impact incurred by each IS C \n')
print(C)
cat('# -------------------ESTIMATION -------------------------- \n')
Q<-array(dim=c(Nmax,Nmax,Rmax)) #transition matrix correspoding to each IS
  for (i in 1:Nmax){
    for (j in 1:Nmax){
      for (k in 1:Rmax){
      if (k==1){
        Q[i,j,k]=P[i,j]
      } else if (k==2){
        Q[i,j,k]=P[i,j]
        Q[3,j,k]=R[k,j]
     #   Q[4,,]=c(1,0,0,0,0)
        Q[5,,]=c(1,0,0,0,0)
        Q[i,j,k]=Q[i,j,k]
      } else {
         Q[i,j,k]=P[i,j]
         Q[4,,]=c(0,0,0,0.85,0.15)
         Q[5,j,k]=R[k,j]
        Q[i,j,k]=Q[i,j,k]
      }
      }
  }
}

#
cat('Intervention repair matrix Q \n')
print(Q)


pi<-array(dim=c(Nmax,YearMax,Rmax)) #this is the state probability

for (k in 1:Rmax){
for (t in 1:YearMax){
  if (t==1){
    pi[,t,k]=c(0.435,0.174,0.261,0.086,0.044)
  } else {
  pi[,t,k]= pi[,t-1,k]%*%Q[,,k]   
  }
}
}

cat('the steady state corresponding to each IS \n')
print(pi[,YearMax,])

cat('estimating the impact associated with each strategy \n')

Ics<-matrix(nrow=Nmax,ncol=Rmax)
IScost<-array(dim=c(Nmax,Rmax))
IStotal<-matrix(nrow=Rmax)

for (k in 2:Rmax){
if (k==2){
  for (i in 1:Nmax){
    if (i<=2){
      Ics[i,k]=0
    } else if (i==3){
      Ics[i,k]=cost[2]
    } else if (i==5) {
      Ics[i,k]=0
    } else {
      Ics[i,k]=0
    }
  }
} else {
  for (i in 1:Nmax){
    if (i<=3){
      Ics[i,k]=0
    }  else if (i==5) {
      Ics[i,k]=cost[3]
    } else {
      Ics[i,k]=0
    }
  }
}
}
for (k in 2:Rmax){
  for (i in 1:Nmax){
    IScost[i,k]  <- Ics[i,k]*pi[i,YearMax,k]
  }
  IStotal[k]=sum(IScost[,k])
}
cat('impact per CS for each IS \n')
print(Ics)
cat('Expected impacts per each CS in each IS \n')
print(IScost)
cat('Total expected impact for each IS \n')
print(IStotal)
cat('The optimal (minimal total impact) IS is \n')
print(which.min(IStotal))

#--------Draw the deterioration distribution

plot.new()
stackedPlot <- function(data, time=NULL, col=1:length(data),...) {
  if (is.null(time)) {
    time <- 1:length(data[[1]])
    plot(0,0, xlim = range(time), ylim = c(0,max(rowSums(data))), axes=FALSE,t="n" , ...)
    axis(2, ylim=c(0,100),col="black",las=1)  ## las=1 makes horizontal labels
    axis(1,c(seq(1,YearMax,by=1)),c(seq(1,YearMax,by=1)))
    box()
    
    for (i in length(data):1) {
      #the summup to the current collumn
      prep.data <- rowSums(data[1:i]);
      # The polygon must have his first and last point on the zero line
      prep.y <- c(0, prep.data,0)
      prep.x <- c(time[1], time, time[length(time)] )
      polygon(prep.x, prep.y, col=col[i], border = NA);
    }
  }
}
pi=pi*100
colors=c("aliceblue","coral1","limegreen","orange","yellow2")
stackedPlot(data.frame(t(pi[,,1])),col=colors,xlab="Time (years)",ylab="Percentage(%)")
legend("topright", inset=0.09, title="CSs",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=1.2)
title(main="Condition state distribution - Do nothing", col.main="red", font.main=4)
stackedPlot(data.frame(t(pi[,,2])),col=colors,xlab="Time (years)",ylab="Percentage(%)")
legend("topright", inset=0.09, title="CSs",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=1.2)
title(main="Condition state distribution - IS 2", col.main="red", font.main=4)
stackedPlot(data.frame(t(pi[,,3])),col=colors,xlab="Time (years)",ylab="Percentage(%)")
legend("topright", inset=0.09, title="CSs",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=1.2)
title(main="Condition state distribution - IS 1", col.main="red", font.main=4)
#THE END



results=data.frame(t(pi[,,3]),t(pi[,,2]))

file.remove("results.csv")
file.create("results.csv")
write.table(results, file="results.csv", sep = ",", append = TRUE,col.names = FALSE)

