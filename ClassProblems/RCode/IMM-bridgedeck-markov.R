# This program was coded by  Nam Lethanh (lethanh@ibi.baug.ethz.ch)
#Steady state Markov model
#Intervention strategy for bridge with Markov model
#------------------------------------------------------
rm(list=ls()) #clear the memory and objects in R console
###            INPUT      
data1 <- read.csv("IMM-bridgedeck-markov-p.csv",header=FALSE,sep=",") 
attach(data1)
data2 <- read.csv("IMM-bridgedeck-markov-r.csv",header=TRUE,sep=",") 
attach(data2)
Nmax<-length(data1[,1]) #maximum number of condition states
Rmax<-length(data2[,1])
P<-array(dim=c(Nmax,Nmax)) #define the dimension for transition matrix P
#intervention effectiveness vector
R<-array(dim=c(Rmax,Nmax))
#cost of each IS
C<-matrix(nrow=Rmax)
YearMax=500 #no of year

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
cat('# -------ESTIMATION ------------ \n')

#STEP 1: Define the intervetion-transition matrix
Q<-array(dim=c(Nmax,Nmax,Rmax)) #transition matrix correspoding to each IS
  for (i in 1:Nmax){
    for (j in 1:Nmax){
      for (k in 1:Rmax){
      if (k==1){
        Q[i,j,k]=P[i,j]
      } else if (k==2){
        Q[i,j,k]=P[i,j]
        Q[4,j,k]=R[k,j]
        Q[i,j,k]=Q[i,j,k]
      }
       else if (k==3){
         Q[i,j,k]=P[i,j]
         Q[5,j,k]=R[k,j]
        Q[i,j,k]=Q[i,j,k]
      }
      }
  }
}

#
cat('Intervention repair matrix Q \n')
print(Q)

#STEP 2: Define the state probabilit
pi<-array(dim=c(Nmax,YearMax,Rmax)) #this is the state probability

for (k in 1:Rmax){
for (t in 1:YearMax){
  if (t==1){
    pi[,t,k]=c(1,0,0,0,0)
  } else {
  pi[,t,k]= pi[,t-1,k]%*%Q[,,k]   
  }
}
}

cat('the steady state corresponding to each IS \n')
print(pi[,YearMax,])

#STEP 3: Determine the long term impact
cat('estimating the impact associated with each strategy \n')
Ics<-matrix(nrow=Nmax,ncol=Rmax)
IScost<-array(dim=c(Nmax,Rmax))
IStotal<-matrix(nrow=Rmax)

for (k in 2:Rmax){
if (k==2){
  for (i in 1:Nmax){
    if (i<=3){
      Ics[i,k]=0
    } else if (i==4){
      Ics[i,k]=cost[2]
    } else if (i==5) {
      Ics[i,k]=cost[3]
    }
  }
} else {
  for (i in 1:Nmax){
    if (i<=4){
      Ics[i,k]=0
    }  else if (i==5) {
      Ics[i,k]=cost[3]
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

#Step 4- Determine the first passage time
library(markovchain)

t=1500 #the number of time step (n)
#rehabiliation strategy IS1
cat('first passage time for IS1\n')
Q1<-new("markovchain", states=c("a","b","c","d","e"),transitionMatrix=matrix(c(0.96,0.04,0,0,0,0,0.96,0.04,0,0,0,0,0.96,0.04,0,0.8,0.2,0,0,0,1,0,0,0,0),nrow=5, byrow=TRUE))
A=firstPassage(Q1,"a",t)
b=0
for (i in 1:t){
  b=b+A[i,4]*i
}
print(b)
#rehabiliation strategy IS2
cat('first passage time for IS2\n')
Q2<-new("markovchain", states=c("a","b","c","d","e"),transitionMatrix=matrix(c(0.96,0.04,0,0,0,0,0.96,0.04,0,0,0,0,0.96,0.04,0,0,0,0,0.96,0.04,1,0,0,0,0),nrow=5, byrow=TRUE))
B=firstPassage(Q2,"a",t)
b=0
for (i in 1:t){
  b=b+B[i,5]*i
}
print(b)
step=10
results=data.frame(B[seq(0,t,by=step),1],B[seq(0,t,by=step),2],B[seq(0,t,by=step),3],B[seq(0,t,by=step),4],B[seq(0,t,by=step),5])
file.remove("results.csv")
file.create("results.csv")
write.table(results, file="results.csv", sep = ",", append = TRUE,col.names = FALSE)