# This program was coded by  Nam Lethanh (lethanh@ibi.baug.ethz.ch)
#Semi-Markov-tunnel fan
#------------------------------------------------------
rm(list=ls()) #clear the memory and objects in R console
#STEP 1:    define  INPUT      

tau_a=1 #initial lifetime of fan a
tau_b=4 #initial lifetime of fan b
tau=4   #lifetime of fan a or b after intervention
N=2     #critical age at which replacement is a must
year=7
I1=1000 #impacts if only one fan is replaced
I2=1400 #impacts if both two fans are replaced and repaired.
t<-seq(0,tau,by=1) #time space of the fan
zeta<-matrix(nrow=(N^2+1)) #this is the state space of the condition state
p<-matrix(nrow=year,ncol=(N+1)) # vector of conditional probability for fan a
q<-matrix(nrow=year,ncol=(N+1)) # vector of conditional probability for fan b

p0=c(0.08,0.14,0.17,0.17,0.15,0.11,0.18)
q0=c(0.05,0.08,0.16,0.3,0.2,0.1,0.11)

#building the state space for the model
for (k in 1:(N^2+1)){
 if (k<=(N+1)){
   zeta[k]=paste(0,(k-1))   
 } else {
   zeta[k]=paste((k-(N+1)),0)   
 }
}
cat('the state space of the model \n')
print(zeta)
  
# STEP 2: define the object life time condition probability

T<-seq(0,N,by=1)
for (i in 1:year){
  for (j in (1:length(T))){
     if (j==1){
      p[i,j]=p0[i]
      q[i,j]=q0[i]
    } else if (i<j){        
      p[i,j]=0
      q[i,j]=0
      } else {
        p [i,j]=p[i,j-1]/(1-p[T[j],T[j]])
        q [i,j]=q[i,j-1]/(1-q[T[j],T[j]])
      }
    }
  }
cat('Probability of failing in year i, fan a \n')
print(p)
cat('Probability of failing in year i, fan b \n')
print(q)

#STEP 3: estimating transition probability matrix M

M<-matrix(nrow=length(zeta),ncol=length(zeta)) #this is the transition matrix of the system (fan a + fan b)
#M<-array(dim=c(N,N,N,N))   	#mean number of failures (object n, time t, interval k)

for (n in 1:length(T)){
if (n==1){
for (i in 1:length(zeta)){
  for (j in 1:length(zeta)){     
        if (i==1 & j==2){
          B=0
          for (k in (i+1): year){
          B=B+q[k,i]
          } 
        M[i,j]<-p[i,T[n+1]]*B
      } else if (i==1 & j==3){
        B=0
        for (k in (i+1): year){
          B=B+p[k,i]
        } 
        M[i,j]<-q[i,T[n+1]]*B
        
      } else if (i==1 & j==4){
        B=0
        for (k in (i+2): year){
          B=B+p[k,i]
        } 
        M[i,j]<-q[i+1,T[n+1]] *B
           
      } else  if (i==1 & j==5)    {
        B=0
        for (k in (i+2): year){
          B=B+q[k,i]
        } 
        M[i,j]<-p[i+1,T[n+1]]* B
      
      } else if (i==2 & j==2) {
        M[i,j]<-0
      } else  if (i==2 & j==3) {
        B=0
        for (k in (i): year){
          B=B+p[k,i-1]
        } 
        M[i,j]<-q[i,T[n+2]] *B
        
      } else if (i==2 & j==4){
        B=0
        for (k in (i+1): year){
          B=B+p[k,i-1]
        } 
        M[i,j]<- q[i+1,T[n+2]] *B
        
      } else if (i==2 & j==5) {
        B=0
        for (k in (i+1): year){
          B=B+q[k,i]
        } 
        M[i,j]<- p[i-1,T[n+1]]  *B
       
      } else  if (i==3 & j==2) {
        B=0
        for (k in (i-1): year){
          B=B+q[k,i-2]
        } 
        M[i,j]<- p[i-1,T[n+2]] *B
      } else if (i==3 & j==3){
        M[i,j]<-0
      } else if (i==3 & j==4){
        B=0
        for (k in (i): year){
          B=B+p[k,i-1]
        } 
        M[i,j]<- q[i-2,T[n+1]] *B
      }else if (i==3 & j==5){
        B=0
        for (k in (i): year){
          B=B+q[k-1,i-1]
        } 
        M[i,j]<- p[i,T[n+2]] *B
      }else if (i==4 & j==2){
        B=0
        for (k in (i-2): year){
          B=B+q[k,i-3]
        } 
        M[i,j]<- p[i-1,T[n+2]+1] *B
      }else if (i==4 & j==3 ){
        M[i,j]<-0
      }else if (i==4 & j==4){
        M[i,j]<-0
      } else if (i==4 & j==5){
        B=0
        for (k in (i-1): year){
          B=B+q[k,i-3]
        } 
        M[i,j]<- p[i,T[n+2]+1] *B
      }else if (i==5 & j ==2){
        
        M[i,j]<-0
      } else if (i==5 & j==3){
        B=0
        for (k in (i-3): year){
          B=B+p[k,i-4]
        } 
        M[i,j]<- q[i-2,T[n+2]+1] *B
      }else if (i==5 & j==4){
        B=0
        for (k in (i-2): year){
          B=B+p[k,i-4]
        } 
        M[i,j]<- q[i-1,T[n+2]+1] *B
      }else 
        
      {
        M[i,j]<-0
      }  
  }
}
}
}

A<-matrix(nrow=length(zeta))

for (i in 1:length(zeta)){

  for (j in 1:length(zeta)){
    A[i]<-sum(M[i,])
  }
}

for (i in 1:length(zeta)){
    M[i,1]<-1-A[i]
}
cat ('Transition probability Matrix \n')
print(M)
#STEP 4- Calculating the steady state probability
library(markovchain) #call the package "markovchain"
statesNames=c("00","01","10","20","02")

N<-new("markovchain", states=statesNames, transitionMatrix=M)
cat('The steady state probability \n')
piM=t(steadyStates(N))
print(piM)
#Step 4: Estimating the probability of being failed in year i for both two fans
O<-array(dim=c(year,length(zeta)))

O[1,1]=p[1,1]*q[1,1]+M[1,2]+M[1,3]
O[1,2]=p[1,1]*q[2,2]+q[2,2]*sum(p[c(2:year),1])+p[1,1]*sum(q[c(3:year),2])
O[1,3]=p[2,2]*q[1,1]+q[1,1]*sum(p[c(3:year),2])+p[2,2]*sum(q[c(2:year),1])
O[1,4]=p[3,3]*q[1,1]+q[1,1]*sum(p[c(4:year),3])+p[3,3]*sum(q[c(2:year),1])
O[1,5]=p[1,1]*q[3,3]+p[1,1]*sum(q[c(4:year),3])+q[3,3]*sum(p[c(2:year),1])
O[2,1]=p[2,1]*q[2,1]+M[1,4]+M[1,5]
O[2,2]=p[2,1]*q[3,2]+p[2,1]*sum(q[c(4:year),2])+q[3,2]*sum(p[c(3:year),1])
O[2,3]=p[3,2]*q[2,1]+p[3,2]*sum(q[c(3:year),1])+q[2,1]*sum(p[c(4:year),2])
O[2,4]=p[4,3]*q[2,1]+p[4,3]*sum(q[c(3:year),1])+q[2,1]*sum(p[c(5:year),3])
O[2,5]=p[2,1]*q[4,3]+p[2,1]*sum(q[c(5:year),3])+q[4,3]*sum(p[c(3:year),1])
O[3,1]=p[3,1]*q[3,1]+p[3,1]*sum(q[c(4:year),1])+q[3,1]*sum(p[c(4:year),1])
O[3,2]=p[3,1]*q[4,2]+p[3,1]*sum(q[c(5:year),1])+q[4,2]*sum(p[c(4:year),1])
O[3,3]=p[4,2]*q[3,1]+p[4,2]*sum(q[c(4:year),1])+q[3,1]*sum(p[c(5:year),2])
O[3,4]=p[5,3]*q[3,1]+p[5,3]*sum(q[c(4:year),1])+q[3,1]*sum(p[c(6:year),3])
O[3,5]=p[3,1]*q[5,3]+p[3,1]*sum(q[c(6:year),3])+q[5,3]*sum(p[c(4:year),1])
O[4,1]=p[4,1]*q[4,1]+p[4,1]*sum(q[c(5:year),1])+q[4,1]*sum(p[c(5:year),1])
O[4,2]=p[4,1]*q[5,2]+p[4,1]*sum(q[c(6:year),2])+q[5,2]*sum(p[c(5:year),1])
O[4,3]=p[5,2]*q[4,1]+p[5,2]*sum(q[c(5:year),1])+q[4,1]*sum(p[c(6:year),2])
O[4,4]=p[6,3]*q[4,1]+p[6,3]*sum(q[c(5:year),1])+q[4,1]*sum(p[c(7:year),3])
O[4,5]=p[4,1]*q[6,3]+p[4,1]*sum(q[c(7:year),3])+q[6,3]*sum(p[c(5:year),1])
O[5,1]=p[5,1]*q[5,1]+p[5,1]*sum(q[c(6:year),1])+q[5,1]*sum(p[c(6:year),1])
O[5,2]=p[5,1]*q[6,2]+p[5,1]*sum(q[c(7:year),2])+q[6,2]*sum(p[c(6:year),1])
O[5,3]=p[6,2]*q[5,1]+p[6,2]*sum(q[c(6:year),1])+q[5,1]*sum(p[c(7:year),2])
O[5,4]=p[7,3]*q[5,1]+p[7,3]*sum(q[c(6:year),1])+q[5,1]*0
O[5,5]=p[5,1]*q[7,3]+q[7,3]*sum(p[c(6:year),1])+p[5,1]*0
O[6,1]=p[6,1]*q[6,1]+p[6,1]*sum(q[c(7:year),1])+q[6,1]*sum(p[c(7:year),1])
O[6,2]=p[6,1]*q[7,2]+q[7,2]*p[7,1]
O[6,3]=p[7,2]*q[6,1]+q[7,2]*p[7,1]
O[6,4]=0
O[6,5]=0
O[7,1]=p[7,1]*q[7,1]
O[7,2]=0
O[7,3]=0
O[7,4]=0
O[7,5]=0



cat('value of probability of failing for both fan a and fan b \n')
print(O)

#step 5 - calculating the mean time the fan system stays in each state
X<-array(dim=c(year,length(zeta)))

for (i in 1:year){
  for (j in 1: length(zeta)){
  X[i,j]<-O[i,j]*i
  }
}
v<-array(dim=c(length(zeta)))
for (i in 1:length(zeta)){
v[i]  =sum(X[,i])
}
cat('value of mean time the fan system stays in each state \n')
print(v)

#Step 6 - calculating the average impact for one step transition from state i
omega<-array(dim=c(length(zeta)))

for (i in 1:length(zeta)){
  omega[i]=M[i,1]*I2+(1-M[i,1])*I1
}
cat('value of eaverage impact for one step transition from condition state i \n')
print(omega)

#step 7- calculating the mean impacts per units of time
A<-array(dim=c(length(zeta))) #the nominator
B<-array(dim=c(length(zeta)))#the denominator

for (i in 1:length(zeta)){
  A[i]=omega[i]*piM[i]
  B[i]=v[i]*piM[i]
}
g<-sum(A)/sum(B)
cat('value of mean impacts per units of time \n')
print(g)