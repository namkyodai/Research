# This program was coded by  Nam Lethanh (lethanh@ibi.baug.ethz.ch)
#Steady state Markov model
#Intervention strategy for bridge with Markov model
#------------------------------------------------------
rm(list=ls()) #clear the memory and objects in R console
###            INPUT      
#---------Use have to install package "markovchain" in order to run this script
library(markovchain)
t=1500 #the number of time step (n)
#rehabiliation strategy IS1
Q2<-new("markovchain", states=c("a","b","c","d","e"),transitionMatrix=matrix(c(0.91,0.09,0,0,0,0,0.95,0.05,0,0,0,0,0.94,0.06,0,0,0,0,0.85,0.15,1,0,0,0,0),nrow=5, byrow=TRUE))
A=firstPassage(Q2,"a",t)
b=0
for (i in 1:t){
  b=b+A[i,5]*i
}
print(b)
#rehabiliation strategy IS1
Q3<-new("markovchain", states=c("a","b","c","d","e"),transitionMatrix=matrix(c(0.91,0.09,0,0,0,0,0.95,0.05,0,0,0.8,0.2,0,0,0,0,0,0,0.85,0.15,0,0,0,0,1),nrow=5, byrow=TRUE))
B=firstPassage(Q3,"a",t)
b=0
for (i in 1:t){
  b=b+B[i,3]*i
}
print(b)

step=100
results=data.frame(B[seq(0,t,by=step),1],B[seq(0,t,by=step),2],B[seq(0,t,by=step),3],B[seq(0,t,by=step),4],B[seq(0,t,by=step),5])

file.remove("results.csv")
file.create("results.csv")
write.table(results, file="results.csv", sep = ",", append = TRUE,col.names = FALSE)

