# This program was coded by  Nam Lethanh (lethanh@ibi.baug.ethz.ch)
#Steady state Markov model
#Intervention strategy for bridge with Markov model
#------------------------------------------------------
rm(list=ls()) #clear the memory and objects in R console
###            INPUT      
#---------Use have to install package "markovchain" in order to run this script
library(markovchain)
statesNames=c("1","2","3","4","5")
#do nothing strategy IS0
Q1<-matrix(c(0.91,0.09,0,0,0,0,0.95,0.05,0,0,0,0,0.94,0.06,0,0,0,0,0.85,0.15,0,0,0,0,1),nrow=5, byrow=TRUE)
#rehabiliation strategy IS1
Q2<-matrix(c(0.91,0.09,0,0,0,0,0.95,0.05,0,0,0.8,0.2,0,0,0,0,0,0,0.85,0.15,0,0,0,0,1),nrow=5, byrow=TRUE)
#replacement strategy IS2
Q3<-matrix(c(0.91,0.09,0,0,0,0,0.95,0.05,0,0,0,0,0.94,0.06,0,0,0,0,0.85,0.15,1,0,0,0,0),nrow=5, byrow=TRUE)

Q1<-new("markovchain", states=statesNames, transitionMatrix=Q1)
Q2<-new("markovchain", states=statesNames, transitionMatrix=Q2)
Q3<-new("markovchain", states=statesNames, transitionMatrix=Q3)
cat('steady state for IS0')
print(t(steadyStates(Q1)))
cat('steady state for IS1')
print(t(steadyStates(Q3)))
cat('steady state for IS2')
print(t(steadyStates(Q2)))
