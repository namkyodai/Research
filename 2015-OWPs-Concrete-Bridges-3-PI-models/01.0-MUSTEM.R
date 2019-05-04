# This program was coded by Christoph Schlegel based on a program coded by Nam Lethanh
# This program generates the optimal work program on object and bridge level based on the MUSTEM deterioration process

# Use of the program:
# 1) Define general inputs for the optimization model:
#     00.0-objects, define area & number of condition states for each object and the discount factor for bridge level
#     00.x-Ix, define the different intervention strategies for the objects (x), intervention strategy has to include the intervention strategy be followed in each condition state and the agency rule
#     00.x-Rx, define the effectiveness matrix and the according costs for each intervention type at each condtion state
# 2) Define model specific input:
#     03.x-Px, define the hazard rates and the initial condition state for the objects
# 3) Run this program to get the output (OWPs)

#------------------------------------------------------
rm(list=ls()) #: clear the memory and objects in R console
#------------------------------------------------------
library(plyr)
#------------------------------------------------------

#------------------------------INPUT------------------------------  

YearMax <- 100 #: number of year

#..... read objects, their area, their initial condition states and the discount rate
obj <- read.csv("00-INPUT/00.0-objects.csv",header=TRUE,sep=";")

Omax <- length(obj[,1]) #: number of objects

NCS <- obj[,3] #: numer of condition states for each object
Nmax <- max(NCS) #: maximum numer of condition stats of all objects
area <- obj[,2] #: surface area of each object
Discount <- obj [,4] #: discount due to the fact that more than one object has an intervention

#..... read hazard rate and the initial condition state for the different objects
P1 <- read.csv("01-MUSTEM/01.1-P1.csv",header=TRUE,sep=";") #: hazard rate for object 1
P2 <- read.csv("01-MUSTEM/01.2-P2.csv",header=TRUE,sep=";") #: hazard rate for object 2
P3 <- read.csv("01-MUSTEM/01.3-P3.csv",header=TRUE,sep=";") #: hazard rate for object 3

H <- array(dim=c(Omax,Nmax)) #: hazard rate for each object
for (o in 1:Omax){
  for (i in 1:Nmax){
      H[o,i] = 0
  }
}
for (o in 1:Omax){
  for (i in 1:NCS[o]){
    if (o==1){
      H[o,i] = P1[i,1] #: read hazard rate for object 1
    } else if (o==2){
        H[o,i] = P2[i,1] #: read hazard rate for object 2
      } else {
          H[o,i] = P3[i,1] #: read hazard rate for object 3
        }
  }
}

#..... if there are some of the hazard rates the same the program can't calculate the transition probability, if there are some the same they're change by 0.00001
for (o in 1:Omax){
  for (i in 1:(NCS[o]-1)){
    if (i==1){
      H[o,i] = H[o,i]
    } else {
      for (j in 1:(i-1)){
        if (H[o,i]==H[o,j]){
          H[o,i] = H[o,i] + 0.00001
        } else {
            H[o,i] = H[o,i]
        }
      }
    }
  }
}

#------------------------------TRANSITION PROBABILITY------------------------------

z <- 1 #: time intervall

source("00.1-mtp.R") #: source the program to calculate the transition probability

P <- mtp(NCS,z,H) #: transition probability for each object

RMD <- array(dim=c(Omax,(Nmax-1))) #: duration of each condition state
CRMD <- array(dim=c(Omax,Nmax)) #: cummulative life time of condition state

#..... subroutine to compute the life expectancy based on hazard rate.
for (o in 1:Omax){
  for (i in 1:(Nmax-1)){
    RMD[o,i] = 0
  }
}
for (o in 1:Omax){
  for (i in 1:(NCS[o]-1)){
    RMD[o,i] = 1/H[o,i]
  }
}
 
#..... subroutine to compute the cummulative life expectancy for plotting
for (o in 1:Omax){
  for (i in 1:Nmax){
    CRMD[o,i] = 0
  }
}
for (o in 1:Omax){
  for (i in 1:NCS[o]){
    if (i==1){
      CRMD[o,i] = 0
    } else {
        CRMD[o,i]=CRMD[o,(i-1)] + RMD[o,(i-1)]
      }
  }
}

#------------------------------------------------------

#..... plot the life time for object 1
plot.new()
plot(CRMD[1,],c(1:NCS[1]),xlim=c(0,CRMD[1,NCS[1]]),ylim=rev(range(c(1,NCS[1]))),xlab="",ylab="",type="p")
predict(smooth.spline(CRMD[1,],c(1:NCS[1]),df=4.5))
lines(predict(smooth.spline(CRMD[1,],c(1:NCS[1]),df=4.5),x=seq(0,CRMD[1,NCS[1]],length=100)),col="red",lwd=2)
title(main="Deterioration curve Deck", col.main="red", font.main=4)
title(xlab="Time (years)", col.lab=rgb(0,0.5,0))
title(ylab="Condition states", col.lab=rgb(0,0.5,0))

#..... plot the life time for object 2
plot.new()
plot(CRMD[2,c(1:NCS[2])],c(1:NCS[2]),xlim=c(0,CRMD[2,NCS[2]]),ylim=rev(range(c(1,NCS[2]))),xlab="",ylab="",type="p")
predict(smooth.spline(CRMD[2,c(1:NCS[2])],c(1:NCS[2]),df=3.5))
lines(predict(smooth.spline(CRMD[2,c(1:NCS[2])],c(1:NCS[2]),df=3.5),x=seq(0,CRMD[2,NCS[2]],length=100)),col="red",lwd=2)
title(main="Deterioration curve Pier", col.main="red", font.main=4)
title(xlab="Time (years)", col.lab=rgb(0,0.5,0))
title(ylab="Condition states", col.lab=rgb(0,0.5,0))

#..... plot the life time for object 3
#plot.new()
#plot(CRMD[3,c(1:NCS[3])],c(1:NCS[3]),xlim=c(0,CRMD[3,NCS[3]]),ylim=rev(range(c(1,NCS[3]))),xlab="",ylab="",type="p")
#predict(smooth.spline(CRMD[2,c(1:NCS[2])],c(1:NCS[2]),df=3.5))
#lines(predict(smooth.spline(CRMD[3,c(1:NCS[3])],c(1:NCS[3]),df=3.5),x=seq(0,CRMD[3,NCS[3]],length=100)),col="red",lwd=2)
#title(main="Deterioration curve Abutment", col.main="red", font.main=4)
#title(xlab="Time (years)", col.lab=rgb(0,0.5,0))
#title(ylab="Condition states", col.lab=rgb(0,0.5,0))

#------------------------------------------------------

#..... read the initial condition states for each object
ICO <- array(dim=c(Omax,Nmax))
for (o in 1:Omax){
  for (i in 1:Nmax){
    ICO[o,i] = 0
  }
}
for (o in 1:Omax){
  for (i in 1:NCS[o]){
    if (o==1){
      ICO[o,i] = P1[i,2] #: read initial condition state for object 1
    } else if (o==2){
        ICO[o,i] = P2[i,2] #: read initial condition state for object 2
      } else {
          ICO[o,i] = P3[i,2] #: read initial condition state for object 3
        }
  }
}

source("00.2-inp.R") #: source the program to calculate the work programs

#------------------------------SAVE THE RESULTS------------------------------

file.remove("01-MUSTEM/01.1-resultsOWP.csv")
file.create("01-MUSTEM/01.1-resultsOWP.csv")

file.remove("01-MUSTEM/01.2-resultsOIS.csv")
file.create("01-MUSTEM/01.2-resultsOIS.csv")

write.table(OWPcostard, file="01-MUSTEM/01.1-resultsOWP.csv", sep = ";", append = TRUE,col.names = FALSE)

write.table(Wcostar, file="01-MUSTEM/01.2-resultsOIS.csv", sep = ";", append = TRUE,col.names = FALSE)

#------------------------------------------------------

cat("annual costs for optimal work program on object level \n")
print(AWcostar)

cat("annual costs for optimal work program on bridge level \n")
print(AOWPcostard)

#------------------------------THE END------------------------------