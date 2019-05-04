# This program was coded by Christoph Schlegel
# This program generates the optimal work program on object and bridge level based on the WEAR-OUT deterioration process
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

#..... read transition probability and the initial condition state for the different objects
P1 <- read.csv("03-WEAROUT/03.1-P1.csv",header=TRUE,sep=";") #: transition probability for object 1
P2 <- read.csv("03-WEAROUT/03.2-P2.csv",header=TRUE,sep=";") #: transition probability for object 2
P3 <- read.csv("03-WEAROUT/03.3-P3.csv",header=TRUE,sep=";") #: transition probability for object 3

#------------------------------TRANSITION PROBABILITY------------------------------

P <- array(dim=c(Nmax,Nmax,Omax)) #: transition probability for each object
for (o in 1:Omax){
  for (i in 1:Nmax){
    for (j in 1:Nmax){
      P[i,j,o] = 0
    }
  }
}
for (o in 1:Omax){
  for (i in 1:NCS[o]){
    for (j in 1:NCS[o]){
      if (o==1){
        P[i,j,o] = P1[i,j] #: read transition probability for object 1
      } else if (o==2){
          P[i,j,o] = P2[i,j] #: read transition probability for object 2
        } else {
            P[i,j,o] = P3[i,j] #: read transition probability for object 3
          }
    }
  }
}

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
      ICO[o,i] = P1[i,NCS[o]+1] #: read initial condition state for object 1
    } else if (o==2){
        ICO[o,i] = P2[i,NCS[o]+1] #: read initial condition state for object 2
      } else {
          ICO[o,i] = P3[i,NCS[o]+1] #: read initial condition state for object 3
        }
  }
}

source("00.2-inp.R") #: source the program to calculate the work programs

#------------------------------SAVE THE RESULTS------------------------------

file.remove("03-WEAROUT/03.1-resultsOWP.csv")
file.create("03-WEAROUT/03.1-resultsOWP.csv")

file.remove("03-WEAROUT/03.2-resultsOIS.csv")
file.create("03-WEAROUT/03.2-resultsOIS.csv")

write.table(OWPcostard, file="03-WEAROUT/03.1-resultsOWP.csv", sep = ";", append = TRUE,col.names = FALSE)

write.table(Wcostar, file="03-WEAROUT/03.2-resultsOIS.csv", sep = ";", append = TRUE,col.names = FALSE)

#------------------------------------------------------

cat("annual costs for optimal work program on object level \n")
print(AWcostar)

cat("annual costs for optimal work program on bridge level \n")
print(AOWPcostard)

#------------------------------THE END------------------------------