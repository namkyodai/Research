# This program was coded by Christoph Schlegel based on a program code by Nam Lethanh
# This program reads the general input needed for the construction of the optimal work programs and generates the graphs as output of the calculations
# This program is used for all three different deterioration processes
#------------------------------------------------------

stackedPlot <- function(data, time=NULL, col=1:length(data),...) {
  if (is.null(time)) {
    time <- 1:length(data[[1]])
    plot(0,0, xlim = range(time), ylim = c(0,max(rowSums(data))), axes=FALSE,t="n" , ...)
    axis(2, ylim=c(0,1),col="black",las=1)
    axis(1,c(seq(1,YearMax,by=1)),c(seq(1,YearMax,by=1)))
    box()
    
    for (i in length(data):1) {
      #..... the summup to the current collumn
      prep.data <- rowSums(data[1:i]);
      #..... the polygon must have his first and last point on the zero line
      prep.y <- c(0, prep.data,0)
      prep.x <- c(time[1], time, time[length(time)] )
      polygon(prep.x, prep.y, col=col[i], border = NA);
    }
  }
}

colors <- c("aliceblue","coral1","limegreen","orange","yellow2")

#------------------------------------------------------

stp <- array(dim=c(Omax,Nmax)) #: define for each object initial condition state equal to 1
for (o in 1:Omax){
  for (i in 1:Nmax){
    if (i==1){
      stp[o,i] = 1
    } else {
        stp[o,i] = 0
    }
  }
}

#..... state probability in every year
csstate <- array(dim=c(YearMax,Nmax,Omax))
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (i in 1:Nmax){
      csstate[t,i,o] = 0
    }
  }
}
for (o in 1:Omax){
  for (t in 1:YearMax){
    if (t==1) {
      csstate[t,,o] = stp[o,]
    } else {
        csstate[t,,o] = csstate[(t-1),,o]%*%P[,,o]
    }
  }
}
#------------------------------------------------------

#..... state probability over time for the different objects
plot.new()
stackedPlot(data.frame(csstate[,,1]),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Deck Condition state distribution - life time", col.main="red", font.main=4)

plot.new()
stackedPlot(data.frame(csstate[,,2]),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Pier Condition state distribution - life time", col.main="red", font.main=4)

plot.new()
stackedPlot(data.frame(csstate[,,3]),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Abutment Condition state distribution - life time", col.main="red", font.main=4)

#------------------------------------------------------

#..... read the intervention strategies, agency rule and condition for the agency rule for each interventions strategy
I1 <- read.csv("00-INPUT/00.1-I1.csv",header=TRUE,sep=";") #: Intervention strategies for object 1
I2 <- read.csv("00-INPUT/00.2-I2.csv",header=TRUE,sep=";") #: Intervention strategies for object 2
I3 <- read.csv("00-INPUT/00.3-I3.csv",header=TRUE,sep=";") #: Intervention strategies for object 3

IS1 <- length(I1[,1]) #: number of intervention strategies for object 1
IS2 <- length(I2[,1]) #: number of intervention strategies for object 2
IS3 <- length(I3[,1]) #: number of intervention strategies for object 3

IS <- cbind(IS1,IS2,IS3)
ISmax <- max(IS) #: maximum number of intervention strategies of all objects

I <- array(dim=c(ISmax,Nmax,Omax)) #: prepare intervention informations for calculations
for (r in 1:ISmax){
  for (j in 1:Nmax){
    for (o in 1:Omax){
      I[r,j,o] = 0
    }
  }
}
for (o in 1:Omax){
  for (r in 1:IS[o]){
    for (j in 1:NCS[o]){
      if (o==1){
        I[r,j,o] = I1[r,j+1]
      } else if (o==2){
        I[r,j,o] = I2[r,j+1]
      } else {
        I[r,j,o] = I3[r,j+1]
      }
    }
  }
}

AR <- array(dim=c(Omax,ISmax)) #: agency rule for each object and intervention strategy
for (o in 1:Omax){
  for (r in 1:ISmax){
    AR[o,r] = 0
  }
}
for (o in 1:Omax){
  for (r in 1:IS[o]){
    if (o==1){
      AR[o,r] = I1[r,NCS[o]+2]
    } else if (o==2){
      AR[o,r] = I2[r,NCS[o]+2]
    } else {
      AR[o,r] = I3[r,NCS[o]+2]
    }
  }
}

SAR <- array(dim=c(Omax,ISmax)) #: condition of the agency rule for each object and intervention strategy
for (o in 1:Omax){
  for (r in 1:ISmax){
    SAR[o,r] = 0
  }
}
for (o in 1:Omax){
  for (r in 1:IS[o]){
    if (o==1){
      SAR[o,r] = I1[r,NCS[o]+3]
    } else if (o==2){
      SAR[o,r] = I2[r,NCS[o]+3]
    } else {
      SAR[o,r] = I3[r,NCS[o]+3]
    }
  }
}

#...... read effectiveness matrix and intervention cost for each object
R1 <- read.csv("00-INPUT/00.1-R1.csv",header=TRUE,sep=";")
R2 <- read.csv("00-INPUT/00.2-R2.csv",header=TRUE,sep=";")
R3 <- read.csv("00-INPUT/00.3-R3.csv",header=TRUE,sep=";")

IT1 <- length(R1[,1]) #: number of different intervention types for object 1
IT2 <- length(R2[,1]) #: number of different intervention types for object 2
IT3 <- length(R3[,1]) #: number of different intervention types for object 3

IT <- cbind(IT1,IT2,IT3)
ITmax <- max(IT) #: maximum number of intervention types for all objects

R <- array(dim=c(ITmax,Nmax+2,Nmax,Omax)) #: effectiveness matrix and Intervention cost for each object and any intervention
for (o in 1:Omax){
  for (c in 1:Nmax){
    for (i in 1:ITmax){
      for (j in 1:(Nmax+2)){
        R[i,j,c,o] = 0
      }
    }
  }
}
for (o in 1:Omax){
  for (c in 1:NCS[o]){
    for (i in 1:IT[o]){
      for (j in 1:(NCS[o]+2)){
        if (o==1){
          R[i,j,c,o] = R1[i,j+(c-1)*(NCS[o]+2)] #: read effectiveness and costs for object 1
        } else if (o==2){
          R[i,j,c,o] = R2[i,j+(c-1)*(NCS[o]+2)] #: read effectiveness and costs for object 2
        } else {
          R[i,j,c,o] = R3[i,j+(c-1)*(NCS[o]+2)] #: read effectiveness and costs for object 3
        }
      }
    }
  }
}

ITU <- array(dim=c(ITmax,Omax)) #: Intervention types used for each object
for (o in 1:Omax){
  ITU[,o] = R[,1,1,o]
}

#..... transition probability matrix for each intervention strategy and object
Q <- array(dim=c(Nmax,Nmax,ISmax,Omax))
for (o in 1:Omax){
  for (r in 1:ISmax){
    for (i in 1:Nmax){
      for (j in 1:Nmax){
        Q[i,j,r,o] = 0
      }
    }
  }
}
for (o in 1:Omax){
  for (r in 1:IS[o]){
    for (i in 1:NCS[o]){
      if (I[r,i,o]==0){
        for (j in 1:NCS[o]){
          Q[i,j,r,o] = P[i,j,o]
        }
      } else {
        for (l in 1:IT[o]){
          if (ITU[l,o]==I[r,i,o]){
            for (j in 1:NCS[o]){
              Q[i,j,r,o] = R[l,j+1,i,o]
            }
          }
        }
      }
    }  
  }
}

#..... cost for each intervention strategy in each condition state for each object
C <- array(dim=c(ISmax,Nmax,Omax))
for (r in 1:ISmax){
  for (i in 1:Nmax){
    for (o in 1:Omax){
      C[r,i,o] = 0
    }
  }
}
for (o in 1:Omax){
  for (r in 1:IS[o]){
    for (i in 1:NCS[o]){
      if (I[r,i,o]==0){
        C[r,i,o] = 0
      } else {
        for (l in 1:IT[o]){
          if (ITU[l,o]==I[r,i,o]){
            C[r,i,o] = R[l,NCS[o]+2,i,o]
          }
        }
      }  
    }
  }
}

#------------------------------------------------------
print(P)
print(I)
print(R)
print(Q)
print(C)
#------------------------------------------------------

#------------------------------RUN CALCULATIONS------------------------------

source("00.3-markov.R")

#------------------------------------------------------

#------------------------------DETERIORATION DISTRIBUTION------------------------------

#..... state probability for IS "do nothing" for the different objects
plot.new()
stackedPlot(data.frame(t(pidn[1,,c(1:YearMax)])),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Deck Condition state distribution - Do nothing", col.main="red", font.main=4)

plot.new()
stackedPlot(data.frame(t(pidn[2,,c(1:YearMax)])),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Pier Condition state distribution - Do nothing", col.main="red", font.main=4)

plot.new()
stackedPlot(data.frame(t(pidn[3,,c(1:YearMax)])),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Abutment Condition state distribution - Do nothing", col.main="red", font.main=4)

#..... state probability for optimal intervention strategy for the different objects based on costs and agency rule
plot.new()
stackedPlot(data.frame(t(Wpiar[1,,c(1:YearMax)])),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Deck Condition state distribution - OIS", col.main="red", font.main=4)

plot.new()
stackedPlot(data.frame(t(Wpiar[2,,c(1:YearMax)])),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Pier Condition state distribution - OIS", col.main="red", font.main=4)

plot.new()
stackedPlot(data.frame(t(Wpiar[3,,c(1:YearMax)])),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Abutment Condition state distribution - OIS", col.main="red", font.main=4)

#..... state probability for optimal intervention strategy for the different objects based on costs agency rule and discount
plot.new()
stackedPlot(data.frame(t(OWPpiard[1,,c(1:YearMax)])),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Deck Condition state distribution - OWP", col.main="red", font.main=4)

plot.new()
stackedPlot(data.frame(t(OWPpiard[2,,c(1:YearMax)])),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Pier Condition state distribution - OWP", col.main="red", font.main=4)

plot.new()
stackedPlot(data.frame(t(OWPpiard[3,,c(1:YearMax)])),col=colors,xlab="Time (tus)",ylab="Probability")
legend("topright", inset=0.09, title="States",col=colors,lty=2,lwd=13,legend=c(1:Nmax),bg="azure2",cex=0.7)
title(main="Abutment Condition state distribution - OWP", col.main="red", font.main=4)

#------------------------------------------------------

#------------------------------INTERVENTIONCOST DISTRIBUTION------------------------------

color <- c("aliceblue","limegreen","orange")
par(mar=c(5,4,4,6)+0.3)
scale <- 1000000
limy <- c(0,round_any(max(TWcostar),100000, f=ceiling))
limx <- c(1,YearMax)

#..... interventioncost for optimal intervention for each object with agency rule
plot.new()
plot(Wcostar[1,],lwd=2,col="cyan3",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="h",pch=5,cex=0.3)
axis(2,c(seq(0,scale,by=100000)),c(seq(0,scale/100000,by=1)))
axis(1,c(seq(1,YearMax,by=1)),c(seq(1,YearMax,by=1)))
mtext(expression(paste('Impacts (',x10^5, 'mus)')),side=2,col="black",line=3)
mtext(expression(paste('Time (Year)')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
title(main="Deck Intervention cost - OIS", col.main="red", font.main=4)

plot.new()
plot(Wcostar[2,],lwd=2,col="cyan3",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="h",pch=5,cex=0.3)
axis(2,c(seq(0,scale,by=100000)),c(seq(0,scale/100000,by=1)))
axis(1,c(seq(1,YearMax,by=1)),c(seq(1,YearMax,by=1)))
mtext(expression(paste('Impacts (',x10^5, 'mus)')),side=2,col="black",line=3)
mtext(expression(paste('Time (Year)')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
title(main="Pier Intervention cost - OIS", col.main="red", font.main=4)

plot.new()
plot(Wcostar[3,],lwd=2,col="cyan3",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="h",pch=5,cex=0.3)
axis(2,c(seq(0,scale,by=100000)),c(seq(0,scale/100000,by=1)))
axis(1,c(seq(1,YearMax,by=1)),c(seq(1,YearMax,by=1)))
mtext(expression(paste('Impacts (',x10^5, 'mus)')),side=2,col="black",line=3)
mtext(expression(paste('Time (Year)')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
title(main="Abutment Intervention cost - OIS", col.main="red", font.main=4)

plot.new()
plot(TWcostar[],lwd=2,col="cyan3",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="h",pch=5,cex=0.3)
axis(2,c(seq(0,scale,by=100000)),c(seq(0,scale/100000,by=1)))
axis(1,c(seq(1,YearMax,by=1)),c(seq(1,YearMax,by=1)))
mtext(expression(paste('Impacts (',x10^5, 'mus)')),side=2,col="black",line=3)
mtext(expression(paste('Time (Year)')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
title(main="Bridge Intervention cost - OIS", col.main="red", font.main=4)

#..... interventioncost for optimal work program for each object with agency rule and discount
plot.new()
plot(OWPcostard[1,],lwd=2,col="cyan3",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="h",pch=5,cex=0.3)
axis(2,c(seq(0,scale,by=100000)),c(seq(0,scale/100000,by=1)))
axis(1,c(seq(1,YearMax,by=1)),c(seq(1,YearMax,by=1)))
mtext(expression(paste('Impacts (',x10^5, 'mus)')),side=2,col="black",line=3)
mtext(expression(paste('Time (Year)')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
title(main="Deck Intervention cost - OWP", col.main="red", font.main=4)

plot.new()
plot(OWPcostard[2,],lwd=2,col="cyan3",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="h",pch=5,cex=0.3)
axis(2,c(seq(0,scale,by=100000)),c(seq(0,scale/100000,by=1)))
axis(1,c(seq(1,YearMax,by=1)),c(seq(1,YearMax,by=1)))
mtext(expression(paste('Impacts (',x10^5, 'mus)')),side=2,col="black",line=3)
mtext(expression(paste('Time (Year)')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
title(main="Pier Intervention cost - OWP", col.main="red", font.main=4)

plot.new()
plot(OWPcostard[3,],lwd=2,col="cyan3",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="h",pch=5,cex=0.3)
axis(2,c(seq(0,scale,by=100000)),c(seq(0,scale/100000,by=1)))
axis(1,c(seq(1,YearMax,by=1)),c(seq(1,YearMax,by=1)))
mtext(expression(paste('Impacts (',x10^5, 'mus)')),side=2,col="black",line=3)
mtext(expression(paste('Time (Year)')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
title(main="Abutment Intervention cost - OWP", col.main="red", font.main=4)

plot.new()
plot(TOWPcostard[],lwd=2,col="cyan3",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="h",pch=5,cex=0.3)
axis(2,c(seq(0,scale,by=100000)),c(seq(0,scale/100000,by=1)))
axis(1,c(seq(1,YearMax,by=1)),c(seq(1,YearMax,by=1)))
mtext(expression(paste('Impacts (',x10^5, 'mus)')),side=2,col="black",line=3)
mtext(expression(paste('Time (Year)')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
title(main="Bridge Intervention cost - OWP", col.main="red", font.main=4)

#------------------------------THE END------------------------------