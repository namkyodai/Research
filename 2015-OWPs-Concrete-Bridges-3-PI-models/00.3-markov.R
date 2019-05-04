# This program was coded by Christoph Schlegel based on a code by Nam Lethanh
# This program calculates the different optimal work programs, based on the input of the different deterioration models, on object and bridge level
#------------------------------------------------------

#------------------------------STATE PROBABILITY------------------------------

#..... define state probability
pi <- array(dim=c(ISmax,Nmax,YearMax,Omax))
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (r in 1:ISmax){
      for (i in 1:Nmax){
        pi[r,i,t,o] = 0
      }
    }
  }
}
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (r in 1:IS[o]){
      if (t==1){
        pi[r,,t,o] = ICO[o,]
      } else {
          pi[r,,t,o] = pi[r,,t-1,o]%*%Q[,,r,o]
        }
    }
  }
}

#..... define state probability for doing nothing
pidn <- array(dim=c(Omax,Nmax,YearMax))
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (i in 1:Nmax){
      pidn[o,i,t] = 0
    }
  }
}
for (o in 1:Omax){
  for (t in 1:YearMax){
      if (t==1){
        pidn[o,,t] = ICO[o,]
      } else {
          pidn[o,,t] = pidn[o,,t-1]%*%P[,,o]
        }
  }
}

#------------------------------------------------------
print(pi[,,YearMax,])
print(pidn[,,YearMax])
#------------------------------------------------------

#------------------------------COST FOR EACH INTERVENTIONSTRATEGY------------------------------

#..... calculate the evoluation of cost over the investigated period of time for each object and each intervention strategy
cost <- array(dim=c(ISmax,Nmax+1,YearMax,Omax))
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (r in 1:ISmax){
      for (i in 1:(Nmax+1)){
        cost[r,i,t,o] = 0
      }
    }
  }
}
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (r in 1:IS[o]){
      for (i in 1:(NCS[o]+1)){
        if (i<(NCS[o]+1)){
          cost[r,i,t,o] = pi[r,i,t,o]%*%C[r,i,o]%*%area[o]
        } else{  
            cost[r,i,t,o] = sum(cost[r,c(1:NCS[o]),t,o])
          }
      }
    }
  }
}

#------------------------------------------------------
print(cost)
#------------------------------------------------------

#------------------------------CONSTRUCTION OF WORKPRGRAM WITH AGENCY RULE------------------------------

piar <- array(dim=c(ISmax,Nmax,YearMax,Omax)) #: this is the state probability for each object in every year with agency rule
sumar <- array(dim=c(ISmax,YearMax,Omax)) #: this is the probability of beeing in the conditionstates where the condition for the agency rule is fulfilled
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (r in 1:ISmax){
      sumar[r,t,o] = 0
      for (i in 1:Nmax){
        piar[r,i,t,o] = 0
      }
    }
  }
}
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (r in 1:IS[o]){
      for (i in 1:NCS[o]){
        sumar[r,t,o] = sum(piar[r,c(SAR[o,r]:NCS[o]),t,o])
        if (t==1){
          piar[r,,t,o] = ICO[o,]
        } else {
            if (sumar[r,t-1,o]>=AR[o,r]){
              piar[r,,t,o] = piar[r,,t-1,o]%*%Q[,,r,o]
            } else {
                piar[r,,t,o] = piar[r,,t-1,o]%*%P[,,o]
              }
          }
        
      }
    }
  }
}   

#------------------------------------------------------
print(piar)
print(sumar)
#------------------------------------------------------

#..... cost for each object and each intervention strategy in every year
costar <- array(dim=c(ISmax,Nmax+1,YearMax,Omax))
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (r in 1:ISmax){
      for (i in 1:(Nmax+1)){
        costar[r,i,t,o] = 0
      }
    }
  }
}
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (r in 1:IS[o]){
      for (i in 1:(NCS[o]+1)){        
        if (i < (NCS[o]+1)){
          if (sumar[r,t,o] >= AR[o,r]){
            costar[r,i,t,o] = piar[r,i,t,o]%*%C[r,i,o]%*%area[o]
          } else {
              costar[r,i,t,o] = 0
            }
        } else {
            costar[r,i,t,o] = sum(costar[r,c(1:NCS[o]),t,o])
          }
      }
    }
  }
}

#------------------------------------------------------
print(costar)
#------------------------------------------------------

#..... summary of the cost for the hole object for each intervention strategy every year
Tcostar <- array(dim=c(ISmax,YearMax,Omax))
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (r in 1:ISmax){
      Tcostar[r,t,o] = 0
    }
  }
}
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (r in 1:ISmax){
      Tcostar[r,t,o] = costar[r,NCS[o]+1,t,o]
    }
  }
}

#..... summary of the cost for the hole object for each intervention strategy over all
Scostar <- array(dim=c(ISmax,Omax))
for (o in 1:Omax){
  for (r in 1:ISmax){
    Scostar[r,o] = 0
  }
}
for (o in 1:Omax){
  for (r in 1:ISmax){
    Scostar[r,o] = sum(Tcostar[r,,o])
  }
}

#------------------------------------------------------
print(Tcostar)
print(Scostar)
#------------------------------------------------------

#..... optimal intervention strategy for each object (based on costs)
OIScostar <- array(dim=c(Omax))
for (o in 1:Omax){
  OIScostar[o] = min(Scostar[c(1:IS[o]),o])
}

OISnrar <- array(dim=c(Omax))
for (o in 1:Omax){
  for (i in 1:IS[o]){
    if (OIScostar[o]==Scostar[i,o]){
      OISnrar[o] = i
    }
  }
}

#------------------------------------------------------
print(OIScostar)
print(OISnrar)
#------------------------------------------------------

#..... work program based on the different optimal intervention strategies
Wcostar <- array(dim=c(Omax,YearMax))
for (o in 1:Omax){
  for (t in 1:YearMax){
    Wcostar[o,t] = 0
  }
}
for (o in 1:Omax){
  for (t in 1:YearMax){
    for (r in 1:IS[o]){
      if (OIScostar[o]==Scostar[r,o]){
        Wcostar[o,t] = Tcostar[r,t,o]
      }
    }
  }
}

#..... total cost for the optimal workprogram based on the different OIS's
TWcostar <- array(dim=c(YearMax))
for (t in 1:YearMax){
  TWcostar[t] = sum(Wcostar[c(1:Omax),t])
}

#------------------------------------------------------
print(Wcostar)
print(TWcostar)
#------------------------------------------------------

#..... condition state distribution of the work program based on the different optimal IS's
Wpiar <- array(dim=c(Omax,Nmax,YearMax))
for (t in 1:YearMax){
  for (o in 1:Omax){
    for (i in 1:Nmax){
      Wpiar[o,i,t] = 0
    }
  }
}
for (t in 1:YearMax){
  for (o in 1:Omax){
    for (i in 1:NCS[o]){
      Wpiar[o,i,t] = piar[OISnrar[o],i,t,o]
    }
  }
}

#------------------------------------------------------
print(Wpiar)
#------------------------------------------------------

#------------------------------CONSTRUCTION OF WORKPRGRAM WITH AGENCY RULE AND "DISCOUNT"------------------------------

#..... maximum number of different work programs
W <- array(dim=c(Omax))
for (o in 1:Omax){
  W[o] = 0
}
for (o in 1:Omax){
  if (o==1){
    W[o] = IS[o]
  } else {
    W[o] = W[o-1]%*%IS[o]
    }
}

Wmax <- W[Omax] #: maximum number of different work programs

WN <- array(dim=c(Omax)) # help for the differerent work programs
WNN <- array(dim=c(Omax)) # help for the differerent work programs
for (o in 1:Omax){
  WNN[o] = 0
}
for (o in 1:Omax){
  if (o==1){
    WNN[o] = IS[Omax]
  } else {
    WNN[o] = WNN[o-1]%*%IS[Omax-(o-1)]
  }
} 
for (o in 1:Omax){
  WN[o] = WNN[Omax-(o-1)]
}

#..... generate all different combinations of intervention strategies for the work programs
WPC <- array(dim=c(Omax,Wmax))
for (o in 1:Omax){
  for (w in 1:Wmax){
    WPC[o,w] = 0
  }
}
for (w in 1:Wmax){
  m = Omax
  if (w==1){
      WPC[,w] = 1
  } else {
      while (m>=1){
        if (m==Omax){
          if (WPC[m,(w-1)]<IS[m]){
            WPC[m,w] = WPC[m,(w-1)] +1
          } else {
              WPC[m,w] = 1
            }
        m = m-1
        } else {
            if (w<=WN[m+1]){
              WPC[m,w] = 1
            } else {
                if (mean(WPC[m,c((w-WN[m+1]):(w-1))])==IS[m]){
                  WPC[m,w] = 1
                } else if (mean(WPC[m,c((w-WN[m+1]):(w-1))])==WPC[m,(w-1)]){
                    WPC[m,w] = WPC[m,(w-1)] + 1
                  } else {
                      WPC[m,w] = WPC[m,(w-1)] 
                    }
              }
          m = m - 1
          }
      }
    }
}

#------------------------------------------------------
print(WPC)
#------------------------------------------------------

#..... cost for all possible work programs
WPcostar <- array(dim=c(Omax+1,YearMax,Wmax))
for (w in 1:Wmax){
  for (t in 1:YearMax){
    for (o in 1:(Omax+1)){
      WPcostar[o,t,w] = 0
    }
  }
}
for (w in 1:Wmax){
  for (t in 1:YearMax){
    for (o in 1:(Omax+1)){
      if (o<=Omax){
        WPcostar[o,t,w] = Tcostar[WPC[o,w],t,o]
      } else {
          WPcostar[o,t,w] = sum(WPcostar[c(1:Omax),t,w])
        }
    }
  }
}

Rabcost <- array(dim=c(YearMax,Wmax))
for (w in 1:Wmax){
  for (t in 1:YearMax){
    Rabcost[t,w] = 0
  }
}
for (w in 1:Wmax){
  for ( t in 1:YearMax){
    Rabcost[t,w] = length(which(WPcostar[c(1:Omax),t,w]>0))
  }
}

#..... generate work program costs with discount due to the fact, that more than one object has to do an intervention
WPcostard <- array(dim=c(Omax+1,YearMax,Wmax))
for (w in 1:Wmax){
  for (t in 1:YearMax){
    for (o in 1:(Omax+1)){
      WPcostard[o,t,w] = 0
    }
  }
}
for (w in 1:Wmax){
  for (t in 1:YearMax){
    for (o in 1:(Omax+1)){
      if (o<=Omax){
        if (Rabcost[t,w]==0){
          WPcostard[o,t,w] = WPcostar[o,t,w]
        } else {
            WPcostard[o,t,w] = WPcostar[o,t,w]%*%(1-Discount[Rabcost[t,w]])
          }    
      } else {
          WPcostard[o,t,w] = sum(WPcostard[c(1:Omax),t,w])
        }
    }
  }
}

#------------------------------------------------------
print(WPcostar)
print(WPcostard)
#------------------------------------------------------

#..... total cost for each work program
TWPcostard <- array(dim=c(Wmax))
for (w in 1:Wmax){
  TWPcostard[w] = 0
}
for (w in 1:Wmax){
  TWPcostard[w] = sum(WPcostard[(Omax+1),,w])
}

#------------------------------------------------------
print(TWPcostard)
#------------------------------------------------------

#..... find optimal work program with discount
OWPcost <- min(TWPcostard)

OWPnr <- array(dim=c(1))
for (w in 1:Wmax){
  if (OWPcost==TWPcostard[w]){
    OWPnr = w
  }
}

OISnrard <- array(dim=c(Omax))
for (o in 1:Omax){
    OISnrard[o] = WPC[o,OWPnr]
}

#------------------------------------------------------
print(OWPcost)
print(OWPnr)
print(OISnrard)
#------------------------------------------------------

#..... optimal work program with discount
OWPcostard <- array(dim=c(Omax,YearMax))
for (o in 1:Omax){
  for (t in 1:YearMax){
    OWPcostard[o,t] = WPcostard[o,t,OWPnr]
  }
}

OWPpiard <- array(dim=c(Omax,Nmax,YearMax))
for (t in 1:YearMax){
  for (o in 1:Omax){
    for (i in 1:Nmax){
      OWPpiard[o,i,t] = 0
    }
  }
}
for (t in 1:YearMax){
  for (o in 1:Omax){
    for (i in 1:NCS[o]){
      OWPpiard[o,i,t] = piar[OISnrard[o],i,t,o]
    }
  }
}

#.... total intervention cost for every year based on the OWP
TOWPcostard <- array(dim=c(YearMax))
for (t in 1:YearMax){
  TOWPcostard[t] = sum(OWPcostard[c(1:Omax),t])
}

#------------------------------------------------------
print(OWPcostard)
print(OWPpiard)
print(TOWPcostard)
#------------------------------------------------------

#------------------------------------------------------
#..... anual cost for the work program based on the different optimal IS's
AWcostar <- sum(TWcostar)/YearMax

#..... anual cost for the work program based on the different optimal IS's
AOWPcostard <- sum(TOWPcostard)/YearMax

#------------------------------THE END------------------------------