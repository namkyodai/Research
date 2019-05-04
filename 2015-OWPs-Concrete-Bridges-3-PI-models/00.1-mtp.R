# This program is coded by Christoph Schlegl
# This program calculates the transition probability matrix out of the hazard rates (MUSTEM model), according to TSUDA et al. [2006]
#------------------------------------------------------

mtp <- function(NCS,z,theta){

probb <- array(dim=c(Nmax,Nmax,Omax))
for (o in 1:Omax){
  for (i in 1:Nmax){
    for (j in 1:Nmax){
      probb[i,j,o] = 0
    }
  }
}

#------------------------------------------------------

for (o in 1:Omax){
  for (i in 1:NCS[o]){
    for (j in 1:NCS[o]){
      k = i
      p = 0
      while (k<=j){
        m = i
        prod1 = 1
        while (m<=(k-1)){
          prod11 = theta[o,m]/(theta[o,m]-theta[o,k])
          prod1 = prod1*prod11
          m = m+1
        }
        n = k
        prod2 = 1
        while (n<=(j-1)){
          prod22 = theta[o,n]/(theta[o,n+1]-theta[o,k])
          prod2 = prod2*prod22
          n = n+1
        }
        pp = prod1*prod2*exp(-theta[o,k]*z)
        p = p + pp
        k = k+1
      }
      probb[i,j,o] = p
    }
  }
}
return(probb)
}

#------------------------------THE END------------------------------