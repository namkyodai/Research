#Data file: IMM-power-genenator.mod
set GENERATOR;
param T; # No. of years
param P;   # Initial capacity (MW)
param Q {t in 1..T} >= 0;   # minimum yearly required capacity
param c{i in GENERATOR,t in 1..T}; # Yearly generating cost (CHF)
param G{i in GENERATOR}; # Capacity of generator i (MW)
var x {i in GENERATOR, t in 1..T} integer >=0;  # No. of generator to buy
var H {t in 1..T} :>=0 ;

minimize purchasingcost:
   sum {i in GENERATOR,t in 1..T} c[i,t] * x[i,t];
subject to minimumcapacity {t in 1..T}: H[t] >= Q[t];
subject to year1 {t in 1..T}: H[1]= P;
subject to year2 {t in 2..T}: H[t]= H[t-1]+ sum{i in GENERATOR}x[i]*G[i];




