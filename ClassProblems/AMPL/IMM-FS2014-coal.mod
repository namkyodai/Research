set TYPE; 
set POLLUTANT;
param S {TYPE};
param e {TYPE, POLLUTANT} >= 0; #
param E {POLLUTANT} >= 0; 
# Decision variables 
var x {TYPE} >=0; 
# Objective function 
maximize totalsteam:   sum {i in TYPE}  x[i]*S[i]; 
subject to limitpollu {j in POLLUTANT}:    sum {i in TYPE} x[i]*e[i,j] <= E[j]; 
subject to percent:    sum {i in TYPE} x[i] = 1; 