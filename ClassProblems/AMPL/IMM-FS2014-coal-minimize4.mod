set TYPE; 
set POLLUTANT;
param S {TYPE};
param e {TYPE, POLLUTANT} >= 0; #
param f1 {TYPE} >= 0; #sulfur
param f2 {TYPE} >= 0; #dust
param E {POLLUTANT} >= 0; 
param w1; #weight for objective function 1
param w2; #weight for objective function 2
param w3; #weight for objective function 3
param Z1; #optimal value of objective function 1
param Z2; #optimal value of objective function 2
param Z3; #optimal value of objective function 3

# Decision variables 
var x {TYPE} >=0; 
var d1; # deviation for obj 1
var d2; #deviation for obj 2
var d3; #deviation for obj 3


# Objective function 
minimize deviation:   w1*d1+w2*d2+w3*d3; 

subject to limitpollu {j in POLLUTANT}:    sum {i in TYPE} x[i]*e[i,j] <= E[j]; 
subject to percent:    sum {i in TYPE} x[i] = 1; 
subject to deviation1 : d1=(-(sum {i in TYPE}  x[i]*S[i]+Z1)/Z1);
subject to deviation2 : d2=((Z2-sum {i in TYPE} x[i]*f1[i])/Z2);
subject to deviation3 : d3=((Z3-sum {i in TYPE} x[i]*f2[i])/Z3);