#parameters
param v; #setup cost
param d; #deterioration rate
param c; #intervention cost
param i; #penalty cost
# Decision variables 
var x >= 0.00001; #condition state added
# Objective function 
minimize annualcost: (v*d/x+c*d+c*x*i/2); 

