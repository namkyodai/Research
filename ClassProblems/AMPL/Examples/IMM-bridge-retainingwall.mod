#Coded by Nam Lethanh. It is used for the course "IMM-FS2014".  
# AMPL language
# April, 2014
#-----------------------------------------------------------------------
#in order to ensure that all 
set	OBJ; # object type (e.g. bridge or retaining wall
param c{OBJ}; #cost of intervention on i
param b{OBJ}; # benefit
param t{OBJ}; #unit time require

param B; # total budget
param TIME; #total time

var x{OBJ} integer >=0; # number of object type

#objective function: Maximize the total benefit
 maximize total_benefit: sum{i in OBJ} x[i]*b[i];
#constraint: 
subject to 
# constraint
budget: sum{i in OBJ} x[i]*c[i] <=B;
totaltime: sum{i in OBJ} x[i]*t[i] <=TIME;
#end
