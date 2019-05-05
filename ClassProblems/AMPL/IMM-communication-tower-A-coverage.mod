param T;
#parameters
param c; # unit construction cost for one tower
param R {1..T,1..T}; 

# Decision variables 
var x{1..T,1..T} binary;  #area to be covered
var y{1..T,1..T} binary;  #location of tower in which area
var z{1..T,1..T} integer >=0;  #times cover by areas

# Objective function 
maximize profit: sum {i in 1..T,j in 1..T} (x[i,j]*R[i,j]-y[i,j]*c); 

subject to coverage {i in 1..T,j in 1..T}: 	x[i,j] = 1; 
subject to revenue {i in 1..T,j in 1..T}: 	x[i,j]*R[i,j] <= R[i,j]; 
subject to equality {i in 1..T,j in 1..T}: z[i,j] >= x[i,j];

subject to times1 {i in 1..1,j in 1..1}: z[i,j] = y[i,j]+y[i,j+1]+y[i+1,j];
subject to times2 {i in 1..1,j in T..T}: z[i,j] = y[i,j-1]+y[i,j]+y[i+1,j];
subject to times3 {i in T..T,j in 1..1}: z[i,j] = y[i-1,j]+y[i,j]+y[i,j+1];
subject to times4 {i in T..T,j in T..T}: z[i,j] = y[i-1,j]+y[i,j-1]+y[i,j];

subject to times5 {i in 1..1,j in 2..(T-1)}: z[i,j] = y[i,j-1]+y[i,j]+y[i,j+1]+y[i+1,j];
subject to times6 {i in T..T,j in 2..(T-1)}: z[i,j] = y[i,j-1]+y[i,j]+y[i,j+1]+y[i-1,j];
subject to times7 {i in 2..(T-1),j in 1..1}: z[i,j] = (y[i-1,j])+(y[i,j]+y[i,j+1])+(y[i+1,j]);
subject to times8 {i in 2..(T-1),j in T..T}: z[i,j] = (y[i-1,j])+(y[i,j-1]+y[i,j])+(y[i+1,j]);

subject to times9 {i in 2..(T-1),j in 2..(T-1)}: z[i,j] = (y[i-1,j])+(y[i,j-1]+y[i,j]+y[i,j+1])+(y[i+1,j]);
