set SECTOR ordered; 
set SITE   ordered; 
set OBJ;
param s {SECTOR} >= 0; #snow amount m3
param q {SITE} >= 0; #capacity m3
param r {SITE}; #max snow removal
param l {SECTOR, SITE} >= 0; #distance in km
param N1; #optimal value of objective function 1
param N2; #optimal value of objective function 2
param w1; #weight for objective function 1
param w2; #weight for objective function 2

# Decision variables 
var x {SECTOR, SITE} binary; 
var d1; # deviation for obj 1
var d2; #deviation for obj 2

# Objective function 
minimize deviation:   w1*d1+w2*d2; 
subject to oneSite {i in SECTOR}:   sum {j in SITE} x[i,j] = 1; 
subject to sitecapacity {j in SITE}:  sum {i in SECTOR} s[i]*x[i,j] <= q[j];
subject to maxremoval {j in SITE}:  r[j]*(sum{i in SECTOR} s[i] * x[i,j]) <= r[j]*q[j];
subject to deviation1 : d1=(((sum {i in SECTOR, j in SITE} 1000 * 0.1 * s[i] * l[i,j] * x[i,j])-N1)/N1);
subject to deviation2 : d2=((N2-(sum {j in SITE} r[j]*sum{i in SECTOR} s[i]*x[i,j]))/N2);

	