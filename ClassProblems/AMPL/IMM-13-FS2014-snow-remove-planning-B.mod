set SECTOR ordered; 
set SITE   ordered; 
param s {SECTOR} >= 0; #snow amount m3
param q {SITE} >= 0; #capacity m3
param r {SITE}; #max snow removal
param l {SECTOR, SITE} >= 0; #distance in km

# Decision variables 
var x {SECTOR, SITE} binary; 
# Objective function 
maximize removalcontamination:   
    sum {j in SITE} r[j]*sum{i in SECTOR} s[i]*x[i,j]; 
subject to oneSite {i in SECTOR}:   
    sum {j in SITE} x[i,j] = 1; 
subject to sitecapacity {j in SITE}: 
    sum {i in SECTOR} s[i]*x[i,j] <= q[j];
subject to maxremoval {j in SITE}: 
    r[j]*(sum{i in SECTOR} s[i] * x[i,j]) <= r[j]*q[j];
	
	