param T>0; 	#planning period in the day
#param require{t in 1..T}; # people
param limit_min{t in 1..T}; # people
var x{1..T}>=0; #requires no of people at period i

#objective function
minimize no_people:
	sum{t in 1..T} x[t]; #minimum number of people
#constraints of the problem
subject to require1 {t in 1..T}: x[1]+x[6] >= limit_min[1]; 
subject to require2 {t in 1..T}: x[1]+x[2] >= limit_min[2]; 
subject to require3 {t in 1..T}: x[2]+x[3] >= limit_min[3]; 
subject to require4 {t in 1..T}: x[3]+x[4] >= limit_min[4]; 
subject to require5 {t in 1..T}: x[4]+x[5] >= limit_min[5]; 
subject to require6 {t in 1..T}: x[5]+x[6] >= limit_min[6]; 