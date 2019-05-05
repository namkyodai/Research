set ROOM; #type of rooms
set OUTCOME; #type of outcome
set DEVIATION; #numbers of deviations

#parameters
param w1 {ROOM, DEVIATION}; # weighting factors assigned to room
param w2 {OUTCOME, DEVIATION}; # weighting factors assigned to outcome
param N {ROOM}; #numbers of room
param M {OUTCOME}; #values of outcome
param c {ROOM,OUTCOME}; #values of outcome

# Decision variables 
var d1{ROOM, DEVIATION} integer >=0;  #number of conference
var d2{OUTCOME, DEVIATION} >=0;  #number of outcome
var x{ROOM} integer >=0;  #integer value

# Objective function 
minimize obj: sum {i in ROOM} (1/N[i]*(sum{k in DEVIATION} (w1[i,k]*d1[i,k]))) + sum {j in OUTCOME}(1/M[j]*(sum{k in DEVIATION} (w2[j,k]*d2[j,k]))); 

subject to room {i in ROOM}: 	x[i] +sum {k in DEVIATION} d1[i,k] = N[i];
subject to outcome {j in OUTCOME}: 	(sum {i in ROOM} (x[i]*c[i,j])) + sum {k in DEVIATION} d2[j,k] = M[j]; 


