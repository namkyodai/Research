set INTER; # intersections
param entr symbolic in INTER; # entrance to road network
param exit symbolic in INTER, <> entr; # exit from road network
set ROADS within (INTER diff {exit}) cross (INTER diff {entr});
param cap {ROADS} >= 0; # capacities
var Traff {(i,j) in ROADS} >= 0, <= cap[i,j]; # traffic loads
maximize Entering_Traff: sum {(entr,j) in ROADS} Traff[entr,j];
subject to Balance {k in INTER diff {entr,exit}}:
sum {(i,k) in ROADS} Traff[i,k] = sum {(k,j) in ROADS} Traff[k,j];