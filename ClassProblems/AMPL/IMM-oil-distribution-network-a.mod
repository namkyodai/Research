set INTER; # intersections
param entr symbolic in INTER; # entrance to road network
param exit symbolic in INTER, <> entr; # exit from road network
set STATION within (INTER diff {exit}) cross (INTER diff {entr});
param cap { STATION } >= 0; # capacities
var Traff {(i,j) in STATION } >= 0, <= cap[i,j]; # traffic loads
maximize Entering_Traff: sum {(entr,j) in STATION } Traff[entr,j];
subject to Balance {k in INTER diff {entr,exit}}:
sum {(i,k) in STATION } Traff[i,k] = sum {(k,j) in STATION} Traff[k,j];
