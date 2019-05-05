set INTER; # intersections
param entr symbolic in INTER; # entrance to road network
param exit symbolic in INTER, <> entr; # exit from road network
set STATION within (INTER diff {exit}) cross (INTER diff {entr});
param cap { STATION } >= 0; # capacities
param percent { STATION } >= 0; # percentage
var Traff {(i,j) in STATION } >= 0, <= cap[i,j]; # traffic loads
maximize Entering_Traff: sum {(i,exit) in STATION } Traff[i,exit]*percent[i,exit];
subject to Balance {k in INTER diff {entr,exit}}:
sum {(i,k) in STATION } Traff[i,k]*percent[i,k] >= sum {(k,j) in STATION} Traff[k,j];
