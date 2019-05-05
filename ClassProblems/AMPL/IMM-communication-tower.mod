#define SET
set city;   # 
param X{city}; # value of the coordinate X 
param Y{city}; # value of the coordinate X 
param D{city}; #maximum distance to the city
# Decision variables 
var xi >=0; # value of coordinate X
var yi >=0; # value of coordinate Y
# Objective function 
minimize distance: sum{i in city} sqrt((xi-X[i])^2+(yi-Y[i])^2); 
subject to maximumdistance{i in city}: sqrt((xi-X[i])^2+(yi-Y[i])^2) <= D[i]