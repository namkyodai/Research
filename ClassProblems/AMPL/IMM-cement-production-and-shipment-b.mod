set nodes;  # intersections
param entr symbolic in nodes;           # entrance to road network
param exit symbolic in nodes, <> entr;  # exit from road network
set links within (nodes diff {exit}) cross (nodes diff {entr});
param l {links} >= 0;         # length
var Use {(i,j) in links} >= 0;   # 1 iff (i,j) in shortest path
minimize Total_l: sum {(i,j) in links} l[i,j] * Use[i,j];
subject to Start:  sum {(entr,j) in links} Use[entr,j] = 1;
subject to Balance {k in nodes diff {entr,exit}}:
sum {(i,k) in links} Use[i,k] = sum {(k,j) in links} Use[k,j];
