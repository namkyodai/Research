#-----------------------------------------------------------------------
#Developed and coded by Dr. Nam Lethanh. It is used for the course "Infrastructure Management II (Fall semester)".  
# AMPL language
# May, 2011
#-----------------------------------------------------------------------
#
#http://www.neos-server.org/neos/solvers/lp:Gurobi/AMPL.html

set nodes;  # intersections
param entr symbolic in nodes;           # entrance to road network
param exit symbolic in nodes, <> entr;  # exit from road network
set links within (nodes diff {exit}) cross (nodes diff {entr});
param c {links} >= 0;         # cost (CHF)
var delta {(i,j) in links} >= 0;   # 1 iff (i,j) in shortest path

minimize Totalcost: sum {(i,j) in links} c[i,j] * delta[i,j];

subject to Start:  sum {(entr,j) in links} delta[entr,j] = 1;

subject to Balance {k in nodes diff {entr,exit}}:
   sum {(i,k) in links} delta[i,k] = sum {(k,j) in links} delta[k,j];

