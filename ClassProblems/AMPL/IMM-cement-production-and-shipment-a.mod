set nodes;
set links within (nodes cross nodes);
param s {nodes} >= 0;   # amounts available at nodes (supply)
param d {nodes} >= 0;   # amounts required at nodes (demand)
check: sum {i in nodes} s[i] = sum {j in nodes} d[j];
param c {links} >= 0;      # 
param Q {links} >= 0;  # capacity of the link
var x {(i,j) in links} >= 0, <= Q[i,j]; 
minimize totalcost:   sum{(i,j) in links} c[i,j] * x[i,j];
subject to balance {k in nodes}:   s[k] + sum {(i,k) in links} x[i,k] = d[k] + sum {(k,j) in links} x[k,j];
