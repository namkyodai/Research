set plant;   # 
set customer;   # 
set material;   # 

param r {plant,material} > 0;     # tons per unit time (hour, day)
param Q {plant} >= 0;        # available time of plant i
param D {customer,material} >= 0;  # tons required for customer 

param c {plant,material} >= 0;        # manufacturing cost/ton
param v {plant,customer,material} >= 0;  # transportation cost/ton

var x {plant,material} >= 0;       # tons material at plant i
var y {plant,customer,material} >= 0; # tons transported

minimize Total_Cost:
   sum {i in plant, p in material} c[i,p] * x[i,p] +
   sum {i in plant, j in customer, p in material}
			v[i,j,p] * y[i,j,p];

subject to Time {i in plant}:
   sum {p in material} (1/r[i,p]) * x[i,p] <= Q[i];

subject to Supply {i in plant, p in material}:
   sum {j in customer} y[i,j,p] = x[i,p];

subject to Demand {j in customer, p in material}:
   sum {i in plant} y[i,j,p] = D[j,p];
