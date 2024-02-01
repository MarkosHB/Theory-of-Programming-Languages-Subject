program TestFor(i,s,j,r);

// loop that modifies the control variable
s:= 0;
for (i:= 1; i <= 10; i:= i + 2) do begin
   i:= i + 1; // iteration is affected by this assignment
   s:= s + i
end;

// empty for loop
for (j:= 15; j <= 10; j:= j+1) do
   r:= 100

// Final State: ["i->13","s->26","j->15","r->0"]
