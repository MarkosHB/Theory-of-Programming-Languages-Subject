program FactorialDoWhile(x, fac);

// A simple While program to compute the factorial of 'x'
// using a 'do while' loop

x := 5;
fac := 1;

do
   fac := fac * x;
   x := x - 1
while !(x = 0)
