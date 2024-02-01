program TestBlockWithoutVars(w,x,y,z);

x:= 1;
y:= x + 4;

begin
   y:= 50;
   w:= 10;
   x:= 20;

   y:= y + 10;
   begin
      x:= x + y + w;

      x:= x + 1;
      z:= 2 * x
   end;
   x:= x * 10
end;

x:= x + 1

// Final State: ["w->10","x->911","y->60","z->182"]
