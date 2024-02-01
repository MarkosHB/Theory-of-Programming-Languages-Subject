program TestBlock(w,x,y,z);

x:= 1;
y:= x + 4;

begin
   var y:= 50;
   var w:= 10;
   var x:= 20;

   y:= y + 10;
   begin
      var x:= x + y + w;

      x:= x + 1;
      z:= 2 * x
   end;
   x:= x * 10
end;

x:= x + 1

// Final State: ["w->0","x->2","y->5","z->182"]
