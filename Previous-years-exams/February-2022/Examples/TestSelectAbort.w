program TestSelectAbort(x,y,z);

x:= 10;
y:= 20;
z:= 30;

select
   when x = 4 do x:= x * 2
   when y = 5 do y:= 0-1
   when z = 6 do z:= z + 4
end

// Natural Semantics:
//
// Program TestSelectAbort finalized.
// Final State: ["x->*** Exception: non-exhaustive select statement

// Structural Semantics:
//
// Stuck state:
// Select (EndCase (Eq (V "z") (N 6)) (Ass "z" (Add (V "z") (N 4))))
//  s(x)= 10
//  s(y)= 20
//  s(z)= 30
