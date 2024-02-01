program Divide(x,y,q,r,error);

// a simple program to compute quotient and remainder

x:= 39;
y:= 5;

select
   when y = 0 do error:= 1
   when true do begin
                   r:= x;
                   for (q:= 0; y <= r; q:= q + 1) do
                      r:= r - y
                end
end

// Final State: ["x->39","y->5","q->7","r->4","error->0"]
