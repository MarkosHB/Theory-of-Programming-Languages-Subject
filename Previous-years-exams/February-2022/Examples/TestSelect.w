program TestSelect(x,y,z);

x:= 1;
y:= 2;
z:= 3;

select
   when x = 1     do x:= x * 2
   when y <= 10   do y:= 0-1
   when z = x + y do z:= 4
end;

select
   when y = 9 do skip
   when z = 0 do skip
   when x = 2 do begin
                     w:= 45;
                     q:= w + 5;
                     select
                        when z = 4 do skip
                        when y = 2 do z:= 3 * q
                        when true  do skip
                     end;
                     y:= 16
                 end
end

// Final State: ["x->2","y->16","z->150"]
