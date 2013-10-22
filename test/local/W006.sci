function [a,b] = f() // W006: return variable "a" is never set
  if( a > 0 ) then
    b = 1;
  else
    b = 0;
  end;
endfunction

