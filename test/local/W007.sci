function [a] = f()
  a = 0;
  for i = 1:100,
    a = a +1; // W007: return variable "a" used as a local variable
  end;
endfunction
