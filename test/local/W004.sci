function [a,b,a] = f() // W004: return variable "a" appears twice
a = cos(0)
b = cos(pi)
c = cos(pi*2)
endfunction
