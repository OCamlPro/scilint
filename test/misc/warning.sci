// W001
function z = f()
  z = x + 1 // W001: "x" not initialized
endfunction

// W002
function z = f(y) // W002: "y" is not used
  z = 0
endfunction

// W003
function z = f(a,b,a) // W003: argument "a" appears twice
  z = a + b
endfunction

// W004
function [a,b,a] = f() // W004: return variable "a" appears twice
  a = 0
  b = 1
endfunction

// W005/W007
function [a] = f(a) // W005: return variable "a" is also an argument
  a = a + 1 // W007 : return variable "a" used as a local variable
endfunction

// W006/W001
function [a,b] = f() // W006: return variable "a" is never set
  if( a > 0 ) then // W001: "a" not initialized
    b = 1;
  else
    b = 0;
  end;
endfunction

//W007
function [a] = f()
  a = 0;
  for i = 1:100,
    a = a +1; // W007: return variable "a" used as a local variable
  end;
endfunction


