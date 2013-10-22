function w = h(a)
  w = 2 + 1
endfunction

function z = f(y)
  b = 3;
  function a = j(x)
    a = y + x
  endfunction
  z = h(b + j(t))
endfunction

function g(x)
  x(1)
endfunction

function w = k(b)
  w = return(b)
endfunction

t = 2
s = f(1)
g(disp)