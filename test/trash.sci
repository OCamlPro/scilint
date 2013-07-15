function w = h(x)
  w = x + 1
endfunction

function z = f(x)
  b = 3
  function a = j(x)
    a = x + b
  endfunction
  z = h(x + j(t))
endfunction

function g(x)
  x(1)
endfunction

function w = k(x)
  w = return(x)
endfunction

t = 2
s = f(1)
g(disp)