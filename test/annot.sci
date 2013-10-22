function z = f()
  // W-1
  z = x + 1 
endfunction

// W-[2-4]
function [w, w] = g(x, y, x)
  w = x + 1
endfunction

// W-{2;4;6}
function [res, res] = h(a, b)
  disp(b + 1)
endfunction

function v = h(x)
  // W-{1;[5-7]}
  v = v + x + a
endfunction



