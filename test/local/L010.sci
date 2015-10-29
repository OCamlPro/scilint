function f()
  function pr(x), disp(x),endfunction
  function pr(x,y) // W010: redefinition of local function 'pr'
    disp(x,y)
  endfunction
endfunction

