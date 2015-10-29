function pr(x), disp(x),endfunction
function f()
   function pr(x,y) // W010: redefinition of library function 'pr'
     disp(x,y)
   endfunction
endfunction
