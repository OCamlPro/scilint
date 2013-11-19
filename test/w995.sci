function f()
  for i = 1:2
    for j = 1:2
      i = 0 // W008 for i
      j = 0 // W008 for j
    end
  end
endfunction

function g()
  for i = 1:2
    for j = 1:2
      disp(j)
    end
    i = 0 // W008 for i
    j = 0 // No W008 here
  end
endfunction

function h()
  for i = 1:2
    for j = 1:2
      disp(j)
    end
    disp(i)
  end
    i = 0 // No W008 here
    j = 0 // No W008 here
endfunction