function jit(file_name)
  file_name = c_test(file_name);
  exec(file_name);
  disp(f(3));
endfunction
