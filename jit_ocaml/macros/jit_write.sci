function jit_write(var_name, val)
  c_jit_write(var_name, val);
  execstr(var_name + " = resume(" + var_name + ");");
endfunction