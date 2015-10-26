function jit_load(filename)
//  full_path = pwd () + "/" + filename
  c_jit_load(filename)
  exec(filename + ".jit")
endfunction