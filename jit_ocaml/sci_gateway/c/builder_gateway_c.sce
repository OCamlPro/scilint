function builder_gw_c()

  includes_src_c = ilib_include_flag(get_absolute_file_path("builder_gateway_c.sce") + "../../src/c");

  // PutLhsVar managed by user in sci_sum and in sci_sub
  // if you do not this variable, PutLhsVar is added
  // in gateway generated (default mode in scilab 4.x and 5.x)
  WITHOUT_AUTO_PUTLHSVAR = %t;

  tbx_build_gateway("jit_c", ..
                    ["c_test","sci_ctest";"c_jit_read","sci_cjit_read"], ..
                    ["sci_ctest.c";"sci_cjit_read.c"], ..
                    get_absolute_file_path("builder_gateway_c.sce"), ..
                    ["../../src/c/libctest"], ..
                    "", ..
                   includes_src_c);
                   
endfunction

builder_gw_c();
clear builder_gw_c; // remove builder_gw_c on stack
