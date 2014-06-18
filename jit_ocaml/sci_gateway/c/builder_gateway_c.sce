function builder_gw_c()

  includes_src_c = ilib_include_flag(get_absolute_file_path("builder_gateway_c.sce") + "../../src/c");

  src_lib_path = "../../src/c/libcjit"

  tbx_build_gateway("jit_c", ..
                    ["c_jit","sci_cjit"; ..
		    "c_jit_read", "sci_cjit_read"; ..
		    "c_jit_write", "sci_cjit_write"], ..
                    ["sci_cjit.c"; ..
		    "sci_cjit_read.c"; ..
		    "sci_cjit_write.c"], ..
                    get_absolute_file_path("builder_gateway_c.sce"), ..
                    [src_lib_path], ..
                    "", ..
                    "", ..
                   includes_src_c);
                   
endfunction

builder_gw_c();
clear builder_gw_c; // remove builder_gw_c on stack
