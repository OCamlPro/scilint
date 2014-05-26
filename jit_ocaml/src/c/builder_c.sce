// This file is released under the 3-clause BSD license. See COPYING-BSD.

// This macro compiles the files

function builder_c()

  src_c_path = get_absolute_file_path("builder_c.sce");

  
  CFLAGS = ilib_include_flag(src_c_path);
  
  caml_lib_path = "../../lib/libcamlrun_shared"
  //caml_lib_path = ""
  
  tbx_build_src(["ctest"],       ..
                ["ctest.c"],     ..
                "c",             ..
                src_c_path,      ..
                [caml_lib_path], ..
                "",              ..
                "");
  
  // CFLAGS
                
endfunction

builder_c();
clear builder_c; // remove builder_c on stack
