// This file is released under the 3-clause BSD license. See COPYING-BSD.

function call_make(path, rule)
  if ( ilib_verbose() <> 0 ) then
     mprintf(gettext("   Running the Makefile\n"));
  end
  cmd = "make -C " + path + " " + rule;
  [msg, ierr, stderr] = unix_g(cmd);
  
  if ierr <> 0 then
     errMsg = sprintf(gettext("%s: An error occurred during the compilation:\n"), "ilib_compile");
     errMsg = [errMsg ; stderr];
     errMsg = [errMsg ; sprintf(gettext("%s: The command was:\n"), "ilib_compile")];
     errMsg = [errMsg ; cmd];
     error(errMsg);
     return ;
  end

  if stderr <> "" then
     if ( ilib_verbose() <> 0 ) then
     	mprintf(gettext("%s: Warning: No error code returned by the compilation but the error output is not empty:\n"),"ilib_compile");
        mprintf(stderr);
     end
     return ;
   end

endfunction

function builder_src()
  src_path = get_absolute_file_path("builder_src.sce");
  // OCaml context's src
  call_make(src_path + "ocaml", "context");  
  // C Context wrapper src
  call_make(src_path + "c", "stubs");  
  // Ocaml src
  call_make(src_path + "ocaml", "all");  
  // C wrapper
  call_make(src_path + "c", "all");  
endfunction

builder_src();
clear builder_src; // remove builder_src on stack
