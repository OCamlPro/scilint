if (isdef('genlib') == %f) then
  exec(SCI+'/modules/functions/scripts/buildmacros/loadgenlib.sce');
end

genlib('jit_ocaml','SCI/modules/jit_ocaml2/macros');
