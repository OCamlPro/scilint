How to run JitOcaml :
- cp -r jit_ocaml SCI/modules
- .SCI/bin/scilab-cli
- exec modules/jit_ocaml/builder.sce
- exec modules/jit_ocaml/loader.sce

Use C api then call OCaml with freevars infos :
jit(expr, freevars) :
  * expr : expression
  * freevars : vector of variable names that are free in expr

Use C api FROM OCaml :
jit_test()