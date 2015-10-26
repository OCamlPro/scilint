How to run JitOcaml :
- cp -r jit_ocaml SCI/modules
- Generate libasmrun_shared.so and copy it in lib/
- edit src/ocaml/Makefile to add path to Scilint and Ollvm
- .SCI/bin/scilab-cli
- exec modules/jit_ocaml/builder.sce
- exec modules/jit_ocaml/loader.sce

jit_load(filename) :
  * filename : script scilab
