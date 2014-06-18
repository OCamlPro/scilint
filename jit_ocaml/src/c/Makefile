CAMLSTDLIB=$(shell ocamlc -where)

all :
#	ocamlopt -ccopt -fPIC -c cjit.c           
	gcc -fPIC -shared -o libcjit.so -I $(CAMLSTDLIB) cjit.c ../../lib/libasmrun_shared.so ../ocaml/ocaml.o 