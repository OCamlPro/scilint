all :
	ocamlopt -ccopt -fPIC -c cjit.c           
	gcc -fPIC -shared -o libcjit.so start_ocaml.c ../../lib/libasmrun_shared.so ../ocaml/ocaml.o cjit.o 