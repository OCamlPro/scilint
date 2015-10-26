CAMLSTDLIB=$(shell ocamlc -where)
INCLUDES=-I ../../../api_scilab/includes/ \
	-I ../../../operations/includes/ \
	-I ../../../core/includes/
stubs :
	gcc -g $(INCLUDES) -fPIC -shared -o libccontext.so context_stubs.c
	mv libccontext.so ../../lib
all :
	gcc -g -fPIC -shared -o libcjit.so -I $(CAMLSTDLIB) cjit.c ../../lib/libasmrun_shared.so ../ocaml/ocaml.o ../../lib/libccontext.so

clean:
	rm -f *.o *.so