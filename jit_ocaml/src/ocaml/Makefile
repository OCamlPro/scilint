SCILINT_PATH= ~/dev_scilint_merge/scilint
OLLVM_PATH= ~/ollvm/src/ollvm

OLLVM_INCL= -I $(OLLVM_PATH) ollvm.cmxa

LLVM_INCL= \
  $(shell ocamlfind query -i-format llvm)

SCILINT_INCL= \
  $(shell ocamlfind query -i-format pprint uutf) \
  -I $(SCILINT_PATH)/src/common \
  -I $(SCILINT_PATH)/src/input \
  -I $(SCILINT_PATH)/src/ast \
  -I $(SCILINT_PATH)/src/parser/scilab_five \
  -I $(SCILINT_PATH)/src/parser/scilab_six \
  -I $(SCILINT_PATH)/src/scilint \
  -I $(SCILINT_PATH)/src/config \
  -I $(SCILINT_PATH)/src/scintax \
  -I $(SCILINT_PATH)/src/docgen \
  -I $(SCILINT_PATH) scilint.cmxa

context :
	ocamlopt -output-obj -o contextml.o -ccopt -fPIC context.ml

all :
	ocamlfind ocamlopt -output-obj $(SCILINT_INCL) $(OLLVM_INCL) $(LLVM_INCL) -o ocaml.o  -package 'uutf,pprint,llvm' -linkpkg -ccopt -fPIC context.cmx mod.ml

clean :
	rm -f \
	  *.old \
	  *~ */*~ */*/*~ */*/*/*~ \
	  *.cm* */*.cm* */*/*.cm* */*/*/*.cm* \
	  *.o */*.o */*/*.o */*/*/*.o