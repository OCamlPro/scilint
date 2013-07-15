OCAMLOPT=ocamlopt
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLDEP=ocamldep

OCAML_COMMON_MLS = \
	src/common/scilabSymbol.ml \
	src/common/scilabAst.ml \
	src/common/scilabAstPrinter.ml \
	src/common/scilabPrettyPrinter.ml

OCAML_PARSER_MLS = \
	src/parser/scilabPreParser.ml \
	src/parser/scilabParser.ml \
	src/parser/uutf.ml \
	src/parser/scilabLexer.ml

OCAML_PARSER_MLIS= \
	src/parser/uutf.mli \
	src/parser/scilabParser.mli

OCAML_SCILINT_MLS = \
	src/scilint/scilabFunctionAnalyze.ml \
	src/scilint/scilabUtils.ml \
	src/scilint/scilabDeffRefactoring.ml \
	src/scilint/scilabAstStats.ml \
	src/scilint/scilintMain.ml

OCAML_SCILINT_MLIS = 


######### OCAML_JIT

OCAML_JIT_MLS = \
	$(OCAML_COMMON_MLS)

OCAML_JIT_MLIS = \
	$(OCAML_COMMON_MLIS)

OCAML_JIT_CMIS = $(OCAML_JIT_MLS:.ml=.cmi) $(OCAML_JIT_MLIS:.mli=.cmi)
OCAML_JIT_CMXS = $(OCAML_JIT_MLS:.ml=.cmx)
OCAML_JIT_OBJS = $(OCAML_JIT_MLS:.ml=.o)

########## SCILINT

SCILINT_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_PARSER_MLS) \
	$(OCAML_SCILINT_MLS)

SCILINT_MLIS = \
	$(OCAML_COMMON_MLIS) \
	$(OCAML_PARSER_MLIS) \
	$(OCAML_SCILINT_MLIS)

SCILINT_CMIS = $(SCILINT_MLS:.ml=.cmi) $(SCILINT_MLIS:.mli=.cmi)
SCILINT_CMXS = $(SCILINT_MLS:.ml=.cmx)
SCILINT_OBJS = $(SCILINT_MLS:.ml=.o)

OCAML_INCL= -I src/common -I src/parser -I src/scilint
OPTFLAGS = -g -c -fPIC $(OCAML_INCL)


all : $(SCILINT_CMXS)
	$(OCAMLOPT) bigarray.cmxa str.cmxa \
	  -o scilint $(SCILINT_CMXS)


depend: $(OCAML_PARSER_MLS)
	$(OCAMLDEP) -native $(OCAML_INCL) \
		$(OCAML_JIT_MLS) $(OCAML_JIT_MLIS) \
		$(SCILINT_MLS) $(SCILINT_MLIS) > .depend_ocaml
include .depend_ocaml

########## COMMON
SUFFIXES += .ml .mli .mll .mly .cmi .cmx

%.cmx: %.ml
	$(OCAMLOPT) $(OPTFLAGS) -c $<

%.cmx: %.mll
	$(OCAMLLEX) $<
	$(OCAMLOPT) $(OPTFLAGS) -c $*.ml

%.cmi: %.mli
	$(OCAMLOPT) $(OPTFLAGS) -c $<

%.ml: %.mll
	$(OCAMLLEX) $<

%.ml %.mli: %.mly
	$(OCAMLYACC) -v $<

%.cmx: %.mly
	$(OCAMLYACC) $<
	$(OCAMLOPT) $(OPTFLAGS) -c $*.mli
	$(OCAMLOPT) $(OPTFLAGS) -c $*.ml

%.cmi: %.mly
	$(OCAMLYACC) -v $<
	$(OCAMLOPT) $(OPTFLAGS) -c $*.mli

clean :
	rm -f $(OCAML_JIT_CMIS)  \
	$(OCAML_JIT_CMXS)  \
	$(OCAML_JIT_OBJS)  \
	$(SCILINT_CMIS)  \
	$(SCILINT_CMXS)  \
	$(SCILINT_OBJS)  \
	scilint \
	src/lex/*.cm* \
	src/lex/*.o \
	src/yacc/*.cm* \
	src/yacc/*.o \
	src/lex/scilabLexer.ml \
	src/yacc/scilabParser.ml \
	src/yacc/scilabParser.mli