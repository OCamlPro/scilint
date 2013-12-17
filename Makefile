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
	src/parser/scilabParserUtils.ml \
	src/parser/scilabParser.ml \
	src/parser/uutf.ml \
	src/parser/scilabLexer.ml

OCAML_PARSER_MLIS= \
	src/parser/uutf.mli \
	src/parser/scilabParser.mli \
	src/scilint/config/scilintParser.mli

OCAML_SCILINT_MLS = \
	src/scilint/config/scilintTree.ml \
	src/scilint/config/scilintParser.ml \
	src/scilint/config/scilintLexer.ml \
	src/scilint/config/scilintConfig.ml \
	src/scilint/scilabUtils.ml \
	src/scilint/scilintFirehosegen.ml \
	src/scilint/scilintWarning.ml \
	src/scilint/scilintProject.ml \
	src/scilint/scilabFunctionAnalyze.ml \
	src/scilint/scilabDeffRefactoring.ml \
	src/scilint/scilabAstStats.ml \
	src/scilint/scilintMain.ml

OCAML_SCILAB_FIVE_PARSER_MLS = \
	src/scilab_five_parser/scilabFiveParserAst.ml \
	src/scilab_five_parser/scilabFiveParser.ml \
	src/scilab_five_parser/scilabFiveParserAstSerializer.ml

OCAML_SCILAB_FIVE_AST_MLS = \
	src/scilab_five_ast/scilabFiveAst.ml \
	src/scilab_five_ast/scilabFiveAstUtils.ml \
	src/scilab_five_ast/scilabFiveAstSexpPrinter.ml \
	src/scilab_five_ast/scilabFiveAstPrettyPrinter.ml

OCAML_SCINTAX_MLS = \
	src/scintax/scintaxMain.ml

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

########## SCINTAX

SCINTAX_MLS = \
	$(OCAML_SCILAB_FIVE_AST_MLS) \
	$(OCAML_SCILAB_FIVE_PARSER_MLS) \
	$(OCAML_SCINTAX_MLS)

SCINTAX_MLIS =

SCINTAX_CMIS = $(SCINTAX_MLS:.ml=.cmi) $(SCINTAX_MLIS:.mli=.cmi)
SCINTAX_CMXS = $(SCINTAX_MLS:.ml=.cmx)
SCINTAX_OBJS = $(SCINTAX_MLS:.ml=.o)

########## COMMON FLAGS

OCAML_INCL= \
  -I $(shell ocamlfind query pprint) \
  -I src/common -I src/parser \
  -I src/scilint -I src/scilint/config \
  -I src/scilab_five_ast -I src/scilab_five_parser

OPTFLAGS = -g -fPIC $(OCAML_INCL)

all: scilint.asm scintax.asm

scilint.asm : $(SCILINT_CMXS)
	$(OCAMLOPT) unix.cmxa \
	  -o scilint.asm $(SCILINT_CMXS)

scintax.asm : $(SCINTAX_CMXS)
	$(OCAMLOPT) $(OPTFLAGS) unix.cmxa PPrintLib.cmxa \
          -o scintax.asm $(SCINTAX_CMXS)

depend: $(OCAML_PARSER_MLS)
	$(OCAMLDEP) -native $(OCAML_INCL) \
		$(OCAML_JIT_MLS) $(OCAML_JIT_MLIS) \
		$(SCILINT_MLS) $(SCILINT_MLIS) \
		$(SCINTAX_MLS) $(SCINTAX_MLIS) > .depend_ocaml
include .depend_ocaml

ChangeLog.txt: _obuild/scilintDocgen/scilintDocgen.byte
	_obuild/scilintDocgen/scilintDocgen.byte -changelog-txt ChangeLog.txt

########## COMMON
.SUFFIXES: .ml .mli .mll .mly .cmi .cmx

.ml.cmx:
	$(OCAMLOPT) $(OPTFLAGS) -c $<

.mll.cmx:
	$(OCAMLLEX) $<
	$(OCAMLOPT) $(OPTFLAGS) -c $*.ml

.mli.cmi:
	$(OCAMLOPT) $(OPTFLAGS) -c $<

.mll.ml: 
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.cmx:
	$(OCAMLYACC) $<
	$(OCAMLOPT) $(OPTFLAGS) -c $*.mli
	$(OCAMLOPT) $(OPTFLAGS) -c $*.ml

.mly.cmi:
	$(OCAMLYACC) -v $<
	$(OCAMLOPT) $(OPTFLAGS) -c $*.mli

clean :
	rm -f $(OCAML_JIT_CMIS)  \
	$(OCAML_JIT_CMXS)  \
	$(OCAML_JIT_OBJS)  \
	$(SCILINT_CMIS)  \
	$(SCILINT_CMXS)  \
	$(SCILINT_OBJS)  \
	$(SCINTAX_CMIS)  \
	$(SCINTAX_CMXS)  \
	$(SCINTAX_OBJS)  \
	scilint \
	scilint.asm \
	scintax \
	scintax.asm \
	src/lex/*.cm* \
	src/lex/*.o \
	src/yacc/*.cm* \
	src/yacc/*.o \
	src/lex/scilabLexer.ml \
	src/yacc/scilabParser.ml \
	src/yacc/scilabParser.mli
