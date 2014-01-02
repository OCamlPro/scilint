all: scilint.asm scintax.asm

OCAMLOPT=ocamlfind ocamlopt
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLDEP=ocamlfind ocamldep

OCAML_COMMON_MLS = \
  src/common/scilintWarning.ml \
  src/common/scilabLocations.ml \
  src/common/scilintOptions.ml

OCAML_SCILAB_SIX_PARSER_MLS = \
  src/scilab_six_parser/scilabLexer.ml \
  src/scilab_six_parser/scilabPreParser.ml \
  src/scilab_six_parser/scilabParserUtils.ml \
  src/scilab_six_parser/scilabParser.ml

src/scilab_six_parser/scilabLexer.cmx: \
  src/scilab_six_parser/scilabParser.cmi

OCAML_SCILAB_SIX_AST_MLS = \
  src/scilab_six_ast/scilabSymbol.ml \
  src/scilab_six_ast/scilabAst.ml \
  src/scilab_six_ast/scilabAstPrinter.ml \
  src/scilab_six_ast/scilabPrettyPrinter.ml

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

OCAML_SCILINT_MLS = \
  src/scilint/scilabDeffRefactoring.ml \
  src/scilint/scilintFirehosegen.ml \
  src/scilint/scilabUtils.ml \
  src/scilint/config/scilintTree.ml \
  src/scilint/config/scilintParser.ml \
  src/scilint/config/scilintLexer.ml \
  src/scilint/config/scilintConfig.ml \
  src/scilint/scilintProject.ml \
  src/scilint/scilabFunctionAnalyze.ml \
  src/scilint/scilabAstStats.ml \
  src/scilint/scilintMain.ml

src/scilint/config/scilintLexer.cmx: \
  src/scilint/config/scilintParser.cmi

OCAML_SCILINT_MLIS = \
  src/scilint/scilintProject.mli \
  src/scilint/scilabUtils.mli

########## SCILINT

SCILINT_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_SCILAB_FIVE_AST_MLS) \
	$(OCAML_SCILAB_FIVE_PARSER_MLS) \
	$(OCAML_SCILAB_SIX_AST_MLS) \
	$(OCAML_SCILAB_SIX_PARSER_MLS) \
	$(OCAML_SCILINT_MLS)

SCILINT_MLIS =

SCILINT_CMIS = $(SCILINT_MLS:.ml=.cmi) $(SCILINT_MLIS:.mli=.cmi)
SCILINT_CMXS = $(SCILINT_MLS:.ml=.cmx)
SCILINT_OBJS = $(SCILINT_MLS:.ml=.o)

########## SCINTAX

SCINTAX_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_SCILAB_FIVE_AST_MLS) \
	$(OCAML_SCILAB_FIVE_PARSER_MLS) \
	$(OCAML_SCINTAX_MLS)

SCINTAX_MLIS =

SCINTAX_CMIS = $(SCINTAX_MLS:.ml=.cmi) $(SCINTAX_MLIS:.mli=.cmi)
SCINTAX_CMXS = $(SCINTAX_MLS:.ml=.cmx)
SCINTAX_OBJS = $(SCINTAX_MLS:.ml=.o)

########## COMMON FLAGS

OCAML_INCL= \
  $(shell ocamlfind query -i-format pprint uutf) \
  -I src/common \
  -I src/scilab_five_ast -I src/scilab_five_parser \
  -I src/scilab_six_ast -I src/scilab_six_parser \
  -I src/scilint -I src/scilint/config \
  -I src/scintax

OPTFLAGS = -g -fPIC $(OCAML_INCL)

scilint.asm : $(SCILINT_CMXS)
	$(OCAMLOPT) -package 'unix,uutf,pprint' -linkpkg \
	  -o scilint.asm $(SCILINT_CMXS)

scintax.asm : $(SCINTAX_CMXS)
	$(OCAMLOPT) $(OPTFLAGS) -package 'unix,pprint'  -linkpkg \
          -o scintax.asm $(SCINTAX_CMXS)

depend:
	$(OCAMLDEP) -native $(OCAML_INCL) \
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
	rm -f \
	  *.old \
	  *~ */*~ */*/*~ */*/*/*~ \
	  *.cm* */*.cm* */*/*.cm* */*/*/*.cm* \
	  *.o */*.o */*/*.o */*/*/*.o \
	  scilint scilint.asm \
	  scintax scintax.asm \
	  src/scilab_six_parser/scilabLexer.ml \
	  src/scilab_six_parser/scilabParser.ml \
	  src/scilint/config/scilintLexer.ml \
	  src/scilint/config/scilintParser.ml
