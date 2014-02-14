all: \
  scilint.asm scintax.asm scilint_doc_gen.asm \
  scilint.byte scintax.byte scilint_doc_gen.byte

OCAMLOPT=ocamlfind ocamlopt
OCAMLC=ocamlfind ocamlc
OCAMLYACC=menhir
OCAMLLEX=ocamllex
OCAMLDEP=ocamlfind ocamldep

OCAML_COMMON_MLS = \
  src/common/scilintWarning.ml \
  src/common/scilintManual.ml \
  src/common/scilabLocations.ml \
  src/common/scilintOptions.ml \
  src/input/scilabTypedPrimitives.ml \
  src/input/scilabTypedPrimitivesLexer.ml \
  src/input/scilabTypedPrimitivesParser.ml \
  src/input/scilabTypedPrimitivesLoader.ml

src/input/scilabTypedPrimitivesParser.cmi: \
  src/input/scilabTypedPrimitives.cmi

src/input/scilabTypedPrimitivesLexer.cmx: \
  src/input/scilabTypedPrimitivesParser.cmi

src/input/scilabTypedPrimitivesLexer.cmo: \
  src/input/scilabTypedPrimitivesParser.cmi

OCAML_SCILAB_SIX_PARSER_MLS = \
  src/parser/scilab_six/scilabSixParserUtils.ml \
  src/parser/scilab_six/scilabSixLexer.ml \
  src/parser/scilab_six/scilabSixGenParser.ml \
  src/parser/scilab_six/scilabSixParser.ml

src/parser/scilab_six/scilabSixLexer.cmx: \
  src/parser/scilab_six/scilabSixGenParser.cmi

src/parser/scilab_six/scilabSixLexer.cmo: \
  src/parser/scilab_six/scilabSixGenParser.cmi

OCAML_SCILAB_FIVE_PARSER_MLS = \
  src/parser/scilab_five/scilabFiveParser.ml

OCAML_AST_MLS = \
  src/ast/scilabAst.ml \
  src/ast/scilabAstUtils.ml \
  src/ast/scilabAstConverter.ml \
  src/ast/scilabAstSexpPrinter.ml \
  src/ast/scilabAstPrettyPrinter.ml \
  src/ast/scilabParserAst.ml \
  src/ast/scilabParserAstSerializer.ml

OCAML_SCINTAX_MLS = \
  src/scintax/scintaxMain.ml

OCAML_SCILINT_DOC_GEN_MLS = \
  src/docgen/scilintDocGenMain.ml

OCAML_SCILINT_MLS = \
#  src/scilint/scilabDeffRefactoring.ml \
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
	$(OCAML_AST_MLS) \
	$(OCAML_SCILAB_FIVE_PARSER_MLS) \
	$(OCAML_SCILAB_SIX_PARSER_MLS) \
	$(OCAML_SCILINT_MLS)

SCILINT_MLIS = \
	$(OCAML_SCILINT_MLIS)

SCILINT_CMIS = $(SCILINT_MLS:.ml=.cmi) $(SCILINT_MLIS:.mli=.cmi)
SCILINT_CMXS = $(SCILINT_MLS:.ml=.cmx)
SCILINT_CMOS = $(SCILINT_MLS:.ml=.cmo)

########## SCINTAX

SCINTAX_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_AST_MLS) \
	$(OCAML_SCILAB_FIVE_PARSER_MLS) \
	$(OCAML_SCINTAX_MLS)

SCINTAX_CMIS = $(SCINTAX_MLS:.ml=.cmi) $(SCINTAX_MLIS:.mli=.cmi)
SCINTAX_CMXS = $(SCINTAX_MLS:.ml=.cmx)
SCINTAX_CMOS = $(SCINTAX_MLS:.ml=.cmo)

########## SCILINT_DOC_GEN

SCILINT_DOC_GEN_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_SCILINT_DOC_GEN_MLS)

SCILINT_DOC_GEN_CMIS = $(SCILINT_DOC_GEN_MLS:.ml=.cmi) $(SCILINT_DOC_GEN_MLIS:.mli=.cmi)
SCILINT_DOC_GEN_CMXS = $(SCILINT_DOC_GEN_MLS:.ml=.cmx)
SCILINT_DOC_GEN_CMOS = $(SCILINT_DOC_GEN_MLS:.ml=.cmo)

########## COMMON FLAGS

OCAML_INCL= \
  $(shell ocamlfind query -i-format pprint uutf) \
  -I src/common -I src/input \
  -I src/ast -I src/parser/scilab_five \
  -I src/parser/scilab_six \
  -I src/scilint -I src/scilint/config \
  -I src/scintax -I src/docgen \

OPTFLAGS = -g -fPIC $(OCAML_INCL)

scilint.asm : $(SCILINT_CMXS)
	$(OCAMLOPT) -package 'unix,uutf,pprint' -linkpkg \
	  -o $@ $(SCILINT_CMXS)

scintax.asm : $(SCINTAX_CMXS)
	$(OCAMLOPT) $(OPTFLAGS) -package 'unix,pprint'  -linkpkg \
          -o $@ $(SCINTAX_CMXS)

scilint_doc_gen.asm : $(SCILINT_DOC_GEN_CMXS)
	$(OCAMLOPT) $(OPTFLAGS) -package 'unix'  -linkpkg \
          -o $@ $(SCILINT_DOC_GEN_CMXS)

scilint.byte : $(SCILINT_CMOS)
	$(OCAMLC) -package 'unix,uutf,pprint' -linkpkg \
	  -o $@ $(SCILINT_CMOS)

scintax.byte : $(SCINTAX_CMOS)
	$(OCAMLC) $(OCAML_INCL) -package 'unix,pprint'  -linkpkg \
          -o $@ $(SCINTAX_CMOS)

scilint_doc_gen.byte : $(SCILINT_DOC_GEN_CMOS)
	$(OCAMLC) $(OCAML_INCL) -package 'unix'  -linkpkg \
          -o $@ $(SCILINT_DOC_GEN_CMOS)

depend:
	$(OCAMLDEP) -native $(OCAML_INCL) \
	  $(SCILINT_DOC_GEN_MLS) $(SCILINT_DOC_GEN_MLIS) \
	  $(SCILINT_MLS) $(SCILINT_MLIS) \
	  $(SCINTAX_MLS) $(SCINTAX_MLIS) > .depend_ocaml

include .depend_ocaml

ChangeLog.txt: scilint_doc_gen.asm
	scilint_doc_gen.asm -changelog-txt ChangeLog.txt

########## COMMON
.SUFFIXES: .ml .mli .mll .mly .cmi .cmx .cmo

.ml.cmx:
	$(OCAMLOPT) $(OPTFLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(OCAML_INCL) -c $<

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

.mly.cmo:
	$(OCAMLYACC) $<
	$(OCAMLC) $(OCAML_INCL) -c $*.mli
	$(OCAMLC) $(OCAML_INCL) -c $*.ml

.mly.cmi:
	$(OCAMLYACC) -v $<
	$(OCAMLOPT) $(OPTFLAGS) -c $*.mli

clean :
	rm -f \
	  *.old \
	  *~ */*~ */*/*~ */*/*/*~ \
	  *.cm* */*.cm* */*/*.cm* */*/*/*.cm* \
	  *.o */*.o */*/*.o */*/*/*.o \
	  scilint.byte scilint.asm \
	  scilint_doc_gen.byte scilint_doc_gen.asm \
	  scintax.byte scintax.asm \
	  src/parser/scilab_six/scilabSixLexer.ml \
	  src/parser/scilab_six/scilabSixGenParser.ml \
	  src/parser/scilab_six/scilabSixGenParser.mli \
	  src/scilint/config/scilintLexer.ml \
	  src/scilint/config/scilintParser.ml \
	  src/scilint/config/scilintParser.mli \
	  src/input/scilabTypedPrimitivesLexer.ml \
	  src/input/scilabTypedPrimitivesParser.ml \
	  src/input/scilabTypedPrimitivesParser.mli
