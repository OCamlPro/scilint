.PHONY: clean

all: \
  scilint.asm scintax.asm scifind.asm scilint_doc_gen.asm scilob.asm \
  scilint.byte scintax.byte scifind.byte scilint_doc_gen.byte scilob.byte \
  sciweb.js

OCAMLOPT=ocamlfind ocamlopt -g -safe-string -strict-formats
OCAMLC=ocamlfind ocamlc -g -safe-string -strict-formats
OCAMLYACC=menhir
OCAMLLEX=ocamllex
OCAMLDEP=ocamlfind ocamldep

OCAML_COMMON_MLS = \
  src/third_party/ptmap.ml \
  src/third_party/ptset.ml \
  src/common/scilintManual.ml \
  src/common/scilintWarning.ml \
  src/common/scilabLocations.ml \
  src/input/scilabTypedPrimitives.ml \
  src/input/scilabTypedPrimitivesLexer.ml \
  src/input/scilabTypedPrimitivesParser.ml \
  src/input/scilabTypedPrimitivesLoader.ml

OCAML_COMMON_MLIS = \
  src/third_party/ptset.mli \
  src/third_party/ptmap.mli \
  src/common/scilintWarning.mli \

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
  src/common/scilintOptions.ml \
  src/scintax/scintaxMain.ml

OCAML_SCIFIND_MLS = \
  src/common/scilintOptions.ml \
  src/scifind/scifindMain.ml

OCAML_SCILOB_MLS = \
  src/common/scilintOptions.ml \
  src/interp/interpStatePureOCaml.ml \
  src/interp/interpValuesPureOCaml.ml \
  src/interp/interpDispatcher.ml \
  src/interp/interpCore.ml \
  src/interp/interpMessages.ml \
  src/interp/interpLib.ml \
  src/interp/interpMathLib.ml \
  src/interp/interp.ml \
  src/interp/interpMain.ml

OCAML_SCILOB_MLIS = \
  src/interp/interpState.mli \
  src/interp/interpValues.mli \

OCAML_SCIWEB_MLS = \
  src/common/scilintOptions.ml \
  src/interp/interpStatePureOCaml.ml \
  src/interp/interpValuesPureOCaml.ml \
  src/interp/interpDispatcher.ml \
  src/interp/interpCore.ml \
  src/interp/interpMessages.ml \
  src/interp/interpLib.ml \
  src/interp/interpMathLib.ml \
  src/interp/interp.ml \
  src/common/tyxml_js_manip.ml \
  src/interp/interpWebLib.ml \
  src/interp/interpWebMain.ml

OCAML_SCIWEB_MLIS = \
  src/interp/interpState.mli \
  src/interp/interpValues.mli \
  src/common/tyxml_js_manip.mli

OCAML_SCILINT_DOC_GEN_MLS = \
  src/docgen/scilintDocGenMain.ml

OCAML_SCILINT_MLS = \
  src/common/scilintOptions.ml \
  src/scilint/scilabUtils.ml \
  src/scilint/scilintProject.ml \
  src/scilint/scilintPassExpandEval.ml \
  src/scilint/scilabFiveFunctionAnalyze.ml \
  src/scilint/scilintMain.ml

src/scilint/config/scilintLexer.cmx: \
  src/scilint/config/scilintParser.cmi

OCAML_SCILINT_MLIS = \
  src/scilint/scilabUtils.mli \
  src/scilint/scilintProject.mli

########## SCILINT

SCILINT_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_AST_MLS) \
	$(OCAML_SCILAB_FIVE_PARSER_MLS) \
	$(OCAML_SCILAB_SIX_PARSER_MLS) \
	$(OCAML_SCILINT_MLS)

SCILINT_MLIS = \
	$(OCAML_COMMON_MLIS) \
	$(OCAML_SCILINT_MLIS)

SCILINT_CMIS = $(SCILINT_MLS:.ml=.cmi) $(SCILINT_MLIS:.mli=.cmi)
SCILINT_CMXS = $(SCILINT_MLS:.ml=.cmx)
SCILINT_CMOS = $(SCILINT_MLS:.ml=.cmo)

########## SCINTAX

SCINTAX_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_AST_MLS) \
	$(OCAML_SCILAB_FIVE_PARSER_MLS) \
	$(OCAML_SCILAB_SIX_PARSER_MLS) \
	$(OCAML_SCINTAX_MLS)

SCINTAX_MLIS = \
	$(OCAML_COMMON_MLIS) \

SCINTAX_CMIS = $(SCINTAX_MLS:.ml=.cmi) $(SCINTAX_MLIS:.mli=.cmi)
SCINTAX_CMXS = $(SCINTAX_MLS:.ml=.cmx)
SCINTAX_CMOS = $(SCINTAX_MLS:.ml=.cmo)

########## SCIFIND

SCIFIND_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_AST_MLS) \
	$(OCAML_SCILAB_FIVE_PARSER_MLS) \
	$(OCAML_SCILAB_SIX_PARSER_MLS) \
	$(OCAML_SCIFIND_MLS)

SCIFIND_MLIS = \
	$(OCAML_COMMON_MLIS) \

SCIFIND_CMIS = $(SCIFIND_MLS:.ml=.cmi) $(SCIFIND_MLIS:.mli=.cmi)
SCIFIND_CMXS = $(SCIFIND_MLS:.ml=.cmx)
SCIFIND_CMOS = $(SCIFIND_MLS:.ml=.cmo)

########## SCILOB

SCILOB_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_AST_MLS) \
	$(OCAML_SCILAB_FIVE_PARSER_MLS) \
	$(OCAML_SCILAB_SIX_PARSER_MLS) \
	$(OCAML_SCILOB_MLS)

SCILOB_MLIS = \
	$(OCAML_COMMON_MLIS) \
	$(OCAML_SCILOB_MLIS)

SCILOB_CMIS = $(SCILOB_MLS:.ml=.cmi) $(SCILOB_MLIS:.mli=.cmi)
SCILOB_CMXS = $(SCILOB_MLS:.ml=.cmx)
SCILOB_CMOS = $(SCILOB_MLS:.ml=.cmo)

########## SCIWEB

SCIWEB_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_AST_MLS) \
	$(OCAML_SCILAB_FIVE_PARSER_MLS) \
	$(OCAML_SCILAB_SIX_PARSER_MLS) \
	$(OCAML_SCIWEB_MLS)

SCIWEB_MLIS = \
	$(OCAML_COMMON_MLIS) \
	$(OCAML_SCIWEB_MLIS)

SCIWEB_CMIS = $(SCIWEB_MLS:.ml=.cmi) $(SCIWEB_MLIS:.mli=.cmi)
SCIWEB_CMXS = $(SCIWEB_MLS:.ml=.cmx)
SCIWEB_CMOS = $(SCIWEB_MLS:.ml=.cmo)

########## SCILINT_DOC_GEN

SCILINT_DOC_GEN_MLS = \
	$(OCAML_COMMON_MLS) \
	$(OCAML_SCILINT_DOC_GEN_MLS)

SCILINT_DOC_GEN_MLIS = \
	$(OCAML_COMMON_MLIS) \

SCILINT_DOC_GEN_CMIS = $(SCILINT_DOC_GEN_MLS:.ml=.cmi) $(SCILINT_DOC_GEN_MLIS:.mli=.cmi)
SCILINT_DOC_GEN_CMXS = $(SCILINT_DOC_GEN_MLS:.ml=.cmx)
SCILINT_DOC_GEN_CMOS = $(SCILINT_DOC_GEN_MLS:.ml=.cmo)

########## COMMON FLAGS

OCAML_INCL= \
  -package 'unix,uutf,pprint,re,re.pcre,re.posix' \
  -I src/common -I src/input \
  -I src/ast -I src/parser/scilab_five \
  -I src/third_party \
  -I src/parser/scilab_six \
  -I src/scilint -I src/scilint/config \
  -I src/scintax -I src/scifind -I src/docgen \
  -I src/interp

OPTFLAGS = -g -fPIC

scilint.asm : $(SCILINT_CMXS)
	$(OCAMLOPT) $(OCAML_INCL) -linkpkg \
	  -o $@ $(SCILINT_CMXS)

scintax.asm : $(SCINTAX_CMXS)
	$(OCAMLOPT) $(OPTFLAGS) $(OCAML_INCL) -linkpkg \
          -o $@ $(SCINTAX_CMXS)

scifind.asm : $(SCIFIND_CMXS)
	$(OCAMLOPT) $(OPTFLAGS) $(OCAML_INCL) -linkpkg \
          -o $@ $(SCIFIND_CMXS)

sciweb.byte: $(SCIWEB_CMOS)
	$(OCAMLC) $(OCAML_INCL) $(OCAML_INCL) \
          -package 'js_of_ocaml.tyxml,js_of_ocaml.syntax' -linkpkg -o $@ $^

sciweb.js: sciweb.byte
	js_of_ocaml +weak.js $< -o $@

src/common/tyxml_js_manip.cmo \
src/common/tyxml_js_manip.cmi \
src/interp/interpWebLib.cmo \
src/interp/interpWebMain.cmo: \
        OCAML_INCL += \
          -package 'js_of_ocaml.tyxml,js_of_ocaml.syntax' \
          -syntax camlp4o

scilob.asm : $(SCILOB_CMXS)
	$(OCAMLOPT) $(OPTFLAGS) $(OCAML_INCL) -linkpkg \
          -o $@ $(SCILOB_CMXS)

scilint_doc_gen.asm : $(SCILINT_DOC_GEN_CMXS)
	$(OCAMLOPT) $(OPTFLAGS) $(OCAML_INCL) -package 'unix' -linkpkg \
          -o $@ $(SCILINT_DOC_GEN_CMXS)

scilint.byte : $(SCILINT_CMOS)
	$(OCAMLC) $(OCAML_INCL) -linkpkg \
	  -o $@ $(SCILINT_CMOS)

scintax.byte : $(SCINTAX_CMOS)
	$(OCAMLC) $(OCAML_INCL) $(OCAML_INCL) -linkpkg \
          -o $@ $(SCINTAX_CMOS)

scifind.byte : $(SCIFIND_CMOS)
	$(OCAMLC) $(OCAML_INCL) $(OCAML_INCL) -linkpkg \
          -o $@ $(SCIFIND_CMOS)

scilob.byte : $(SCILOB_CMOS)
	$(OCAMLC) $(OCAML_INCL) $(OCAML_INCL) -linkpkg \
          -o $@ $(SCILOB_CMOS)

scilint_doc_gen.byte : $(SCILINT_DOC_GEN_CMOS)
	$(OCAMLC) $(OCAML_INCL) -package 'unix' -linkpkg \
          -o $@ $(SCILINT_DOC_GEN_CMOS)

.depend_ocaml: $(SCILINT_DOC_GEN_MLS) $(SCILINT_DOC_GEN_MLIS) \
	  $(OCAML_COMMON_MLS) $(OCAML_COMMON_MLIS) \
	  $(SCILINT_MLS) $(SCILINT_MLIS) \
	  $(SCINTAX_MLS) $(SCINTAX_MLIS) \
	  $(SCIFIND_MLS) $(SCIFIND_MLIS) \
	  $(SCILOB_MLS) $(SCILOB_MLIS) \
	  $(SCIWEB_MLS) $(SCIWEB_MLIS)
	$(OCAMLDEP) -native $(OCAML_INCL) \
          $(SCILINT_DOC_GEN_MLS) $(SCILINT_DOC_GEN_MLIS) \
	  $(OCAML_COMMON_MLS) $(OCAML_COMMON_MLIS) \
	  $(SCILINT_MLS) $(SCILINT_MLIS) \
	  $(SCINTAX_MLS) $(SCINTAX_MLIS) \
	  $(SCIFIND_MLS) $(SCIFIND_MLIS) \
	  $(SCILOB_MLS) $(SCILOB_MLIS) \
          > .depend_ocaml
	$(OCAMLDEP) $(OCAML_INCL) \
          src/common/tyxml_js_manip.ml \
          src/common/tyxml_js_manip.mli \
          src/interp/interpWebLib.ml \
          src/interp/interpWebMain.ml \
          -package 'js_of_ocaml.tyxml,js_of_ocaml.syntax' \
          -syntax camlp4o >> .depend_ocaml

include .depend_ocaml

ChangeLog.txt: scilint_doc_gen.asm
	scilint_doc_gen.asm -changelog-txt ChangeLog.txt

########## COMMON
.SUFFIXES: .ml .mli .mll .mly .cmi .cmx .cmo

.ml.cmx:
	$(OCAMLOPT) $(OPTFLAGS) $(OCAML_INCL) -c $<

.ml.cmo:
	$(OCAMLC) $(OCAML_INCL) -c $<

.mll.cmx:
	$(OCAMLLEX) $<
	$(OCAMLOPT) $(OPTFLAGS) $(OCAML_INCL) -c $*.ml

.mli.cmi:
	$(OCAMLOPT) $(OPTFLAGS) $(OCAML_INCL) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.cmx:
	$(OCAMLYACC) $<
	$(OCAMLOPT) $(OPTFLAGS) $(OCAML_INCL) -c $*.mli
	$(OCAMLOPT) $(OPTFLAGS) $(OCAML_INCL) -c $*.ml

.mly.cmo:
	$(OCAMLYACC) $<
	$(OCAMLC) $(OCAML_INCL) -c $*.mli
	$(OCAMLC) $(OCAML_INCL) -c $*.ml

.mly.cmi:
	$(OCAMLYACC) -v $<
	$(OCAMLOPT) $(OPTFLAGS) $(OCAML_INCL) -c $*.mli

clean:
	rm -fr \
	  *.old _obuild \
	  *~ */*~ */*/*~ */*/*/*~ \
	  *.cm* */*.cm* */*/*.cm* */*/*/*.cm* \
	  *.o */*.o */*/*.o */*/*/*.o \
	  scilint.byte scilint.asm \
	  scilint_doc_gen.byte scilint_doc_gen.asm \
	  scintax.byte scintax.asm \
	  scifind.byte scifind.asm \
	  scilob.byte scilob.asm \
	  src/parser/scilab_six/scilabSixLexer.ml \
	  src/parser/scilab_six/scilabSixGenParser.ml \
	  src/parser/scilab_six/scilabSixGenParser.mli \
	  src/scilint/config/scilintLexer.ml \
	  src/scilint/config/scilintParser.ml \
	  src/scilint/config/scilintParser.mli \
	  src/input/scilabTypedPrimitivesLexer.ml \
	  src/input/scilabTypedPrimitivesParser.ml \
	  src/input/scilabTypedPrimitivesParser.mli
