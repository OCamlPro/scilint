all: warnings.pdf

warnings.pdf: warnings.tex warnings_tables.tex
	pdflatex warnings.tex

warnings_tables.tex: ../../_obuild/scilintDocgen/scilintDocgen.asm
	../../_obuild/scilintDocgen/scilintDocgen.asm -latex warnings_tables.tex

upload:
	rsync -auv warnings.pdf webmaster@ocamlpro.com:/home/scilint.ocamlpro.com/www/warnings.pdf
