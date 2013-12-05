all: warnings.html changelog.html

warnings.html: ../../_obuild/scilintDocgen/scilintDocgen.asm
	../../_obuild/scilintDocgen/scilintDocgen.asm -warnings-html warnings.html

changelog.html: ../../_obuild/scilintDocgen/scilintDocgen.asm
	../../_obuild/scilintDocgen/scilintDocgen.asm -changelog-html changelog.html



upload:
	rsync -auv . webmaster@ocamlpro.com:/home/scilint.ocamlpro.com/www/.
