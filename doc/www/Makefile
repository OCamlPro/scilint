
warnings.html: ../../_obuild/scilintDocgen/scilintDocgen.asm
	../../_obuild/scilintDocgen/scilintDocgen.asm -html warnings.html

upload:
	rsync -auv . webmaster@ocamlpro.com:/home/scilint.ocamlpro.com/www/.
