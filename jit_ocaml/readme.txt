!! No ATOMS packet for now !!

How make it load automatically in scilab5 :
- cp jit_ocaml dir into SCI/modules
- edit SCI/etc/modules.xml and add jit_ocaml to the list
- edit SCI/configure.ac and add modules/jit_ocaml/Makefile to AC_CONFIG_FILES
- edit SCI/modules/Makefile.am and add jit_ocaml to SUBDIRS 
- ./configure
- autoconf
- automake
- make

How to use it in scilab5 :
- jit_read("var_name") // will return the value of variable var_name
- jit_aff("var_name",1) // will affect 1 to variable var_name
