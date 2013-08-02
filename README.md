scilint
=======

A tool to analyze Scilab projects to find potential bugs


======

How to use scilint :

- -t : this will lauche analyzes on a hard coded list of scilab files (scilab5 + scilab6 + scilab forge). These analyzes are stats recorder (see module ScilabAstStats) and function analyzer (see ScilabFunctionAnalyze)

- -a file.sci : lauch analyzes (same as -t option) on a given file

- -typ file.sci : run function analyze for a given file

- -cfg : if this command is launch in a scilab project scilint will try to create a .scilint file to describe this project

- -load file.scilint : load a config file (hand written or generate with -cfg)