%{%}

%token FILES EQ LB RB
%token<string> STR

%start config
%type <ScilintTree.t>config

%%
config :
| FILES EQ LB files_list RB  { ScilintTree.Files $4 }

files_list :
| /* Empty */                        { [] }
| STR files_list                     { $1::$2 }










