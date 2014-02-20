(*  OCamlPro Scilab Toolbox - Scilab 6 parser OCaml port
 *  Copyright (C) 2013 - OCamlPro - Michael LAPORTE
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

%{

open ScilabParserAst
open ScilabSixParserUtils
open Lexing

exception Error (* menhir workaround *)

%}

%token SOF
%token LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE SPACES
%token COMMA EOL DOLLAR SEMI IF THEN ELSE ELSEIF END WHILE DO
%token COLON ASSIGN FOR FUNCTION ENDFUNCTION HIDDEN HIDDENFUNCTION
%token PLUS MINUS RDIVIDE LDIVIDE TIMES POWER EQ NE LT GT LE GE
%token SELECT SWITCH OTHERWISE CASE TRY CATCH RETURN BREAK CONTINUE
%token BOOLTRUE BOOLFALSE QUOTE AND NOT DOT DOTQUOTE DOTTIMES
%token DOTLDIVIDE DOTRDIVIDE DOTPOWER OR KRONTIMES CONTROLTIMES
%token CONTROLLDIVIDE CONTROLRDIVIDE LINEBREAK KRONLDIVIDE KRONRDIVIDE

%token<float> VARINT
%token<float> NUM
%token<string> ID
%token<string> COMMENT
%token<string> STR
%token EOF

%left OR
%left AND
%left EQ NE LT LE GT GE
%left MINUS PLUS
%left TIMES DOTTIMES KRONTIMES CONTROLTIMES RDIVIDE DOTRDIVIDE
      KRONRDIVIDE CONTROLRDIVIDE LDIVIDE
      DOTLDIVIDE KRONLDIVIDE CONTROLLDIVIDE
%left UNARY
%left POWER DOTPOWER
%left QUOTE DOTQUOTE
%left NOT


%start program
%type <ScilabParserAst.ast>program

%%

%inline commented (rule):
| descr = rule { descr }
| exp = rule com = COMMENT
  { let comment =  [ string_descr com (loc $startpos(com) $endpos(com)) ] in
    { exp with comment } }

%inline ident:
| id = ID { string_descr id (loc $startpos $endpos)}
| DOLLAR { string_descr "$" (loc $startpos $endpos)}

program:
| list (stmt_break) stmts = stmts_r (EOF) { stmts }

stmts (stop):
| list (stmt_break) stmts = stmts_r (stop)
  { match stmts with [ s ] -> s | _ -> descr (Seq stmts) (loc $startpos $endpos)}

stmts_r (stop):
| first = stmt nonempty_list (stmt_break) rest = stmts_r (stop) { first :: rest }
| single = stmt stop { [ single ]  }
| stop { [] }
    
stmt:
| exp = commented (toplevel_expression)
  { let loc = loc $startpos $endpos in
    descr (Exp exp) loc }
| stmt = if_control
| stmt = select_control
| stmt = try_control
| stmt = for_control
| stmt = while_control
| stmt = function_declaration { stmt }
| com = COMMENT { descr (Comment com) (loc $startpos(com) $endpos(com)) }
| BREAK { descr Break (loc $startpos $endpos) }
| RETURN { descr Return (loc $startpos $endpos) }
| CONTINUE { descr Continue (loc $startpos $endpos) }
| l = assignable ASSIGN
  r = expression
  { let loc = loc $startpos $endpos in
    descr (Assign (l, r)) loc }
| l = assignable ASSIGN
  r = RETURN LPAREN args = separated_list (COMMA, call_arg) RPAREN
  { let floc = loc $startpos(r) $endpos(r) in
    let f = var_descr "return" floc in
    let rloc = loc $startpos(r) $endpos in
    let r = descr (Call (f, args, Tuplified)) rloc in
    let loc = loc $startpos $endpos in
    descr (Assign (l, r)) loc }

%inline assignable :
| single = injection { [ single ] }
| mat = matrix
  (* ugly but drastically reduces the grammar *)
  { match mat with
    | { cstr = Matrix [ { cstr = items } ] } ->
      (* FIXME: check only injections *)
      items
    | _ -> [ { mat with cstr = ScilabParserAst.Error } ] }

%inline stmt_break : SEMI | COMMA | EOL {}

%inline toplevel_expression :
| exp = expression { exp }
| f = ident args = nonempty_list (shell_call_arg)
  { let f = descr (Var f) f.loc in
    let loc = loc $startpos $endpos in
    let anon (arg, loc) = None, descr (String arg) loc in
    descr (Call (f, List.map anon args , Shell)) loc }

shell_call_arg:
| text = ID
| text = STR { text, loc $startpos $endpos }
| num = VARINT
| num = NUM { string_of_float num, loc $startpos $endpos }
| DOLLAR { "$", loc $startpos $endpos }
| BOOLTRUE { "%T", loc $startpos $endpos }
| BOOLFALSE { "%F", loc $startpos $endpos }
| l = shell_call_arg DOT r = field_name { fst l ^ "." ^ r, loc $startpos $endpos }

call_arg:
| exp = expression { None, exp }
| var = ident ASSIGN exp = expression  { Some var, exp }

function_declaration :
| FUNCTION rets = defun_rets name = ident
  args = defun_args body = stmts (defun_break)
| hidden rets = defun_rets name = ident
  args = defun_args body = stmts (defun_break)
  { let loc = loc $startpos $endpos in
    descr (Defun { name ; args ; body ; rets }) loc }

defun_break: END | ENDFUNCTION {}

hidden: HIDDENFUNCTION | HIDDEN FUNCTION {}

%inline defun_args:
| LPAREN args = separated_list (COMMA, ident) RPAREN defun_args_break { args }
| defun_args_break { [] }

%inline defun_rets:
| LBRACK rets = separated_list (COMMA, ident) RBRACK ASSIGN { rets }
| ret = ident ASSIGN { [ ret ] }
| /* empty */ { [] }

defun_args_break: SEMI | EOL | COMMA {}

expression:
| exps = separated_nonempty_list (COLON, simple_expression) 
  { match exps with
    | [] -> assert false
    | [ exp ] -> exp
    | [ l ; s ; r ] ->
      descr (Range (l, Some s, r)) (loc $startpos $endpos)
    | [ l ; r ] ->
      descr (Range (l, None, r)) (loc $startpos $endpos)
    | _ ->
      (* FIXME: warning *)
      descr ScilabParserAst.Error (loc $startpos $endpos) }

simple_expression:
| NOT exp = simple_expression %prec NOT
  { descr (Unop (Not, exp)) (loc $startpos $endpos) }
| num = VARINT
| num = NUM { descr (Num num) (loc $startpos $endpos) }
| text = STR { descr (String text) (loc $startpos $endpos) }
| COLON { descr Colon (loc $startpos $endpos) }
| BOOLTRUE { descr (Bool true) (loc $startpos $endpos) }
| BOOLFALSE { descr (Bool false) (loc $startpos $endpos) }
| LPAREN exps = separated_list (COMMA, simple_expression) RPAREN
  { descr (Identity exps) (loc $startpos $endpos) }
| exp = matrix
| exp = cell
| exp = injection { exp }
| l = simple_expression AND r = simple_expression
  { descr (Op (And, l, r)) (loc $startpos $endpos) }
| l = simple_expression OR r = simple_expression
  { descr (Op (Or, l, r)) (loc $startpos $endpos) }
| l = simple_expression EQ r = simple_expression
  { descr (Op (Eq, l, r)) (loc $startpos $endpos) }
| l = simple_expression NE r = simple_expression
  { descr (Op (Ne, l, r)) (loc $startpos $endpos) }
| l = simple_expression LT r = simple_expression
  { descr (Op (Lt, l, r)) (loc $startpos $endpos) }
| l = simple_expression GT r = simple_expression
  { descr (Op (Gt, l, r)) (loc $startpos $endpos) }
| l = simple_expression LE r = simple_expression
  { descr (Op (Le, l, r)) (loc $startpos $endpos) }
| l = simple_expression GE r = simple_expression
  { descr (Op (Ge, l, r)) (loc $startpos $endpos) }
| l = simple_expression PLUS r = simple_expression
  { descr (Op (Plus, l, r)) (loc $startpos $endpos) }
| l = simple_expression MINUS r = simple_expression
  { descr (Op (Minus, l, r)) (loc $startpos $endpos) }
| l = simple_expression TIMES r = simple_expression
  { descr (Op (Times, l, r)) (loc $startpos $endpos) }
| l = simple_expression DOTTIMES r = simple_expression
  { descr (Op (Dot_times, l, r)) (loc $startpos $endpos) }
| l = simple_expression KRONTIMES r = simple_expression
  { descr (Op (Kron_times, l, r)) (loc $startpos $endpos) }
| l = simple_expression CONTROLTIMES r = simple_expression
  { descr (Op (Control_times, l, r)) (loc $startpos $endpos) }
| l = simple_expression RDIVIDE r = simple_expression
  { descr (Op (Rdivide, l, r)) (loc $startpos $endpos) }
| l = simple_expression DOTRDIVIDE r = simple_expression
  { descr (Op (Dot_rdivide, l, r)) (loc $startpos $endpos) }
| l = simple_expression KRONRDIVIDE r = simple_expression
  { descr (Op (Kron_rdivide, l, r)) (loc $startpos $endpos) }
| l = simple_expression CONTROLRDIVIDE r = simple_expression
  { descr (Op (Control_rdivide, l, r)) (loc $startpos $endpos) }
| l = simple_expression LDIVIDE r = simple_expression
  { descr (Op (Ldivide, l, r)) (loc $startpos $endpos) }
| l = simple_expression DOTLDIVIDE r = simple_expression
  { descr (Op (Dot_ldivide, l, r)) (loc $startpos $endpos) }
| l = simple_expression KRONLDIVIDE r = simple_expression
  { descr (Op (Kron_ldivide, l, r)) (loc $startpos $endpos) }
| l = simple_expression CONTROLLDIVIDE r = simple_expression
  { descr (Op (Control_ldivide, l, r)) (loc $startpos $endpos) }
| l = simple_expression POWER r = simple_expression
  { descr (Op (Power, l, r)) (loc $startpos $endpos) }
| l = simple_expression DOTPOWER r = simple_expression
  { descr (Op (Dot_power, l, r)) (loc $startpos $endpos) }
| MINUS exp = simple_expression %prec UNARY
  { descr (Unop (Unary_minus, exp)) (loc $startpos $endpos) }
| PLUS exp = simple_expression %prec UNARY
  { descr (Unop (Unary_plus, exp)) (loc $startpos $endpos) }
| exp = simple_expression QUOTE
  { descr (Unop (Transpose_conjugate, exp)) (loc $startpos $endpos) }
| exp = simple_expression DOTQUOTE
  { descr (Unop (Transpose_non_conjugate, exp)) (loc $startpos $endpos) }

if_control:
| IF stmt = if_body { stmt }

if_body:
| cond = expression if_then_token b = stmts (END)
  { let loc = loc $startpos $endpos in
    descr (If (cond, b, None)) loc }
| cond = expression if_then_token t = stmts (ELSE) f = stmts (END)
  { let loc = loc $startpos $endpos in
    descr (If (cond, t, Some f)) loc }
| cond = expression if_then_token t = stmts (ELSEIF) f = if_body
  { let loc = loc $startpos $endpos in
    descr (If (cond, t, Some f)) loc }

%inline if_cond_break : SEMI | COMMA | EOL | COMMENT {}

if_then_token : THEN | if_cond_break option (THEN) {}

select_control :
| SELECT cond = commented (expression) select_cond_break CASE cases = select_cases
| SWITCH cond = commented (expression) select_cond_break CASE cases = select_cases
  { let cases, default = cases in
    let loc = loc $startpos $endpos in
    descr (Select { cond ; cases ; default }) loc }

%inline select_cond_break: COMMA | SEMI | EOL | COMMENT {}

select_cases:
| first = select_case (CASE) rest = select_cases
  { let rest, def = rest in first :: rest, def }
| single = select_case (ELSE) def = stmts (END)
| single = select_case (OTHERWISE) def = stmts (END)
  { [ single ], Some def }
| single = select_case (END)
  { [ single ], None }

select_case (stop):
| exp = expression case_cond_break body = stmts (stop) { exp, body }

%inline case_cond_break: THEN | COMMA | SEMI | EOL | COMMENT {}

for_control:
| FOR v = ident ASSIGN r = expression for_cond_break body = stmts (END)
| FOR LPAREN v = ident ASSIGN r = expression RPAREN for_cond_break body = stmts (END)
  { let loc = loc $startpos $endpos in
    descr (For (v, r, body)) loc }

%inline for_cond_break:
| EOL | SEMI | COMMA | DO | COMMENT {}

while_control:
| WHILE cond = expression while_cond_break b = stmts (END)
  { let loc = loc $startpos $endpos in
    descr (While (cond, b, None)) loc }
| WHILE cond = expression while_cond_break t = stmts (ELSE) f = stmts (END)
  { let loc = loc $startpos $endpos in
    descr (While (cond, t, Some f)) loc }

%inline while_cond_break: COMMA | SEMI | DO | THEN | COMMENT {}

try_control:
| TRY t = stmts (CATCH) c = stmts (END)
  { let loc = loc $startpos $endpos in
    descr (Try (t, c)) loc }
| TRY t = stmts (END)
  { descr (Seq []) (Forged, ((0, 0), (0, 0))) }

%inline matrix:
| LBRACK
  list (COMMA) option (matrix_line_breaks)
  lines = separated_nonempty_list (matrix_line_breaks, matrix_line)
  RBRACK
  { descr (Matrix lines) (loc $startpos $endpos) }
| LBRACK
  list (COMMA) option (matrix_line_breaks)
  RBRACK
  { descr (Matrix []) (loc $startpos $endpos) }

%inline cell:
| LBRACE
  list (COMMA) option (matrix_line_breaks)
  lines = separated_nonempty_list (matrix_line_breaks, matrix_line)
  RBRACE
  { descr (Cell_array lines) (loc $startpos $endpos) }
| LBRACE
  list (COMMA) option (matrix_line_breaks)
  RBRACE
  { descr (Cell_array []) (loc $startpos $endpos) }

%inline matrix_line_break: SEMI | EOL {}

%inline matrix_line_breaks:
| nonempty_list (terminated (matrix_line_break, list (COMMA))) {}

%inline matrix_column_breaks:
| nonempty_list (COMMA) {}

%inline matrix_line :
| cells = separated_nonempty_list (matrix_column_breaks, expression)
  { descr cells (loc $startpos $endpos) }

injection:
| n = ident { descr (Var n) n.loc}
| f = injection LPAREN args = separated_list (COMMA, call_arg) RPAREN
  { let loc = loc $startpos $endpos in
    descr (Call (f, args, Tuplified)) loc }
| f = BOOLTRUE LPAREN args = separated_list (COMMA, call_arg) RPAREN
  { let f = var_descr "%T" (loc $startpos(f) $endpos(f)) in
    let loc = loc $startpos $endpos in
    descr (Call (f, args, Tuplified)) loc }
| f = BOOLFALSE LPAREN args = separated_list (COMMA, call_arg) RPAREN
  { let f = var_descr "%F" (loc $startpos(f) $endpos(f)) in
    let loc = loc $startpos $endpos in
    descr (Call (f, args, Tuplified)) loc }
| l = injection DOT m = field_name
  { let m = var_descr m (loc $startpos(m) $endpos(m)) in
    let loc = loc $startpos $endpos in
    descr (Call (l, [ None, m ], Field)) loc }

field_name:
| id = ID { id }
| IF { "if" }
| THEN { "then" }
| ELSE { "else" }
| ELSEIF { "elseif" }
| END { "end" }
| SELECT { "select" }
| SWITCH { "switch" }
| OTHERWISE { "otherwise" }
| CASE { "case" }
| FUNCTION { "function" }
| ENDFUNCTION { "endfunction" }
| HIDDENFUNCTION { "#function" }
| HIDDEN { "#" }
| FOR { "for" }
| WHILE { "while" }
| DO { "do" }
| BREAK { "break" }
| TRY { "try" }
| CATCH { "catch" }
| RETURN { "return" }

%%
