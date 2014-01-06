(*  OCamlPro Scilab Toolbox - Typed primitives
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

{ open ScilabTypedPrimitivesParser }

let alpha = [ 'a'-'z' 'A'-'Z' '_']
let digit = [ '0'-'9' ]
let alnum = alpha | digit | '_'

rule token = parse
(* ignored *)
| "//" [^ '\r' '\n' ]* { token lexbuf }
| [ ' ' '\t' ] { token lexbuf }
(* line enders *)
| '\n' | '\r' | "\r\n" | ';' { SEMI }
(* fixed terminals *)
| "..." { DOTDOTDOT }
| ':' { COLON }
| ',' { COMA }
| "=>" { ARROW }
| '*' { STAR }
| '+' { PLUS }
| '-' { MINUS }
| '/' { DIV }
| '(' { OPAR }
| ')' { CPAR }
| '[' { OBRA }
| ']' { CBRA }
| "int" { INT }
| "real" { REAL }
| "void" { VOID }
| "string" { STRING }
| "complex" { COMPLEX }
| "bool" { BOOL }
| "sparse" { SPARSE }
(* variable terminals *)
| '&' (digit+ as i) { DIMVAR (int_of_string i) }
| (digit+ as i) { CONST_INT (int_of_string i) }
| '\'' ([^ '\'' ]* as text) '\'' { CONST_STRING text }
| '@' (alnum+ as sec) '|' ([^ '\n' ]* as com) { MARKUP (sec, com) }
| ('R' alnum+ as text) { R text }
| ("NR" alnum+ as text) { NR text }
| "Op_" ([^ ' ' ':' '\t' '\n' ]+ as text) ( ' '* '(' [ ^ ')' ]* ')')? { OP text }
| "Macro_" ([^ ':' '\t' '\n' ]+ as text) { OP text }
| eof { EOF }
| (_ as c)
    { failwith
        (Printf.sprintf "Unexpected character %C at offset %d."
           c (Lexing.lexeme_start lexbuf)) }
