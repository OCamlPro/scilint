(*  OCamlPro Scilab Toolbox - Scilab 6 parser OCaml port
 *  Copyright (C) 2013 - OCamlPro - Michael LAPORTE
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabSixParserUtils
open ScilabSixLexer
open ScilabSixGenParser
open ScilintWarning

let parse_file name =
  let ch = open_in name in
  let new_prog, corrupt_zone = pre_parse ch in
  let lexbuf = Lexing.from_string new_prog in
  init_lexer_var ();
  try
    init_var_corrupt corrupt_zone;
    let ast = program token lexbuf in
    close_in ch;
    ast
  with
  | exn ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let loc = loc curr curr in
    close_in ch;
    let warns =
      match exn with
      | ScilabSixGenParser.Error -> [ Recovered "syntax error" ]
      | ScilabSixLexer.Error msg -> [ Recovered msg ]
      | _ -> [ Recovered ("internal parser error" ^ Printexc.to_string exn) ]
    in
    [ let exp = descr ~warns ScilabFiveParserAst.Error loc in
      descr (ScilabFiveParserAst.Exp exp) loc ]

let parse_string name str =
  failwith "ScilabSixParser.parse_string not implemented"
