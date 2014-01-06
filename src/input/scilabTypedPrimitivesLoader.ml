(*  OCamlPro Scilab Toolbox - Typed primitives
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

let load_from_file name =
  try
    let chan = open_in name in
    try
      let lexbuf = Lexing.from_channel chan in
      let res =
        try
          ScilabTypedPrimitivesParser.main
            ScilabTypedPrimitivesLexer.token
            lexbuf
        with
          ScilabTypedPrimitivesParser.Error ->
          failwith
            (Printf.sprintf "Syntax error at offset %d."
               (Lexing.lexeme_start lexbuf))
      in
      close_in chan ;
      res
    with
      Failure msg ->
      close_in chan ;
      Printf.eprintf "Internal error while reading file %S:\n%s\n%!" name msg ;
      []
  with
    Sys_error _ ->
    Printf.eprintf "Internal error: cannot open file %S\n%!" name ;
    []
