(*  OCamlPro Scilab Toolbox - Scilint - Eval expansion pass
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabParserAst
open ScilintWarning
open Printf

let pass ast =
  let rewriter = object
    inherit ast_mapper as dad
    method! exp exp =
      let exp = dad # exp exp in
      match exp.cstr with
      | Call ({ cstr = Var { cstr = "deff" }}, _, _) ->
        { exp with meta = Recovered "unparsable deff" :: exp.meta ;
                   cstr = Error }
      | _ -> exp
  end in
  rewriter # ast ast

let _ = (* plug it in *)
  ScilintOptions.add_pass
    "expand-eval" pass
    "expansion of 'eval' and 'deff'"
    true
