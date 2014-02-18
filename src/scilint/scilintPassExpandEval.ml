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

(* This is not so simple as it sounds.

   We basically do statically what is done by eval's implementation :
   when eval is called on a single string argument, it interpretes the
   code prepended by "%_h= ".  This means that if one can write a
   sequence of instructions, the result is the result of the first
   one, parsed as an expression, and the rest is executed. The
   interpretation is performed in a sub-environment.

   So, for each toplevel expression, we maintain an environment of
   temporary variables. When an eval is encountered with a single,
   static string parameter str, we build a new variable "%_t_n" and
   parse the string "%_t_n= " ^ str as a program. If the result is a
   single assignment, we drop the temporaty variable and replace the
   eval expression by the right hand side of the parsed assignment. If
   the result is a sequence, we build a fake function containing the
   parsed sequence before the toplevel expression and replace the eval
   with a call to this function.

   For instance [x = eval ('3')] will be replaced by [x = 3] and [r =
   eval ('3, disp a') + eval ('2, disp b')] will be rewritten as
   [function %_h_0 = %_h_0_f (), %_h_0 = 3, disp "a", endfunction;
    function %_h_1 = %_h_1_f (), %_h_1 = 2, disp "b", endfunction;
    r = %_h_0_f () + %_h_1_f ()]. *)

let pass ast =
  let rewriter = object (self)
    inherit ast_mapper as dad
    val mutable nvar = 0
    val mutable instrs = []
    method reset () =
      nvar <- 0 ;
      instrs <- []
    method fresh () =
      let cvar = Printf.sprintf "%%_h_%d" nvar in
      nvar <- nvar + 1 ;
      cvar
    method push is =
      instrs <- is :: instrs
    method! ast ast =
      List.flatten
        (List.map
           (fun stmt ->
              self # reset () ;
              let res = self # stmt stmt in
              List.rev (res :: instrs))
           ast)
    method! stmt_cstr = function
      | Seq is -> Seq (self # ast is)
      | cstr -> dad # stmt_cstr cstr

    method! exp exp =
      let exp = dad # exp exp in
      match exp.cstr with
      | Call ({ cstr = Var { cstr = "eval" }}, [ None, { cstr = String code } ], _)->
        let var = self # fresh () in
        let code = var ^ "= " ^ code in
        begin match ScilintOptions.SelectedParser.parse_exec_string exp.loc code with
          | [ { cstr = Assign ([ { cstr = Var { cstr = c }} ], exp)} ] when c = var -> 
            exp
          | { cstr = Assign ([ { cstr = Var { cstr = c }} ], exp)} :: _ as instrs when c = var -> 
            self # push (ghost (Defun { name = ghost (var ^ "_f") ;
                                        args = [] ; rets = [ ghost var ] ;
                                        body = ghost (Seq instrs) })) ;
            ghost (Call (ghost (Var (ghost (var ^ "_f"))), [], Tuplified))
          | _ ->
            { exp with meta = Recovered "unparsable eval" :: exp.meta }
        end
      | Call ({ cstr = Var { cstr = "eval" }}, _, _) ->
        { exp with meta = Recovered "unparsable eval" :: exp.meta }
      | _ -> exp
  end in
  rewriter # ast ast

let _ = (* plug it in *)
  ScilintOptions.add_pass
    "expand-eval" pass
    "expansion of 'eval' and 'deff'"
    true
