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
    r = %_h_0_f () + %_h_1_f ()].

   The eval function cam also take a matrix of strings as parameter,
   and return a matrix of evaluation results. This is done in a very
   similar way: the code str of every cell (i, j) is evaluated by
   interpreting "%_h(i,j)= " ^ str. So when we encounter a matrix of
   constant strings fed to eval, we do the same thing as with a single
   string. If each parsed strings boils down to a single assignment,
   we replace the whole matrix by the matrix of resulting right hand
   sides. Otherwise, we build a temporary function that performs all
   the assignments, and replace the matrix litteral by a call to this
   generated function. *)

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
      let parse = ScilintOptions.SelectedParser.parse_exec_string in
      let exp = dad # exp exp in
      match exp.cstr with
      | Call ({ cstr = Var { cstr = "eval" }},
              [ None, { cstr = String code ; loc } ], _)->
        let var = self # fresh () in
        let code = var ^ "= " ^ code in
        begin match parse loc code with
          | { cstr = Assign (_, { cstr = Error })} :: _ ->
            exp
          | [ { cstr = Assign (_, exp')} ]  -> 
            let str = Pretty.to_compact_string [ ghost (Exp exp') ] in
            let meta = (exp.loc, Replace str) :: exp.meta in
            { exp' with meta }
          | { cstr = Assign (_, exp)} :: _ as instrs -> 
            self # push (ghost (Defun { name = ghost (var ^ "_f") ;
                                        args = [] ; rets = [ ghost var ] ;
                                        body = ghost (Seq instrs) })) ;
            ghost (Call (ghost (Var (ghost (var ^ "_f"))), [], Tuplified))
          | _ -> exp
          (* { exp with meta = Recovered "unparsable eval" :: exp.meta } *)
        end
      | Call ({ cstr = Var { cstr = "eval" }},
              [ None, { cstr = Matrix codes ; loc ; meta ; comment } ], _) ->
        begin try
            let cells =
              List.mapi
                (fun i { cstr = cells ; loc ; meta ; comment } ->
                   List.mapi (fun j { cstr ; comment ; loc} ->
                       match cstr with
                       | String code -> i + 1, j + 1, code, loc, comment
                       | _ -> raise Exit)
                     cells, loc, meta, comment)
                codes
            in
            let var = self # fresh () in
            let all_simple = ref true in
            let instrs = ref [] in
            let parsed =
              List.map
                (fun (cells, loc, meta, comment) ->
                   let cstr =
                     List.map (fun (i, j, code, sloc, comment) ->
                         let var = Printf.sprintf "%s(%d,%d)" var i j in
                         let code = var ^ "= " ^ code in
                         begin match parse sloc code with
                           | { cstr = Assign (_, { cstr = Error })} :: _ ->
                             raise Exit
                           | [ { cstr = Assign (_, exp)} ] as is ->
                             instrs := is :: !instrs ;
                             { exp with comment }
                           | { cstr = Assign (_, exp)} :: _ as is ->
                             all_simple := false ;
                             instrs := is :: !instrs ;
                             ghost Error
                           | _ -> raise Exit
                         end)
                       cells
                   in { loc ; meta ; comment ; cstr } )
                cells
            in
            if !all_simple then
              let str = Pretty.to_compact_string [ ghost (Exp (ghost (Matrix parsed))) ] in
              let meta = (loc, Replace str) :: meta in
              { cstr = Matrix parsed ; loc ; meta ; comment }
            else
              let defun = { name = ghost (var ^ "_f") ;
                            args = [] ; rets = [ ghost var ] ;
                            body = ghost (Seq (List.flatten (List.rev !instrs))) } in
              self # push (ghost (Defun defun)) ;
              ghost (Call (ghost (Var (ghost (var ^ "_f"))), [], Tuplified))
          with Exit -> (* not a parsable matrix *) exp
        end
      | Call ({ cstr = Var { cstr = "eval" }}, _, _) -> exp
      (* { exp with meta = Recovered "unparsable eval" :: exp.meta } *)
      | _ -> exp
  end in
  rewriter # ast ast

let _ = (* plug it in *)
  ScilintOptions.add_pass
    "expand-eval" pass
    "expansion of 'eval' and 'deff'"
    true
