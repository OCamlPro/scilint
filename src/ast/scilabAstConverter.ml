(*  Scilab / OCaml Toolbox - Automated conversion between AST instances
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

(** The conversion functions needed to build a converter *)
module type ConverterParameters = sig

  (** Domain *)
  module From : ScilabAst.S

  (** Domain *)
  module To : ScilabAst.S

  (** Location converter *)
  val loc : From.loc -> To.loc
  (** Symbol converter *)
  val symbol : From.symbol -> To.symbol
  (** Meta information converter *)
  val meta : From.meta -> To.meta
end

module Make
    (From : ScilabAst.S)
    (To : ScilabAst.S)
    (Parameters : ConverterParameters with module From = From
                                       and module To = To) = struct

  let rec convert_ast (ast : From.ast) : To.ast =
    List.map convert_stmt ast
  and convert_descr
    : 'a 'b. ('a -> 'b) -> 'a From.descr -> 'b To.descr
    = fun convert { From.loc ; From.meta ; From.cstr ; From.comment } ->
    let open To in
    { loc = Parameters.loc loc ;
      meta = Parameters.meta meta ;
      cstr = convert cstr ;
      comment = List.map (convert_descr (fun x -> x)) comment ;
      id = UUID.make () }

  and convert_matrix_contents rows =
    let convert_row row = List.map convert_exp row in 
    List.map (convert_descr convert_row) rows

  and convert_stmt stmt = convert_descr convert_stmt_cstr stmt

  and convert_exp exp = convert_descr convert_exp_cstr exp

  and convert_var exp = convert_descr Parameters.symbol exp

  and convert_arg (name, exp) =
    (match name with None -> None | Some n -> Some (convert_var n)),
    convert_exp exp

  and convert_stmt_cstr cstr =
    let open To in
    match cstr with
    | From.Assign (lefts, right) ->
      Assign (List.map (convert_exp) lefts, convert_exp right)
    | From.Seq stmts ->
      Seq (List.map (convert_stmt) stmts)
    | From.Defun { From.name ; From.args ; From.rets ; From.body } ->
      Defun { name = convert_var name ;
              args = List.map (convert_var) args ;
              rets = List.map (convert_var) rets ;
              body = convert_stmt body }
    | From.Exp exp ->
      Exp (convert_exp exp)
    | From.Break ->
      Break
    | From.Continue ->
      Continue
    | From.Comment text ->
      Comment text
    | From.For (it, range, body) ->
      For (convert_var it, convert_exp range, convert_stmt body)
    | From.If (cond, tbody, Some fbody)  ->
      If (convert_exp cond, convert_stmt tbody, Some (convert_stmt fbody)) 
    | From.If (cond, tbody, None)  ->
      If (convert_exp cond, convert_stmt tbody, None) 
    | From.Return  ->
      Return 
    | From.Select { From.cond ; From.cases ; From.default = None }  ->
      let cases = List.map (fun (e, s) -> convert_exp e, convert_stmt s) cases in
      Select { cond = convert_exp cond ; cases ; default = None } 
    | From.Select { From.cond ; From.cases ; From.default = Some d }  ->
      let cases = List.map (fun (e, s) -> convert_exp e, convert_stmt s) cases in
      Select { cond = convert_exp cond ; cases ; default = Some (convert_stmt d) } 
    | From.Try (tbody, cbody)  ->
      Try (convert_stmt tbody, convert_stmt cbody) 
    | From.While (cond, tbody, Some fbody)  ->
      While (convert_exp cond, convert_stmt tbody, Some (convert_stmt fbody)) 
    | From.While (cond, tbody, None)  ->
      While (convert_exp cond, convert_stmt tbody, None) 
        
  and convert_exp_cstr cstr =
    let open To in
    match cstr with
    | From.Call (name, args, kind) ->
      Call (convert_exp name, List.map (convert_arg) args, kind)
    | From.Identity args ->
      Identity (List.map (convert_exp) args)
    | From.Range (sexp, None, eexp) ->
      Range (convert_exp sexp, None, convert_exp eexp)
    | From.Range (sexp, Some stepexp, eexp) ->
      Range (convert_exp sexp, Some (convert_exp stepexp), convert_exp eexp)
    | From.Var sym ->
      Var (convert_var sym)
    | From.Matrix rows ->
      Matrix (convert_matrix_contents rows)
    | From.Cell_array rows ->
      Cell_array (convert_matrix_contents rows)
    | From.Unop (unop, exp) ->
      Unop (unop, convert_exp exp)
    | From.Op (op, lexp, rexp) ->
      Op (op, convert_exp lexp, convert_exp rexp)
    | From.Bool b -> Bool b
    | From.Num n -> Num n
    | From.String s -> String s
    | From.Colon -> Colon
    | From.Error -> Error
end
