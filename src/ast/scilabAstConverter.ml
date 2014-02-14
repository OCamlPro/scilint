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
  module From : ScilabAst.Parameters

  (** Domain *)
  module To : ScilabAst.Parameters

  (** Location converter *)
  val loc : From.loc -> To.loc
  (** Symbol converter *)
  val symbol : From.symbol -> To.symbol
  (** Meta information converter *)
  val meta : From.meta -> To.meta
end

module Make
    (From : ScilabAst.Parameters)
    (To : ScilabAst.Parameters)
    (FromAst : module type of ScilabAst.Make (From))
    (ToAst : module type of ScilabAst.Make (To))
    (Parameters : ConverterParameters with module From = From
                                       and module To = To) = struct

  let rec convert_ast (ast : FromAst.ast) : ToAst.ast =
    List.map convert_stmt ast
  and convert_descr
    : 'a 'b. ('a -> 'b) -> 'a FromAst.descr -> 'b ToAst.descr
    = fun convert { FromAst.loc ; FromAst.meta ; FromAst.cstr ; FromAst.comment } ->
    let open ToAst in
    { loc = Parameters.loc loc ;
      meta = Parameters.meta meta ;
      cstr = convert cstr ;
      comment = List.map (convert_descr (fun x -> x)) comment }

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
    let open ToAst in
    match cstr with
    | FromAst.Assign (lefts, right) ->
      Assign (List.map (convert_exp) lefts, convert_exp right)
    | FromAst.Seq stmts ->
      Seq (List.map (convert_stmt) stmts)
    | FromAst.Defun { FromAst.name ; FromAst.args ; FromAst.rets ; FromAst.body } ->
      Defun { name = convert_var name ;
              args = List.map (convert_var) args ;
              rets = List.map (convert_var) rets ;
              body = convert_stmt body }
    | FromAst.Exp exp ->
      Exp (convert_exp exp)
    | FromAst.Break ->
      Break
    | FromAst.Continue ->
      Continue
    | FromAst.Comment text ->
      Comment text
    | FromAst.For (it, range, body) ->
      For (convert_var it, convert_exp range, convert_stmt body)
    | FromAst.If (cond, tbody, Some fbody)  ->
      If (convert_exp cond, convert_stmt tbody, Some (convert_stmt fbody)) 
    | FromAst.If (cond, tbody, None)  ->
      If (convert_exp cond, convert_stmt tbody, None) 
    | FromAst.Return  ->
      Return 
    | FromAst.Select { FromAst.cond ; FromAst.cases ; FromAst.default = None }  ->
      let cases = List.map (fun (e, s) -> convert_exp e, convert_stmt s) cases in
      Select { cond = convert_exp cond ; cases ; default = None } 
    | FromAst.Select { FromAst.cond ; FromAst.cases ; FromAst.default = Some d }  ->
      let cases = List.map (fun (e, s) -> convert_exp e, convert_stmt s) cases in
      Select { cond = convert_exp cond ; cases ; default = Some (convert_stmt d) } 
    | FromAst.Try (tbody, cbody)  ->
      Try (convert_stmt tbody, convert_stmt cbody) 
    | FromAst.While (cond, tbody, Some fbody)  ->
      While (convert_exp cond, convert_stmt tbody, Some (convert_stmt fbody)) 
    | FromAst.While (cond, tbody, None)  ->
      While (convert_exp cond, convert_stmt tbody, None) 
        
  and convert_exp_cstr cstr =
    let open ToAst in
    match cstr with
    | FromAst.Call (name, args, kind) ->
      Call (convert_exp name, List.map (convert_arg) args, kind)
    | FromAst.Identity args ->
      Identity (List.map (convert_exp) args)
    | FromAst.Range (sexp, None, eexp) ->
      Range (convert_exp sexp, None, convert_exp eexp)
    | FromAst.Range (sexp, Some stepexp, eexp) ->
      Range (convert_exp sexp, Some (convert_exp stepexp), convert_exp eexp)
    | FromAst.Var sym ->
      Var (convert_var sym)
    | FromAst.Matrix rows ->
      Matrix (convert_matrix_contents rows)
    | FromAst.Cell_array rows ->
      Cell_array (convert_matrix_contents rows)
    | FromAst.Unop (unop, exp) ->
      Unop (unop, convert_exp exp)
    | FromAst.Op (op, lexp, rexp) ->
      Op (op, convert_exp lexp, convert_exp rexp)
    | FromAst.Bool b -> Bool b
    | FromAst.Num n -> Num n
    | FromAst.String s -> String s
    | FromAst.Colon -> Colon
    | FromAst.Error -> Error
end
