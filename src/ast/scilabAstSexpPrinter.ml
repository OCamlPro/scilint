(*  OCamlPro Scilab Toolbox - AST to S-expressions conversion and printing
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

(** An intermediate representation for S-expressions *)
type sexp =
  | B of sexp list (** A list of S-exps to be put inside parentheses *)
  | L of string (** A single litteral *)
  | C of sexp option * string (** A comment *)

(** Obtain a PPrint document from an S-expr intermediate representation *)
let rec document_of_sexp (s : sexp) =
  let open PPrint in
  match s with 
  | B ss ->
    let rec sep acc = function
      | [] -> group acc
      | C _ as e :: ss -> sep ((group acc ^^ document_of_sexp e) ^^ hardline) ss
      | e :: [] -> group (acc ^^ document_of_sexp e)
      | e :: ss -> sep (group (acc ^^ document_of_sexp e) ^^ break 1) ss
    in
    group (string "(" ^^ nest 2 (sep empty ss) ^^ string ")")
  | L text -> string text
  | C (s, doc) ->
    let words = words doc in
      let br = ifflat (string " ") (hardline ^^ string ";; ") in
    (match s with None -> empty | Some d -> document_of_sexp d ^^ string " ")
    ^^ align (string ";; " ^^ flow br words)
      
(** Output an S-expr to a channel using PPrint for great beauty. *)
let pretty_output ?(width = 80) (fp : out_channel) (s : sexp) =
  PPrint.ToChannel.pretty 0.9 width fp (document_of_sexp s)
    
(** Output an S-expr to a channel using PPrint for great beauty. *)
let compact_output (fp : out_channel) (s : sexp) =
  PPrint.ToChannel.compact fp (document_of_sexp s)  

open ScilabAst

(** Parameters for the printer: AST type parameters and their
    projections to {!sexp} *)
module type PrinterParameters = sig
  module Parameters : Parameters
  val sexp_of_loc : Parameters.loc -> sexp -> sexp
  val sexp_of_meta : Parameters.meta -> sexp -> sexp
  val sexp_of_symbol : Parameters.symbol -> sexp
end

(** Instanciate this functor with the same {!Parameters} as the AST to
    obtain an S-expr style printer ([output_ast file ast]). *)
module Make
    (Parameters : Parameters)
    (PrinterParameters : PrinterParameters with module Parameters := Parameters)
    (Ast : module type of ScilabAst.Make (Parameters)) = struct
  open Ast

  (** Convert an AST to its S-expr intermediate representation *)
  let rec sexp_of_ast (a : Ast.ast) =
    B (L "script" :: List.map sexp_of_stmt a)

  and sexp_of_exp (w : Ast.exp) =
    sexp_of_descr w (sexp_of_exp_cstr w.cstr)

  and sexp_of_exp_cstr (e : Ast.exp_cstr) =
    match e with
    | Call (name, args, Tuplified) ->
      B (L "call" :: sexp_of_exp name :: B (List.map sexp_of_arg args) :: [])
    | Call (name, args, Cell) ->
      B (L "cell-call" :: sexp_of_exp name :: B (List.map sexp_of_arg args) :: [])
    | Call (name, args, Shell) ->
      B (L "shell-call" :: sexp_of_exp name :: B (List.map sexp_of_arg args) :: [])
    | Call (lexp, [ None, rexp ], Field) ->
      B [ L "field" ; sexp_of_exp lexp ; sexp_of_exp rexp ]
    | Call (lexp, _, Field) ->
      assert false
    | Identity args ->
      B (L "id" :: List.map sexp_of_exp args)
    | Range (sexp, None, eexp) ->
      B [ L "range" ; sexp_of_exp sexp ; sexp_of_exp eexp ]
    | Range (sexp, Some stepexp, eexp) ->
      B [ L "range" ; sexp_of_exp sexp ; sexp_of_exp stepexp ; sexp_of_exp eexp ]
    | Bool true ->
      L "true"
    | Bool false ->
      L "false"
    | Num f ->
      L (Printf.sprintf "%g" f)
    | String str ->
      L (Printf.sprintf "%S" str)
    | Var sym ->
      sexp_of_var sym
    | Colon ->
      L "colon"
    | Matrix rows ->
      B (L "matrix" :: sexps_of_matrix_ctns rows)
    | Cell_array rows ->
      B (L "cell-array" :: sexps_of_matrix_ctns rows)
    | Unop (unop, exp) ->
      B [ sexp_of_unop unop ; sexp_of_exp exp ]
    | Op (op, lexp, rexp) ->
      B [ sexp_of_op op ; sexp_of_exp lexp ; sexp_of_exp rexp ]      
    | Error ->
      B [ L "error" ]

  and sexp_of_unop unop =
    match unop with
    | Unary_plus -> L "+"
    | Unary_minus -> L "-"
    | Transpose_conjugate -> L "'"
    | Transpose_non_conjugate -> L ".'"
    | Not -> L "~"

  and sexp_of_op op =
    match op with
    | Plus -> L "+"
    | Minus -> L "-"
    | Times -> L "*"
    | Rdivide -> L "/"
    | Ldivide -> L "\\"
    | Power -> L "**"
    | Dot_times-> L ".*"
    | Dot_rdivide -> L "./"
    | Dot_ldivide -> L ".\\"
    | Dot_power -> L ".^"
    | Kron_times -> L ".*."
    | Kron_rdivide -> L "./."
    | Kron_ldivide -> L ".\\."
    | Control_times -> L "*."
    | Control_rdivide -> L "/."
    | Control_ldivide -> L "\\."
    | Eq -> L "=="
    | Ne -> L "<>"
    | Lt -> L "<"
    | Le -> L "<="
    | Gt -> L "<"
    | Ge -> L ">="
    | And -> L "&"
    | Or -> L "|"
    | Seq_and -> L "&&"
    | Seq_or -> L "||"

  and sexps_of_matrix_ctns rows =
    List.map (fun { cstr = cells } -> B (L "row" :: List.map sexp_of_exp cells)) rows
      
  and sexp_of_arg (name, exp) =
    match name with
    | None -> sexp_of_exp exp
    | Some n -> B [ L "named" ; sexp_of_var n ; sexp_of_exp exp ]

  and sexp_of_stmt (w : Ast.stmt) =
    sexp_of_descr w (sexp_of_stmt_cstr w.cstr)

  and sexp_of_stmt_cstr (st : Ast.stmt_cstr) =
    match st with
    | Assign ([left], right) ->
      B [ L "assign" ; sexp_of_exp left ; sexp_of_exp right ]
    | Assign (lefts, right) ->
      B [ L "assign" ; B (List.map sexp_of_exp lefts) ; sexp_of_exp right ]
    | Seq stmts ->
      B (L "seq" :: List.map sexp_of_stmt stmts)
    | Defun { name ; args ; rets ; body } ->
      B [ L "defun" ;
          sexp_of_var name ;
          B (List.map sexp_of_var args) ;
          B (List.map sexp_of_var rets) ;
          sexp_of_stmt body ]
    | Exp exp ->
      sexp_of_exp exp
    | Break ->
      B [ L "break" ]
    | Continue ->
      B [ L "continue" ]
    | Comment text ->
      C (None, ctrim text)
    | For (it, range, body) ->
      B [ L "for" ; sexp_of_var it ; sexp_of_exp range ; sexp_of_stmt body ]
    | If (cond, tbody, Some fbody)  ->
      B [ L "if" ; sexp_of_exp cond ; sexp_of_stmt tbody ; sexp_of_stmt fbody ]
    | If (cond, tbody, None)  ->
      B [ L "if" ; sexp_of_exp cond ; sexp_of_stmt tbody ]
    | Return  ->
      B [ L "return" ]
    | Select { cond ; cases ; default = None }  ->
      B (L "select" :: sexp_of_exp cond
         :: List.map sexp_of_case cases)
    | Select { cond ; cases ; default = Some d }  ->
      B (L "select" :: sexp_of_exp cond
         :: List.map sexp_of_case cases
         @ [ B [ L "default" ; sexp_of_stmt d ] ])
    | Try (tbody, cbody)  ->
      B [ L "try" ; sexp_of_stmt tbody ; sexp_of_stmt cbody ]
    | While (cond, tbody, Some fbody)  ->
      B [ L "while" ; sexp_of_exp cond ; sexp_of_stmt tbody ; sexp_of_stmt fbody ]
    | While (cond, tbody, None)  ->
      B [ L "while" ; sexp_of_exp cond ; sexp_of_stmt tbody ]

  and sexp_of_case (exp, stmt) =
    B [ L "case" ; sexp_of_exp exp ; sexp_of_stmt stmt ]

  and sexp_of_var (s : Ast.var) =
    PrinterParameters.sexp_of_symbol s.cstr

  and sexp_of_descr : 'a. 'a descr -> sexp -> sexp
    = fun { comment ; loc ; meta } sexp ->
      List.fold_left
        (fun r { cstr } -> C (Some sexp, ctrim cstr))
        sexp comment
      |> PrinterParameters.sexp_of_loc loc
      |> PrinterParameters.sexp_of_meta meta

  and ctrim str =
    (* remove slashes from comments *)
    let len = String.length str in
    let rec left i =
      if i = len then right i ( pred len)
      else match str.[i] with
        | ' ' | '\t' | '/' -> left (succ i)
        | _ -> right i (pred len)
    and right l i =
      if i <= l then
        String.sub str l (max 0 (i - l))
      else match str.[i] with
        | ' ' | '\t' | '/' -> right l (pred i)
        | _ -> String.sub str l (i - l + 1)
    in
    left 0

  (** Output to a channel using PPrint for great beauty. *)
  let pretty_output ?(width = 80) (fp : out_channel) ast =
    pretty_output ~width fp (sexp_of_ast ast)

  (** Output to a channel using PPrint for great beauty. *)
  let compact_output (fp : out_channel) ast =
    compact_output fp (sexp_of_ast ast)
end
