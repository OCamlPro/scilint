(*  OCamlPro Scilab Toolbox - AST instance for the parser output
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

(** Instance of AST parameters *)
module Parameters = struct

  (** Locations as given from the parser *)
  type loc = source * (point * point) (** source,  (start, stop) *)
  and point = int * int  (** (line >= 1, column >= 0) *)

  and source =
    | String of string
    | File of string
    | Forged

  (** Location builder *)
  let loc source fl fc ll lc =
    (source, ((fl, fc), (ll, lc)))

  (** Location with every field at -1 *)
  let ghost_loc = loc Forged (-1) (-1) (-1) (-1)

  (** Symbols are just strings *)
  type symbol = string

  (** Warnings emitted by the parser stored as meta-info *)
  type meta = message list

  and message =
    | Insert of point * string * string
    | Drop of (point * point) * string * string
    | Warning of string

  (** Dummy meta *)
  let ghost_meta = []

  (** Forge a location that contains all the others. *)
  let rec merge_locs locs =
    match locs with
    | [] -> ghost_loc
    | [ loc ] -> loc
    | loc :: locs ->
      List.fold_left
	(fun
          (srcr, ((flr, fcr), (llr, lcr)))
          (srce, ((fle, fce), (lle, lce))) ->
          ((if srcr = srce then srcr else Forged),
           ((if fle < flr then fle, fce
             else if fle = flr then fle, min fce fcr
             else flr, fcr),
            (if lle > llr then lle, lce
             else if lle = llr then lle, max lce lcr
             else llr, lcr))))
        loc locs
end

(** Instance of AST parameters for use with the printer *)
module PrinterParameters = struct
  include Parameters
  open ScilabFiveAstSexpPrinter
  let sexp_of_loc _ s = s
  let sexp_of_meta _ s = s
  let sexp_of_symbol s = L (Printf.sprintf "!%s" s)
  open PPrint
  let document_of_meta _ s = s
  let document_of_symbol s = string s
end

module Ast = ScilabFiveAst.Make (Parameters)
module Utils = ScilabFiveAstUtils.Make (Parameters) (Ast)

(** Export ALL the types *)
include Parameters

(** Export the pre-instanciated AST *)
include Ast

(** Forge a location that contains all the expressions. *)
let merge_descr_locs exprs =
  merge_locs (List.map (fun expr -> expr.loc) exprs)

(** Export utilities *)
include Utils

(* printers *)
module Sexp = ScilabFiveAstSexpPrinter.Make (Parameters) (PrinterParameters) (Ast)
module Pretty = ScilabFiveAstPrettyPrinter.Make (Parameters) (PrinterParameters) (Ast)
