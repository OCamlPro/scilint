(*  Scilab / OCaml Toolbox - AST for Scilab 5's syntax
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

(** AST parameters to tweak it for different uses *)
module type Parameters = sig

  (** A type for locations (depending on the source) *)
  type loc

  (** A type for symbols (probably string) *)
  type symbol

  (** The type of meta-data attached to AST nodes *)
  type meta

  (** A predefined dummy location value *)
  val ghost_loc : loc

  (** A predefined dummy meta value *)
  val ghost_meta : meta

end

(** Build an AST module specialized according to its {!Parameters} *)
module Make (Parameters : Parameters) = struct

  (** Node wrapper for location and meta-information *)
  type 'a descr = {
    cstr : 'a ;                 (** The raw AST node *)
    loc : Parameters.loc ; (** code location *)
    meta : Parameters.meta ;    (** Meta-information for the node *)
    comment : string descr list ;   (** Attached comment, if any *)
  }

  (** Wrap a node inside a ghost node descriptor *)
  let ghost cstr =
    { cstr ;
      comment = [] ;
      loc = Parameters.ghost_loc ;
      meta = Parameters.ghost_meta }

  (** Main entry point: a list of statements *)
  type ast = stmt list

  (** Expression node wrapped with location and meta-information *)
  and exp = exp_cstr descr

  (** Statement node wrapped with location and meta-information *)
  and stmt = stmt_cstr descr

  (** Symbol wrapped with location and meta-information *)
  and var = Parameters.symbol descr

  (** Raw statement node (<> means [,;\n] and sometimes "then") *)
  and stmt_cstr =
    (** {4 Basic statements} *)
    | Assign of exp list * exp        (** lvalue(s) = rvalue <> *)
    | Defun of defun_params           (** function ... endfunction *)
    | Exp of exp                      (** Expression as instruction (exp <>) *)
    | Comment of string               (** Oh yeah ! *)

    (** {4 Control structures} *)
    | Seq of stmt list                (** stmt <> ... <> exp *)
    | Break                           (** break <> *)
    | Continue                        (** continue <> *)
    | For of var * exp * stmt         (** for var = exp <> stmt, end *)
    | If of exp * stmt * stmt option  (** if exp <> stmt [ else stmt ] end *)
    | Return                          (** return <> (_ = return (_) is an Assign)*)
    | Select of select_params         (** select exp <> case exp <> stmt ... end *)
    | Try of stmt * stmt              (** try ... catch ... end *)
    | While of exp * stmt             (** while exp <> stmt end *)

  (** Function definition parameters *)
  and defun_params =  {
    name : var ;
    args : var list ;
    rets : var list ;
    body : stmt ;
  }

  (** Select parameters *)
  and select_params = {
    cond : exp ;
    cases : (exp * stmt) list ;
    default : stmt option ;
  }

  (** Raw expression node *)
  and exp_cstr =
    (** {4 Composite expressions} *)
    | Call of exp * arg list * call_kind  (** f (exp, ..., exp) (see {!call_kind}) *)
    | Identity of exp list                (** (exp, ..., exp) (* call to identity *) *)
    | Range of exp * exp option * exp     (** exp : exp? : exp *)

    (** {4 Simple expressions} *)
    | Bool of bool                    (** %t, %T, %f, %F *)
    | Num of float                    (** Any number (float, int) *)
    | String of string                (** "..." '...' *)
    | Var of var                      (** Symbols, including "$" *)
    | Colon                           (** Special variable ":" *)
    | Error of string                 (** Syntax error recovered by the parser *)

    (** {4 Math expressions} *)
    | Matrix of matrix_contents       (** Matrix litteral [ ... ] *)
    | Cell_array of matrix_contents   (** Cell litteral { ... } (Scilab 6 / MATLAB) *)
    | Unop of unop * exp              (** Prefix or postfix unary op *)
    | Op of op * exp * exp            (** Infix binary op *)

  (** This flag preserves the concrete syntax of the call *)
  and call_kind =
    | Tuplified                        (** f (x, y) *)
    | Field                            (** o.f (eq. o ('f') *)
    | Shell                            (** o x y (eq. o ('x', 'y') *)
    | Cell                             (** f {x, y} (Scilab 6 / MATLAB) *)

  (** Optionally named function argument *)
  and arg = var option * exp

  (** Two dimensional matrix literrals _, _, _ ; _, _, _ *)
  and matrix_contents = exp list descr list

  (** Unary operators *)
  and unop =
    | Unary_minus                     (** "-" *)
    | Unary_plus                      (** "+" *)
    | Not                             (** "~" *)
    | Transpose_conjugate             (** exp' *)
    | Transpose_non_conjugate         (** exp.' *)

  (** Operators *)
  and op =
    (** {4 Arithmetics} *)
    | Plus                            (** "+" *)
    | Minus                           (** "-" *)
    | Times                           (** "*" *)
    | Rdivide                         (** "/" *)
    | Ldivide                         (** \   *)
    | Power                           (** "**" or "^" *)

    (** {4 Element-wise operations} *)
    | Dot_times                        (** ".*" *)
    | Dot_rdivide                      (** "./" *)
    | Dot_ldivide                      (** .\   *)
    | Dot_power                        (** ".^" *)

    (** {4 Kroneckers} *)
    | Kron_times                       (** ".*." *)
    | Kron_rdivide                     (** "./." *)
    | Kron_ldivide                     (** ".\." *)

    (** {4 Control} *)
    | Control_times                    (** "*." *)
    | Control_rdivide                  (** "/." *)
    | Control_ldivide                  (** "\." *)

    (** {4 Comparison} *)
    | Eq                              (** "==" *)
    | Ne                              (** "<>" or "~=" *)
    | Lt                              (** "<" *)
    | Le                              (** "<=" *)
    | Gt                              (** "<" *)
    | Ge                              (** ">=" *)

    (** {4 Logical operators} *)
    | And                             (** "&" *)
    | Or                              (** "|" *)
    | Seq_and                         (** "&&" *)
    | Seq_or                          (** "||" *)

end
