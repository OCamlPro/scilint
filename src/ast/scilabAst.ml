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

(** Unique identifiers *)
module UUID : sig

  (** The type of unique identifiers, coerceable to int *)
  type t = int

  (** Unique identifier generator *)
  val make : unit -> t

  module Set : Ptset.T with type elt = t
  module Map : Ptmap.T with type key = t
end = struct
  type t = int
  let make =
    let cur = ref 0 in
    fun () -> incr cur ; assert (!cur <> 0) ; !cur
  module Set = Ptset
  module Map = Ptmap
end

(** Types shared between Ast instances *)
module Shared = struct

  (** This flag is used to preserve the concrete syntax of the call *)
  type call_kind =
    | Tuplified                        (** f (x, y) *)
    | Field                            (** o.f (eq. o ('f') *)
    | Shell                            (** o x y (eq. o ('x', 'y') *)
    | Cell                             (** f {x, y} (Scilab 6 / MATLAB) *)
      
  (** Unary operators *)
  and unop =
    | Unary_minus                     (** "-" *)
    | Unary_plus                      (** "+" *)
    | Not                             (** "~" *)
    | Transpose_conjugate             (** exp' *)
    | Transpose_non_conjugate         (** exp.' *)
      
  (** Binary operators *)
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

  let string_of_unop = function
    | Unary_minus -> "unary substraction"
    | Unary_plus -> "unary addition"
    | Not -> "unary negation"
    | Transpose_conjugate -> "conjugate transposition"
    | Transpose_non_conjugate -> "non conjugate transposition"
      
  let string_of_op = function
    | Plus -> "addition"
    | Minus -> "substraction"
    | Times -> "multiplication"
    | Rdivide -> "division"
    | Ldivide -> "reversed division"
    | Power -> "power"
    | Dot_times -> "scalar-matrix multiplication"
    | Dot_rdivide -> "scalar-matrix division"
    | Dot_ldivide -> "scalar-matrix reversed division"
    | Dot_power -> "scalar-matrix power"
    | Kron_times -> "konecker multiplication"
    | Kron_rdivide -> "konecker division"
    | Kron_ldivide -> "konecker reversed division"
    | Control_times -> "matrix-scalar multiplication"
    | Control_rdivide -> "matrix-scalar division"
    | Control_ldivide -> "matrix-scalar reversed division"
    | Eq -> "equality"
    | Ne -> "inequality"
    | Lt -> "< inequality"
    | Le -> "<= inequality"
    | Gt -> "> inequality"
    | Ge -> ">= inequality"
    | And -> "logical and"
    | Or -> "logical or"
    | Seq_and -> "sequential and"
    | Seq_or -> "sequential or"
end

(** The type of specialized AST modules *)
module type S = sig

  (** Export parameters *)
  include Parameters

  (** Node wrapper for location and meta-information *)
  type 'a descr = {
    mutable cstr : 'a ;                 (** The raw AST node *)
    mutable loc : loc ;      (** code location *)
    mutable meta : meta ;    (** Meta-information for the node *)
    comment : string descr list ;       (** Attached comment, if any *)
    id : UUID.t ;                       (** A unique int for identifying the node *)
  }

  (** Export UUID primitives *)
  module UUID : module type of UUID

  (** Wrap a node inside a ghost node descriptor *)
  val ghost : 'a -> 'a descr

  (** Export shared types *)

  (** This flag is used to preserve the concrete syntax of the call *)
  type call_kind = Shared.call_kind =
    | Tuplified                        (** f (x, y) *)
    | Field                            (** o.f (eq. o ('f') *)
    | Shell                            (** o x y (eq. o ('x', 'y') *)
    | Cell                             (** f {x, y} (Scilab 6 / MATLAB) *)
      
  (** Unary operators *)
  and unop = Shared.unop =
    | Unary_minus                     (** "-" *)
    | Unary_plus                      (** "+" *)
    | Not                             (** "~" *)
    | Transpose_conjugate             (** exp' *)
    | Transpose_non_conjugate         (** exp.' *)
      
  (** Binary operators *)
  and op = Shared.op =
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

  (** Human friendly unary operator name *)
  val string_of_unop : unop -> string

  (** Human friendly binary operator name *)
  val string_of_op : op -> string

  (** Main entry point: a list of statements *)
  type ast = stmt list

  (** Expression node wrapped with location and meta-information *)
  and exp = exp_cstr descr

  (** Statement node wrapped with location and meta-information *)
  and stmt = stmt_cstr descr

  (** Symbol wrapped with location and meta-information *)
  and var = symbol descr

  (** Raw statement node (<> means [,;\n] and sometimes "then") *)
  and stmt_cstr =
    (** {4 Basic statements} *)
    | Assign of exp list * exp        (** lvalue(s) = rvalue <> *)
    | Defun of defun_params           (** function ... endfunction *)
    | Exp of exp                      (** Expression as instruction (exp <>) *)
    | Comment of string               (** Oh yeah ! *)

    (** {4 Control structures} *)
    | Seq of stmt list                  (** stmt <> ... <> exp *)
    | Break                             (** break <> *)
    | Continue                          (** continue <> *)
    | For of var * exp * stmt           (** for var = exp <> stmt, end *)
    | If of exp * stmt * stmt option    (** if exp <> stmt [ else stmt ] end *)
    | Return                            (** return <> (_ = return (_) is an Assign)*)
    | Select of select_params           (** select exp <> case exp <> stmt ... end *)
    | Try of stmt * stmt                (** try ... catch ... end *)
    | While of exp * stmt * stmt option (** while exp <> stmt [ else stmt ] end *)

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
    | Call of exp * arg list * call_kind  (** f (exp, ..., exp) ({!call_kind}) *)
    | Identity of exp list                (** (exp, ..., exp) *)
    | Range of exp * exp option * exp     (** exp : exp? : exp *)

    (** {4 Simple expressions} *)
    | Bool of bool                    (** %t, %T, %f, %F *)
    | Num of float                    (** Any number (float, int) *)
    | String of string                (** "..." '...' *)
    | Var of var                      (** Symbols, including "$" *)
    | Colon                           (** Special variable ":" *)
    | Error                           (** Syntax error recovered by the parser *)

    (** {4 Math expressions} *)
    | Matrix of matrix_contents       (** Matrix litteral [ ... ] *)
    | Cell_array of matrix_contents   (** Cell litteral { ... } (Scilab 6/MATLAB) *)
    | Unop of unop * exp              (** Prefix or postfix unary op *)
    | Op of op * exp * exp            (** Infix binary op *)

  (** Optionally named function argument *)
  and arg = var option * exp

  (** Two dimensional matrix literrals _, _, _ ; _, _, _ *)
  and matrix_contents = exp list descr list
end

(** Build an AST module specialized according to its {!Parameters} *)
module Make (Parameters : Parameters)
  : S with type loc = Parameters.loc
       and type symbol = Parameters.symbol
       and type meta = Parameters.meta = struct

  (** Export parameters *)
  include Parameters

  (** Node wrapper for location and meta-information *)
  type 'a descr = {
    mutable cstr : 'a ;                 (** The raw AST node *)
    mutable loc : loc ;      (** code location *)
    mutable meta : meta ;    (** Meta-information for the node *)
    comment : string descr list ;       (** Attached comment, if any *)
    id : UUID.t ;                       (** A unique int for identifying the node *)
  }

  (** Export UUID primitives *)
  module UUID = UUID

  (** Wrap a node inside a ghost node descriptor *)
  let ghost cstr =
    { cstr ;
      comment = [] ;
      loc = ghost_loc ;
      meta = ghost_meta ;
      id = UUID.make () }

  (** Export shared types *)
  include Shared

  (** Main entry point: a list of statements *)
  type ast = stmt list

  (** Expression node wrapped with location and meta-information *)
  and exp = exp_cstr descr

  (** Statement node wrapped with location and meta-information *)
  and stmt = stmt_cstr descr

  (** Symbol wrapped with location and meta-information *)
  and var = symbol descr

  (** Raw statement node (<> means [,;\n] and sometimes "then") *)
  and stmt_cstr =
    (** {4 Basic statements} *)
    | Assign of exp list * exp        (** lvalue(s) = rvalue <> *)
    | Defun of defun_params           (** function ... endfunction *)
    | Exp of exp                      (** Expression as instruction (exp <>) *)
    | Comment of string               (** Oh yeah ! *)

    (** {4 Control structures} *)
    | Seq of stmt list                  (** stmt <> ... <> exp *)
    | Break                             (** break <> *)
    | Continue                          (** continue <> *)
    | For of var * exp * stmt           (** for var = exp <> stmt, end *)
    | If of exp * stmt * stmt option    (** if exp <> stmt [ else stmt ] end *)
    | Return                            (** return <> (_ = return (_) is an Assign)*)
    | Select of select_params           (** select exp <> case exp <> stmt ... end *)
    | Try of stmt * stmt                (** try ... catch ... end *)
    | While of exp * stmt * stmt option (** while exp <> stmt [ else stmt ] end *)

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
    | Call of exp * arg list * call_kind  (** f (exp, ..., exp) ({!call_kind}) *)
    | Identity of exp list                (** (exp, ..., exp) *)
    | Range of exp * exp option * exp     (** exp : exp? : exp *)

    (** {4 Simple expressions} *)
    | Bool of bool                    (** %t, %T, %f, %F *)
    | Num of float                    (** Any number (float, int) *)
    | String of string                (** "..." '...' *)
    | Var of var                      (** Symbols, including "$" *)
    | Colon                           (** Special variable ":" *)
    | Error                           (** Syntax error recovered by the parser *)

    (** {4 Math expressions} *)
    | Matrix of matrix_contents       (** Matrix litteral [ ... ] *)
    | Cell_array of matrix_contents   (** Cell litteral { ... } (Scilab 6/MATLAB) *)
    | Unop of unop * exp              (** Prefix or postfix unary op *)
    | Op of op * exp * exp            (** Infix binary op *)

  (** Optionally named function argument *)
  and arg = var option * exp

  (** Two dimensional matrix literrals _, _, _ ; _, _, _ *)
  and matrix_contents = exp list descr list
end
