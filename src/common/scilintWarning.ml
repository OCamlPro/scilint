(*  OCamlPro Scilab Toolbox - Common types and code for displaying warning
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU, Michael LAPORTE
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabLocations

(** A located message *)
type message = loc * message_contents

(** Raw contents of a message *)
and message_contents =
  | Insert of point * string (** (where, what) *)
  | Drop of bounds
  | Replace of (point * point) * string (** (where, by what) *)
  | Warning of warning
  | Recovered of string

(** Various kinds of warnings *)
and warning =
  | L of local_warning
  | S of style_warning
  | W of string * string (** generic warning (kind, message) *)

(** Intra-procedural warnings *)
and local_warning =
  | Uninitialized_var of string (* W001 *)
  | Unused_arg of string (* W002 *)
  | Duplicate_arg of string (* W003 *)
  | Duplicate_return of string (* W004 *)
  | Var_arg_ret of string (* W005 *)
  | Unset_ret of string (* W006 *)
  | Return_as_var of string (* W007 *)
  | For_var_modif (* W008 *)
  | Primitive_with_too_many_arguments of string * int (* W009 *)
  | Overriding_primitive of string (* W010 *)
  | Overriding_declared_function of string * loc (* W011 *)
  | Overriding_toplevel_function of string * string (* W012 *)
  | Unexpected_string_argument of string * int * string * string list (* W013 *)
  | Unexpected_argument_type of string * int * string (* W014 *)
  | Int_argument_out_of_range of string * int * float * int * int (* W015 *)
  | Var_def_not_used of string (* W016 *)
  | Var_redef_not_used of string (* 017 *)
  | Break_outside_loop (* W018 *)
  | Continue_outside_loop (* W019 *)

(** Textual mistakes *)
and style_warning =
  | Inconsistent_string_delimiters
  | Inconsistent_matrix_delimiters
  | Inconsistent_matrix_separators
  | Spaces_in_operator
  | Spaces_around_dot
  | Missing_function_parameters
  | Missing_catch
  | Keyword_as_var
  | Keyword_as_shell_arg
  | Deprecated of string
  | Ambiguous_dot_left_of_oper
  | Ambiguous_dot_right_of_oper

(** Builds a displayable version of a source *)
let rec string_of_source ?(cap = true) source =
  let open Printf in
  match source with
  | Forged ->
    if cap then "ghost code" else "Ghost code"
  | String (name, _) ->
    sprintf (if cap then "String %S" else "string %S") name
  | File (name) ->
    sprintf (if cap then "File %S" else "file %S") name
  | Eval_string loc ->
    string_of_loc loc
    ^ if cap then "Eval string" else "eval string"

(** Builds a displayable version of a location *)
and string_of_loc ?(cap = true) (source, ((ls, cs), (le, ce))) =
  let open Printf in
  if ls = le then
    if cs = ce then
      sprintf "%s, line %i, character %i:\n"
        (string_of_source ~cap source) ls cs
    else
      sprintf "%s, line %i, characters %i-%i:\n"
        (string_of_source ~cap source) ls cs ce
  else
    sprintf "%s, lines %i-%i, characters %i-%i:\n"
      (string_of_source ~cap source) ls le cs ce

(** Builds a human readable version of a message *)
let rec string_of_message ((src, bounds), msg) =
  let open Printf in
  match msg with
  | Warning (L descr) ->
    let num = num_of_local_warning descr in
    let msg = string_of_local_warning descr in
    sprintf "%sWarning L%03i: %s\n"
      (string_of_loc (src, bounds)) num msg
  | Warning (S descr) ->
    let num = num_of_style_warning descr in
    let msg = string_of_style_warning descr in
    sprintf "%sWarning S%03i: %s\n"
      (string_of_loc (src, bounds)) num msg
  | Warning (W (name, msg)) ->
    sprintf "%sWarning %s: %s\n"
      (string_of_loc (src, bounds)) name msg
  | Recovered msg ->
    sprintf "%sError: %s\n"
      (string_of_loc (src, bounds)) msg
  | Insert (point, kwd) ->
    sprintf "%sInsert %S\n"
      (string_of_loc (src, (point, point))) kwd
  | Drop bounds ->
    sprintf "%sDrop\n"
      (string_of_loc (src, bounds))
  | Replace (bounds, rep) ->
    sprintf "%sReplace by %S\n"
      (string_of_loc (src, bounds)) rep

(** Build a message for a given local warning *)
and string_of_local_warning descr =
  let open Printf in
  match descr with
  | Uninitialized_var s -> "\"" ^ s ^ "\" not initialized"
  | Unused_arg s -> "\"" ^ s ^ "\" not used"
  | Duplicate_arg s -> "argument \"" ^ s ^ "\" appears several times"
  | Duplicate_return s -> "return variable \"" ^ s ^ "\" appears several times"
  | Var_arg_ret s -> "return variable \"" ^ s ^ "\" is also an argument"
  | Unset_ret s -> "return variable \"" ^ s ^ "\" is never set"
  | Return_as_var s -> "return variable \"" ^ s ^ "\" is used as a local variable"
  | For_var_modif ->
    "modifying variable of for loop does not change loop behavior"
  | Primitive_with_too_many_arguments (fun_name, i) ->
    sprintf "primitive %S called with too many arguments (>= %d)" fun_name i
  | Overriding_primitive fun_name ->
    sprintf "overriding primitive %S" fun_name
  | Overriding_declared_function (fun_name, fun_loc) ->
    sprintf "overriding function %S already declared at %s"
      fun_name (string_of_loc ~cap:false fun_loc)
  | Overriding_toplevel_function (fun_name, file) ->
    sprintf "overriding toplevel function %S of file %S" fun_name file
  | Unexpected_string_argument (fun_name, i, s, possible) ->
    sprintf "Function %S does not expect %S as argument %d. \
             Should be one of: %s"
      fun_name s (succ i) (String.concat ", " possible)
  | Unexpected_argument_type (fun_name, i, expected_type) ->
    sprintf "Function %S expects type %s as argument %d\n"
      fun_name expected_type (succ i)
  | Int_argument_out_of_range (fun_name, i, v, min, max) ->
    sprintf "Function %S does not expect %.1f as argument %d. \
             Should be between %d and %d"
      fun_name v (succ i) min max
  | Var_def_not_used var_name ->
    sprintf "variable %S defined but not used" var_name
  | Var_redef_not_used var_name ->
    sprintf "variable %S redefined before being used" var_name
  | Break_outside_loop -> sprintf "break outside of loop"
  | Continue_outside_loop -> sprintf "continue outside of loop"

and num_of_local_warning descr =
  match descr with
  | Uninitialized_var s -> 001
  | Unused_arg s -> 002
  | Duplicate_arg s -> 003
  | Duplicate_return s -> 004
  | Var_arg_ret s -> 005
  | Unset_ret s -> 006
  | Return_as_var s -> 007
  | For_var_modif -> 008
  | Primitive_with_too_many_arguments (fun_name, i) -> 009
  | Overriding_primitive fun_name -> 010
  | Overriding_declared_function (fun_name, fun_loc) -> 011
  | Overriding_toplevel_function (fun_name, file) -> 012
  | Unexpected_string_argument (fun_name, i, s, possible) -> 013
  | Unexpected_argument_type (fun_name, i, expected_type) -> 014
  | Int_argument_out_of_range (fun_name, i, v, min, max) -> 015
  | Var_def_not_used var_name -> 016
  | Var_redef_not_used var_name -> 017
  | Break_outside_loop -> 018
  | Continue_outside_loop -> 019

and num_of_style_warning descr =
  match descr with
  | Inconsistent_string_delimiters -> 001
  | Inconsistent_matrix_delimiters -> 002
  | Inconsistent_matrix_separators -> 003
  | Spaces_in_operator -> 004
  | Spaces_around_dot -> 005
  | Missing_function_parameters -> 006
  | Missing_catch -> 007
  | Keyword_as_var -> 008
  | Keyword_as_shell_arg -> 009
  | Deprecated arg -> 010
  | Ambiguous_dot_left_of_oper -> 011
  | Ambiguous_dot_right_of_oper -> 012

and string_of_style_warning descr =
  let open Printf in
  match descr with
  | Inconsistent_string_delimiters -> "inconsistent string delimiters"
  | Inconsistent_matrix_delimiters -> "inconsistent matrix delimiters"
  | Inconsistent_matrix_separators -> "inconsistent matrix separators"
  | Spaces_in_operator -> "spaces in operator"
  | Spaces_around_dot -> "spaces around dot"
  | Missing_function_parameters -> "missing function parameters"
  | Missing_catch -> "missing catch in try block"
  | Keyword_as_var -> "keyword should not be used as variable names"
  | Keyword_as_shell_arg -> "keywords in shell args should be quoted"
  | Deprecated arg -> "deprecated " ^ arg
  | Ambiguous_dot_left_of_oper -> "ambiguous decimal dot before operator"
  | Ambiguous_dot_right_of_oper -> "ambiguous decimal dot after operator"
