(*  OCamlPro Scilab Toolbox - Common types and code for displaying warning
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU, Michael LAPORTE
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabLocations

(** {2 Types} *)

(** A located message *)
type message = loc * message_contents

(** Raw contents of a message *)
and message_contents =
  | Warning of warning
  | Recovered of string
  | Drop
  | Insert of string
  | Replace of string

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
  | Function_name_not_started_by_lowercase
  | Function_name_contains_digits
  | Inconsistent_string_delimiters
  | Inconsistent_matrix_delimiters
  | Inconsistent_matrix_separators
  | Spaces_in_operator
  | Spaces_around_dot
  | Missing_function_parameters
  | Missing_catch
  | Misused_keyword
  | Keyword_as_shell_arg
  | Deprecated of string
  | Ambiguous_dot_left_of_oper
  | Ambiguous_dot_right_of_oper
  | Ambiguous_toplevel_expression

(** {2 Display} *)

(** Builds a displayable version of a location *)
val string_of_loc : loc -> string

(** Builds a displayable version of a source *)
val string_of_source : source -> string

(** Supported output formats *)
type format = Emacs | Firehose | Human

(** Formats a list of messages in a string *)
val string_of_messages : format -> message list -> string

(** Formats a list of messages in an output channel *)
val output_messages : format -> message list -> out_channel -> unit
