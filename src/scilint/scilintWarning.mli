
type location = string * ScilabAst.Location.t

type local_warning =
  | Uninitialized_var of string (* W001 *)
  | Unused_arg of string (* W002 *)
  | Duplicate_arg of string (* W003 *)
  | Duplicate_return of string (* W004 *)
  | Var_arg_ret of string (* W005 *)
  | Unset_ret of string (* W006 *)
  | Return_as_var of string (* W007 *)
  | For_var_modif
  | Primitive_with_too_many_arguments of string * int
  | Overriding_primitive of string
  | Overriding_declared_function of string * location
  | Overriding_toplevel_function of string * string
  | Unexpected_string_argument of string * int * string * string list
  | Unexpected_argument_type of string * int * string (* W014 *)
  | Int_argument_out_of_range of string * int * float * int * int (* W015 *)
  | Var_def_not_used of string (* W016 *)
  | Var_redef_not_used of string (* W017 *)
  | Break_outside_loop of unit (* W018 *)
  | Continue_outside_loop of unit (* W019 *)

val local_warning : location -> local_warning -> unit

val set_format_to_xml : unit -> unit

val is_format_xml : unit -> bool
