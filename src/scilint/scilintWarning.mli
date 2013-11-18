
type location = string * ScilabAst.Location.t

type local_warning =
  | Uninitialized_var of string (* W001 *)
  | Unused_arg of string (* W002 *)
  | Duplicate_arg of string (* W003 *)
  | Duplicate_return of string (* W004 *)
  | Var_arg_ret of string (* W005 *)
  | Unset_ret of string (* W006 *)
  | Return_as_var of string (* W007 *)
  | Overriding_primitive of string
  | Overriding_declared_function of string * location
  | Overriding_toplevel_function of string * string
  | Unexpected_string_argument of string * int * string * string list

val local_warning : location -> local_warning -> unit

