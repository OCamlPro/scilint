
type local_warning =
  | Uninitialized_var of string (* W001 *)
  | Unused_arg of string (* W002 *)
  | Duplicate_arg of string (* W003 *)
  | Duplicate_return of string (* W004 *)
  | Var_arg_ret of string (* W005 *)
  | Unset_ret of string (* W006 *)
  | Return_as_var of string (* W007 *)

type location = string * ScilabAst.Location.t

let print_warning (code, msg) (file,loc) =
  ScilabUtils.print_warning (Printf.sprintf "Warning: %s.  (%s)\n" msg code)
    file loc

let local_warning loc w =
  let (code, msg) =
    match w with
    | Uninitialized_var s -> ("W001",
        "\"" ^ s ^ "\" not initialized")
    | Unused_arg s -> ("W002",
        "\"" ^ s ^ "\" not used")
    | Duplicate_arg s -> ("W003",
        "argument \"" ^ s ^ "\" appears several times")
    | Duplicate_return s -> ("W004",
        "return variable \"" ^ s ^ "\" appears several times")
    | Var_arg_ret s -> ("W005",
        "return variable \"" ^ s ^ "\" is also an argument")
    | Unset_ret s -> ("W006",
        "return variable \"" ^ s ^ "\" is never set")
    | Return_as_var s -> ("W007",
        "return variable \"" ^ s ^ "\" is used as a local variable")
  in
  print_warning (code, msg) loc


