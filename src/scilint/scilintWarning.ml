open ScilabAst

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

let print_warning code locs =
  List.iteri (fun i ((file, loc), msg) ->
    Printf.printf "File \"%s\", line %i, characters %i-%i:\n"
      file loc.first_line loc.first_column loc.last_column;
    if i = 0 then Printf.printf "Warning W%03d: " code;
    Printf.printf "%s\n" msg
  ) locs

let local_warning loc w =
  let (code, msg) =
    match w with
    | Uninitialized_var s -> 1,
      [ loc, "\"" ^ s ^ "\" not initialized" ]
    | Unused_arg s -> 1,
      [ loc, "\"" ^ s ^ "\" not used" ]
    | Duplicate_arg s -> 3,
      [ loc, "argument \"" ^ s ^ "\" appears several times" ]
    | Duplicate_return s -> 4,
      [ loc, "return variable \"" ^ s ^ "\" appears several times" ]
    | Var_arg_ret s -> 5,
      [ loc, "return variable \"" ^ s ^ "\" is also an argument" ]
    | Unset_ret s -> 6,
      [ loc, "return variable \"" ^ s ^ "\" is never set" ]
    | Return_as_var s -> 7,
      [ loc, "return variable \"" ^ s ^ "\" is used as a local variable" ]

    | Overriding_primitive fun_name -> 996,
      [ loc, Printf.sprintf "overriding primitive %S" fun_name ]

    | Overriding_declared_function (fun_name, fun_loc) -> 997,
      [ loc, Printf.sprintf "overriding function %S" fun_name;
        fun_loc, "Already declared here"
      ]
    | Overriding_toplevel_function (fun_name, file) -> 998,
      [ loc, Printf.sprintf "overriding toplevel function %S of file %S" fun_name file ]

    | Unexpected_string_argument (fun_name, i, s, possible) -> 999,
      [ loc,
        Printf.sprintf "Function %S does not expect %S as argument %d,\nShould be one of: %s"
          fun_name s (i+1) (String.concat ", " possible)
      ]
  in
  print_warning code msg


