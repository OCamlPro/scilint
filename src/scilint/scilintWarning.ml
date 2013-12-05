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
  | For_var_modif (* W008 *)
  | Primitive_with_too_many_arguments of string * int (* W009 *)
  | Overriding_primitive of string (* W010 *)
  | Overriding_declared_function of string * location (* W011 *)
  | Overriding_toplevel_function of string * string (* W012 *)
  | Unexpected_string_argument of string * int * string * string list (* W013 *)
  | Unexpected_argument_type of string * int * string (* W014 *)
  | Int_argument_out_of_range of string * int * float * int * int (* W015 *)

type output_format = TextFormat | XmlFormat

let output_format = ref TextFormat

let is_format_xml () = !output_format = XmlFormat

let set_format_to_xml () = output_format := XmlFormat

let print_warning_in_text code locs =
  List.iteri (fun i ((file, loc), msg) ->
    if loc.first_line = loc.last_line
    then
      begin
        Printf.printf "File \"%s\", line %i, characters %i-%i:\n"
          file loc.first_line loc.first_column loc.last_column;
        if i = 0 then Printf.printf "Warning W%03d: " code;
        Printf.printf "%s\n" msg
      end
    else
      begin
        Printf.printf "File \"%s\", line %i-%i, characters %i-%i:\n"
          file loc.first_line loc.last_line loc.first_column loc.last_column;
        if i = 0 then Printf.printf "Warning W%03d: " code;
        Printf.printf "%s\n" msg
      end
  ) locs

let print_warning code locs = match !output_format with
  | TextFormat -> print_warning_in_text code locs
  | XmlFormat -> let str = ScilintFirehosegen.warning_to_firehose code locs in
                 Printf.printf "%s%!" str

let local_warning loc w =
  let (code, msg) =
    match w with
    | Uninitialized_var s -> 1,
      [ loc, "\"" ^ s ^ "\" not initialized" ]
    | Unused_arg s -> 2,
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
    | For_var_modif -> 8,
      [ loc,
        Printf.sprintf "modifying variable of 'for' loop does not change loop behavior"
      ]
    | Primitive_with_too_many_arguments (fun_name, i) -> 9,
      [ loc,
        Printf.sprintf "primitive %S called with too many arguments (>= %d)"
          fun_name i ]
    | Overriding_primitive fun_name -> 10,
      [ loc, Printf.sprintf "overriding primitive %S" fun_name ]

    | Overriding_declared_function (fun_name, fun_loc) -> 11,
      [ loc, Printf.sprintf "overriding function %S" fun_name;
        fun_loc, "Already declared here"
      ]
    | Overriding_toplevel_function (fun_name, file) -> 12,
      [ loc, Printf.sprintf "overriding toplevel function %S of file %S" fun_name file ]

    | Unexpected_string_argument (fun_name, i, s, possible) -> 13,
      [ loc,
        Printf.sprintf "Function %S does not expect %S as argument %d,\nShould be one of: %s"
          fun_name s (i+1) (String.concat ", " possible)
      ]
    | Unexpected_argument_type (fun_name, i, expected_type) -> 14,
      [ loc,
        Printf.sprintf "Function %S expects type %s as argument %d\n"
          fun_name expected_type (i+1)
      ]
    | Int_argument_out_of_range (fun_name, i, v, min, max) -> 15,
      [ loc,
        Printf.sprintf "Function %S does not expect %.1f as argument %d\nShould be between %d and %d"
          fun_name v (i+1) min max
      ]

  in
  print_warning code msg


