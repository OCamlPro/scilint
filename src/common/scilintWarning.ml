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

(** {2 Display} *)

let rec format_source ppf ?(cap = true) source =
  let open Printf in
  match source with
  | Forged ->
    Format.fprintf ppf (if cap then "ghost code" else "Ghost code")
  | String (name, _) ->
    Format.fprintf ppf (if cap then "String %S" else "string %S") name
  | File (name) ->
    Format.fprintf ppf (if cap then "File %S" else "file %S") name
  | Eval_string loc ->
    format_loc ppf loc ;
    Format.fprintf ppf (if cap then "Eval string" else "eval string")

and format_loc ppf ?(cap = true) (source, ((ls, cs), (le, ce))) =
  let open Printf in
  if ls = le then
    if cs = ce then
      Format.fprintf ppf "%a, line %i, character %i:@ "
        (format_source ~cap) source ls cs
    else
      Format.fprintf ppf "%a, line %i, characters %i-%i:@ "
        (format_source ~cap) source ls cs ce
  else
    Format.fprintf ppf "%a, lines %i-%i, characters %i-%i:@ "
      (format_source ~cap) source ls le cs ce

let rec format_local_warning ppf descr =
  let open Printf in
  match descr with
  | Uninitialized_var s ->
    Format.fprintf ppf "%S not initialized" s
  | Unused_arg s  ->
    Format.fprintf ppf "%S not used" s
  | Duplicate_arg s  ->
    Format.fprintf ppf "argument %S appears several times" s
  | Duplicate_return s  ->
    Format.fprintf ppf "return variable %S appears several times" s
  | Var_arg_ret s  ->
    Format.fprintf ppf "return variable %S is also an argument" s
  | Unset_ret s  ->
    Format.fprintf ppf "return variable %S is never set" s
  | Return_as_var s  ->
    Format.fprintf ppf "return variable %S is used as a local variable" s
  | For_var_modif  ->
    Format.fprintf ppf "modifying variable of for loop does not change loop behavior"
  | Primitive_with_too_many_arguments (fun_name, i) ->
    Format.fprintf ppf "primitive %S called with too many arguments (>= %d)" fun_name i
  | Overriding_primitive fun_name ->
    Format.fprintf ppf "overriding primitive %S" fun_name
  | Overriding_declared_function (fun_name, fun_loc) ->
    Format.fprintf ppf "overriding function %S already declared at %a"
      fun_name (format_loc ~cap:false) fun_loc
  | Overriding_toplevel_function (fun_name, file) ->
    Format.fprintf ppf "overriding toplevel function %S of file %S" fun_name file
  | Unexpected_string_argument (fun_name, i, s, possible) ->
    Format.fprintf ppf "Function %S does not expect %S as argument %d. \
             Should be one of: %s"
      fun_name s (succ i) (String.concat ", " possible)
  | Unexpected_argument_type (fun_name, i, expected_type) ->
    Format.fprintf ppf "Function %S expects type %s as argument %d"
      fun_name expected_type (succ i)
  | Int_argument_out_of_range (fun_name, i, v, min, max) ->
    Format.fprintf ppf "Function %S does not expect %.1f as argument %d. \
                        Should be between %d and %d"
      fun_name v (succ i) min max
  | Var_def_not_used var_name ->
    Format.fprintf ppf "variable %S defined but not used" var_name
  | Var_redef_not_used var_name ->
    Format.fprintf ppf "variable %S redefined before being used" var_name
  | Break_outside_loop ->
    Format.fprintf ppf "break outside of loop"
  | Continue_outside_loop ->
    Format.fprintf ppf "continue outside of loop"

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
  | Function_name_not_started_by_lowercase -> 001
  | Function_name_contains_digits -> 002
  | Inconsistent_string_delimiters -> 003
  | Inconsistent_matrix_delimiters -> 004
  | Inconsistent_matrix_separators -> 005
  | Spaces_in_operator -> 006
  | Spaces_around_dot -> 007
  | Missing_function_parameters -> 008
  | Missing_catch -> 009
  | Misused_keyword -> 010
  | Keyword_as_shell_arg -> 011
  | Deprecated arg -> 012
  | Ambiguous_dot_left_of_oper -> 013
  | Ambiguous_dot_right_of_oper -> 014

and format_style_warning ppf descr =
  let open Printf in
  match descr with
  | Function_name_not_started_by_lowercase ->
    Format.fprintf ppf "function name not started by lowercase"
  | Function_name_contains_digits ->
    Format.fprintf ppf "function name contains digits"
  | Inconsistent_string_delimiters ->
    Format.fprintf ppf "inconsistent string delimiters"
  | Inconsistent_matrix_delimiters ->
    Format.fprintf ppf "inconsistent matrix delimiters"
  | Inconsistent_matrix_separators ->
    Format.fprintf ppf "inconsistent matrix separators"
  | Spaces_in_operator ->
    Format.fprintf ppf "spaces in operator"
  | Spaces_around_dot ->
    Format.fprintf ppf "spaces around dot"
  | Missing_function_parameters ->
    Format.fprintf ppf "missing function parameters"
  | Missing_catch ->
    Format.fprintf ppf "missing catch in try block"
  | Misused_keyword ->
    Format.fprintf ppf "misused keyword"
  | Keyword_as_shell_arg ->
    Format.fprintf ppf "keywords in shell args should be quoted"
  | Deprecated arg ->
    Format.fprintf ppf "deprecated %s" arg
  | Ambiguous_dot_left_of_oper ->
    Format.fprintf ppf "ambiguous decimal dot before operator"
  | Ambiguous_dot_right_of_oper ->
    Format.fprintf ppf "ambiguous decimal dot after operator"

(** Builds a displayable version of a location *)
let string_of_loc loc =
  let buf = Buffer.create 200 in
  format_loc (Format.formatter_of_buffer buf) ~cap:false loc ;
  Buffer.contents buf

(** Builds a displayable version of a source *)
let string_of_source source =
  let buf = Buffer.create 200 in
  format_source (Format.formatter_of_buffer buf) ~cap:false source ;
  Buffer.contents buf

(** Supported output formats *)
type format = Emacs | Firehose | Human

(** Formats a list of messages *)
let format_messages format messages ppf =
  let emacs colors () =
    let rec format_message ppf ((src, bounds), msg) =
      let cap = true in
      match msg with
      | Warning (L descr) ->
        Format.fprintf ppf
          (if colors then "\027[33m%aWarning L%03i:\027[0m %a@," else "%aWarning L%03i: %a@,")
          (format_loc ~cap) (src, bounds)
          (num_of_local_warning descr)
          format_local_warning descr
      | Warning (S descr) ->
        Format.fprintf ppf
          (if colors then "\027[33m%aWarning S%03i:\027[0m %a@," else "%aWarning S%03i: %a@,")
          (format_loc ~cap) (src, bounds)
          (num_of_style_warning descr)
          format_style_warning descr
      | Warning (W (name, msg)) ->
        Format.fprintf ppf
          (if colors then "\027[33m%aWarning %s:\027[0m %s@," else "%aWarning %s: %s@,")
          (format_loc ~cap) (src, bounds) name msg
      | Recovered msg ->
        Format.fprintf ppf
          (if colors then "\027[31m%aError:\027[0m %s@," else "%aError: %s@,")
          (format_loc ~cap) (src, bounds) msg
      | Insert (point, kwd) ->
        Format.fprintf ppf
          (if colors then "\027[32m%aInsert:\027[0m %S@," else "%aInsert %S@,")
          (format_loc ~cap) (src, (point, point)) kwd
      | Drop bounds ->
        Format.fprintf ppf
          (if colors then "\027[32m%aDrop\027[0m@," else "%aDrop:@,")
          (format_loc ~cap) (src, bounds)
      | Replace (bounds, rep) ->
        Format.fprintf ppf
          (if colors then "\027[32m%aReplace by:\027[0m %S@," else "%aReplace by: %S@,")
          (format_loc ~cap) (src, bounds) rep
    in
    Format.fprintf ppf "@[<v>" ;
    List.iter (format_message ppf) messages ;
    Format.fprintf ppf "@]%!"
  and firehose () =
    let attrs ppf =
      List.iter (fun (n, v) -> Format.fprintf ppf " %s=%S" n v)
    in
    let markup n a c =
      let c ppf () = c ppf in 
      Format.fprintf ppf "@[<hv 2><%s%a>@,%a@;<0 -2>@]</%s>" n attrs a c () n
    and ocmarkup n a =
      Format.fprintf ppf "<%s%a/>" n attrs a
    and br () =
      Format.fprintf ppf "@,"
    in
    let location (source, ((ls, cs), (le, ce))) =
      let point l c =
        ocmarkup "point" [ "line", string_of_int l ; "column", string_of_int c ]
      in
      (match source with
       | Forged ->
         ocmarkup "file" [ "given-path", "(ghost-code)" ]
       | String (name, _) ->
         ocmarkup "file" [ "given-path", Printf.sprintf "(%s)" name ]
       | File (name) ->
         ocmarkup "file" [ "given-path", name ]
       | Eval_string loc ->
         let buf = Buffer.create 200 in
         format_loc (Format.formatter_of_buffer buf) ~cap:false loc ;
         let name = Format.sprintf "(eval@%s)" (Buffer.contents buf) in
         ocmarkup "file" [ "given-path", name ]
      ) ;
      br () ;
      if ls = le && cs = ce then
        point ls cs
      else
        markup "range" [] (fun _ -> point ls cs ; point le ce) ;
 in
    let message ((src, bounds), msg) =
      match msg with
      | Warning (L descr) ->
        markup "issue" [ "test-id", Printf.sprintf "L%03i" (num_of_local_warning descr) ] (fun _ ->
            markup "location" [] (fun _ -> location (src, bounds)) ; br () ;
            markup "message" [] (fun _ -> format_local_warning ppf descr))
      | Warning (S descr) ->
        markup "issue" [ "test-id", Printf.sprintf "S%03i" (num_of_style_warning descr) ] (fun _ ->
            markup "location" [] (fun _ -> location (src, bounds)) ; br () ;
            markup "message" [] (fun _ -> format_style_warning ppf descr))
      | Warning (W (name, msg)) ->
        markup "issue" [ "test-id", "Warning" ] (fun _ ->
            markup "location" [] (fun _ -> location (src, bounds)) ; br () ;
            markup "message" [] (fun _ -> Format.fprintf ppf "%s" msg))
      | Recovered msg ->
        markup "issue" [ "test-id", "Error" ] (fun _ ->
            markup "location" [] (fun _ -> location (src, bounds)) ; br () ;
            markup "message" [] (fun _ -> Format.fprintf ppf "%s" msg))
      | Insert (point, kwd) ->
        markup "issue" [ "test-id", "Insert" ] (fun _ ->
            markup "location" [] (fun _ -> location (src, bounds)) ; br () ;
            markup "message" [] (fun _ -> Format.fprintf ppf "%s" kwd))
      | Drop bounds ->
        markup "issue" [ "test-id", "Drop" ] (fun _ ->
            markup "location" [] (fun _ -> location (src, bounds)))
      | Replace (bounds, rep) ->
        markup "issue" [ "test-id", "Insert" ] (fun _ ->
            markup "location" [] (fun _ -> location (src, bounds)) ; br () ;
            markup "message" [] (fun _ -> Format.fprintf ppf "%s" rep))
    in
    markup "analysis" [] (fun _ ->
        markup "meta" [] (fun _ ->
            ocmarkup "generator" [ "name", "scilint"  ; "version", ScilintManual.version ] ; br () ;
            markup "results" [] (fun _ ->
                let rec sep = function
                  | [] -> ()
                  | [ m ] -> message m 
                  | m :: ms -> message m ; br () ; sep ms
                in
                sep messages))) ;
    Format.fprintf ppf "@,%!"
  in
  match format with
  | Emacs -> emacs false ()
  | Human -> emacs true ()
  | Firehose -> firehose ()

(** Formats a list of messages in a string *)
let string_of_messages format messages =
  let buf = Buffer.create 10_000 in
  format_messages format messages (Format.formatter_of_buffer buf) ;
  Buffer.contents buf


(** Formats a list of messages in an output channel *)
let output_messages format messages fpout =
  format_messages format messages (Format.formatter_of_out_channel fpout)
