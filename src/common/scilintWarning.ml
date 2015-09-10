(*  OCamlPro Scilab Toolbox - Common types and code for displaying messages
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
  | Werror of warning
  | Warning of warning
  | Unrecovered of string
  | Recovered of string
  | Hint of string
  | Drop
  | Insert of string
  | Replace of string
  | Generic of string * string option * (Format.formatter -> unit)

(** Various kinds of warnings *)
and warning =
  | L of local_warning
  | S of style_warning
  | P of program_warning
  | W of string * string (** generic warning (kind, message) *)

(** Inter-procedural warnings *)
and program_warning =
  | Variable_cleared of string
  | Null_result
  | Too_few_arguments of int
  | Too_many_arguments of int
  | Too_few_results of int
  | Too_many_results of int
  | Unused_results of int
  | Unused_argument_label of string
  | Duplicate_argument_label of string
  | Unbound_argument_label of string

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

let rec format_source ppf ?(inline = false) source =
  match source with
  | Forged ->
    Format.fprintf ppf (if not inline then "ghost code" else "Ghost code")
  | String (name, _) ->
    Format.fprintf ppf (if not inline then "String %S" else "string %S") name
  | File (name) ->
    Format.fprintf ppf (if not inline then "File %S" else "file %S") name
  | Eval_string loc ->
    format_loc ppf loc ;
    Format.fprintf ppf (if not inline then "Eval string" else "eval string")
  | External n ->
    Format.fprintf ppf (if not inline then "Primitive %s" else "primitive %s") n
  | Call (_, _) -> assert false
  | Nowhere -> assert false

and format_loc ppf ?(inline = false) loc =
  match loc with
  | Nowhere, _ -> ()
  | External _ as source, _ ->
    Format.fprintf ppf "%a" (format_source ~inline) source ;
    if not inline then Format.fprintf ppf ":@,"
  | Call (loc, source'), bounds ->
    format_loc ppf ~inline loc ;
    format_loc ppf ~inline (source', bounds) ;
  | source, ((ls, cs), (le, ce)) ->
    if ls = le then
      if cs = ce then
        Format.fprintf ppf "%a, line %i, character %i"
          (format_source ~inline) source ls cs
      else
        Format.fprintf ppf "%a, line %i, characters %i-%i"
          (format_source ~inline) source ls cs ce
    else
      Format.fprintf ppf "%a, lines %i-%i, characters %i-%i"
        (format_source ~inline) source ls le cs ce ;
    if not inline then Format.fprintf ppf ":@,"

let rec format_local_warning ppf descr =
  match descr with
  | Uninitialized_var s ->
    Format.fprintf ppf "variable %s not initialized" s
  | Unused_arg s  ->
    Format.fprintf ppf "variable %s not used" s
  | Duplicate_arg s  ->
    Format.fprintf ppf "argument %s appears several times" s
  | Duplicate_return s  ->
    Format.fprintf ppf "return variable %s appears several times" s
  | Var_arg_ret s  ->
    Format.fprintf ppf "return variable %s is also an argument" s
  | Unset_ret s  ->
    Format.fprintf ppf "return variable %s is not set" s
  | Return_as_var s  ->
    Format.fprintf ppf "return variable %s is used as a local variable" s
  | For_var_modif  ->
    Format.fprintf ppf "modifying variable of for loop does not change loop behavior"
  | Primitive_with_too_many_arguments (fun_name, i) ->
    Format.fprintf ppf "primitive %s called with too many arguments (>= %d)" fun_name i
  | Overriding_primitive fun_name ->
    Format.fprintf ppf "overriding primitive %s" fun_name
  | Overriding_declared_function (fun_name, fun_loc) ->
    Format.fprintf ppf "overriding function %s already declared at %a"
      fun_name (format_loc ~inline:true) fun_loc
  | Overriding_toplevel_function (fun_name, file) ->
    Format.fprintf ppf "overriding toplevel function %s of file %S" fun_name file
  | Unexpected_string_argument (fun_name, i, s, possible) ->
    Format.fprintf ppf "Function %s does not expect %S as argument %d. \
             Should be one of: %s"
      fun_name s (succ i) (String.concat ", " possible)
  | Unexpected_argument_type (fun_name, i, expected_type) ->
    Format.fprintf ppf "Function %s expects type %s as argument %d"
      fun_name expected_type (succ i)
  | Int_argument_out_of_range (fun_name, i, v, min, max) ->
    Format.fprintf ppf "Function %s does not expect %.1f as argument %d. \
                        Should be between %d and %d"
      fun_name v (succ i) min max
  | Var_def_not_used var_name ->
    Format.fprintf ppf "variable %s defined but not used" var_name
  | Var_redef_not_used var_name ->
    Format.fprintf ppf "variable %s redefined before being used" var_name
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
  | Ambiguous_toplevel_expression -> 015

and num_of_program_warning descr =
  match descr with
  | Variable_cleared _ -> 001
  | Null_result -> 002
  | Too_few_arguments _ -> 003
  | Too_many_arguments _ -> 004
  | Too_few_results _ -> 005
  | Too_many_results _ -> 006
  | Unused_results _ -> 007
  | Unused_argument_label _ -> 008
  | Duplicate_argument_label _ -> 009
  | Unbound_argument_label _ -> 010

and format_style_warning ppf descr =
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
  | Ambiguous_toplevel_expression ->
    Format.fprintf ppf "cannot decide between a toplevel expression or a shell call"


and format_program_warning ppf descr =
  match descr with
  | Variable_cleared n ->
    Format.fprintf ppf "variable %s cleared" n
  | Null_result ->
    Format.fprintf ppf "this expression returns a null result"
  | Too_few_arguments 1 ->
    Format.fprintf ppf "this function takes one more argument"
  | Too_many_arguments 1 ->
    Format.fprintf ppf "this function takes one less argument"
  | Too_few_results 1 ->
    Format.fprintf ppf "this expression should return one more result"
  | Too_many_results 1 ->
    Format.fprintf ppf "a result of this expression cannot be assigned"
  | Unused_results 1 ->
    Format.fprintf ppf "a result of this expression is ignored"
  | Too_few_arguments nb ->
    Format.fprintf ppf "this function takes %d more arguments" nb
  | Too_many_arguments nb ->
    Format.fprintf ppf "this function takes %d less arguments" nb
  | Too_few_results nb ->
    Format.fprintf ppf "this expression should return %d more results" nb
  | Too_many_results nb ->
    Format.fprintf ppf "%d results of this expression cannot be assigned" nb
  | Unused_results nb ->
    Format.fprintf ppf "%d results of this expression are ignored" nb
  | Unused_argument_label n ->
    Format.fprintf ppf "argument name %s unused" n
  | Duplicate_argument_label lbl ->
    Format.fprintf ppf "argument name %s provided twice" lbl
  | Unbound_argument_label lbl ->
    Format.fprintf ppf "argument name %s unknown to the function" lbl

(** Builds a displayable version of a location *)
let string_of_loc loc =
  let buf = Buffer.create 200 in
  let ppf = Format.formatter_of_buffer buf in
  format_loc ppf ~inline:true loc ;
  Format.fprintf ppf "%!" ;
  Buffer.contents buf

(** Builds a displayable version of a source *)
let string_of_source source =
  let buf = Buffer.create 200 in
  let ppf = Format.formatter_of_buffer buf in
  format_source ppf ~inline:true source ;
  Format.fprintf ppf "%!" ;
  Buffer.contents buf

(** Supported output formats *)
type format = Emacs | Firehose | Human

(** Formats a list of messages *)
let format_messages format messages ppf =
  let emacs colors () =
    let begin_color ppf s =
      if colors then Format.fprintf ppf "@<0>%s@<0>%s@<0>%s" "\027[" s "m" in
    let end_color ppf () =
      if colors then Format.fprintf ppf "@<0>%s" "\027[0m" in
    let format_message format_loc ppf (loc, msg) =
      match msg with
      | Generic (color, None, callback) ->
        Format.fprintf ppf "%a%a%a@[<hov>%a@]"
          begin_color color format_loc loc end_color ()
          (fun p () ->  callback p) ()
      | Generic (color, Some n, callback) ->
        Format.fprintf ppf "%a%a%s:%a @[<hov>%a@]"
          begin_color color format_loc loc n end_color ()
          (fun p () ->  callback p) ()
      | Werror (L descr) ->
        Format.fprintf ppf "%a%aError L%03i:%a @[<hov>%a@]"
          begin_color "31" format_loc loc (num_of_local_warning descr) end_color ()
          format_local_warning descr
      | Werror (P descr) ->
        Format.fprintf ppf "%a%aError P%03i:%a @[<hov>%a@]"
          begin_color "31" format_loc loc (num_of_program_warning descr) end_color ()
          format_program_warning descr
      | Werror (S descr) ->
        Format.fprintf ppf "%a%aError S%03i:%a @[<hov>%a@]"
          begin_color "31" format_loc loc (num_of_style_warning descr) end_color ()
          format_style_warning descr
      | Werror (W (name, msg)) ->
        Format.fprintf ppf "%a%aError %s:%a @[<hov>%s@]"
          begin_color "31" format_loc loc name end_color () msg
      | Warning (L descr) ->
        Format.fprintf ppf "%a%aWarning L%03i:%a @[<hov>%a@]"
          begin_color "33" format_loc loc (num_of_local_warning descr) end_color ()
          format_local_warning descr
      | Warning (P descr) ->
        Format.fprintf ppf "%a%aWarning P%03i:%a @[<hov>%a@]"
          begin_color "33" format_loc loc (num_of_program_warning descr) end_color ()
          format_program_warning descr
      | Warning (S descr) ->
        Format.fprintf ppf "%a%aWarning S%03i:%a @[<hov>%a@]"
          begin_color "33" format_loc loc (num_of_style_warning descr) end_color ()
          format_style_warning descr
      | Warning (W (name, msg)) ->
        Format.fprintf ppf "%a%aWarning %s:%a %s"
          begin_color "33" format_loc loc name end_color () msg
      | Recovered msg ->
        Format.fprintf ppf "%a%aRecovered:%a %s"
          begin_color "31" format_loc loc end_color () msg
      | Unrecovered msg ->
        Format.fprintf ppf "%a%aError:%a %s"
          begin_color "31" format_loc loc end_color () msg
      | Hint msg ->
        Format.fprintf ppf "%a%aHint:%a %s"
          begin_color "36" format_loc loc end_color () msg
      | Insert kwd ->
        Format.fprintf ppf "%a%aInsert:%a %S"
          begin_color "32" format_loc loc end_color () kwd
      | Drop ->
        Format.fprintf ppf "%a%aDrop%a"
          begin_color "32" format_loc loc end_color ()
      | Replace rep ->
        Format.fprintf ppf "%a%aReplace by:%a %S"
          begin_color "32" format_loc loc end_color () rep
    in
    Format.fprintf ppf "@[<v 0>" ;
    let rec do_list prev messages =
      match prev, messages with
      | _, [] -> ()
      | None, (loc, msg) :: rest ->
        format_message (format_loc ~inline:false) ppf (loc, msg) ;
        Format.fprintf ppf "@," ;
        do_list (Some loc) rest
      | Some prev, (loc, msg) :: rest when prev = loc ->
        format_message (fun _ _ -> ()) ppf (loc, msg) ;
        Format.fprintf ppf "@," ;
        do_list (Some loc) rest
      | Some prev, (loc, msg) :: rest ->
        format_message (format_loc ~inline:false) ppf (loc, msg) ;
        Format.fprintf ppf "@," ;
        do_list (Some loc) rest
    in
    do_list None messages ;
    Format.fprintf ppf "@]@?"
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
    let location (source, bounds) =
      let point l c =
        ocmarkup "point" [ "line", string_of_int l ; "column", string_of_int c ]
      in
      let rec print_source bounds = function
        | Forged ->
          ocmarkup "file" [ "given-path", "(ghost-code)" ] ; print_bounds bounds
        | String (name, _) ->
          ocmarkup "file" [ "given-path", Printf.sprintf "(%s)" name ] ; print_bounds bounds
        | File name ->
          ocmarkup "file" [ "given-path", name ] ; print_bounds bounds
        | External name ->
          ocmarkup "primitive" [ "name", name ]
        | Eval_string (source, bounds) ->
          print_source bounds source
        | Call (loc, source') ->
          print_source bounds source' ; print_bounds bounds
        | Nowhere -> ()
      and print_bounds ((ls, cs), (le, ce)) =
        br () ;
        if ls = le && cs = ce then
          point ls cs
        else
          markup "range" [] (fun _ -> point ls cs ; point le ce)
      in print_source bounds source
    in
    let message (loc, msg) =
      match msg with
      | Generic (_, Some n, f) ->
        markup "issue" [ "test-id", n ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> f ppf))
      | Generic (_, None, f) ->
        markup "issue" [ "test-id", "Generic" ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> f ppf))
      | Werror (L descr) ->
        markup "issue" [ "test-id", Printf.sprintf "L%03i" (num_of_local_warning descr) ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> format_local_warning ppf descr))
      | Werror (P descr) ->
        markup "issue" [ "test-id", Printf.sprintf "P%03i" (num_of_program_warning descr) ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> format_program_warning ppf descr))
      | Werror (S descr) ->
        markup "issue" [ "test-id", Printf.sprintf "S%03i" (num_of_style_warning descr) ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> format_style_warning ppf descr))
      | Werror (W (name, msg)) ->
        markup "issue" [ "test-id", "Werror" ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> Format.fprintf ppf "%s" msg))
      | Warning (L descr) ->
        markup "issue" [ "test-id", Printf.sprintf "L%03i" (num_of_local_warning descr) ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> format_local_warning ppf descr))
      | Warning (P descr) ->
        markup "issue" [ "test-id", Printf.sprintf "P%03i" (num_of_program_warning descr) ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> format_program_warning ppf descr))
      | Warning (S descr) ->
        markup "issue" [ "test-id", Printf.sprintf "S%03i" (num_of_style_warning descr) ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> format_style_warning ppf descr))
      | Warning (W (name, msg)) ->
        markup "issue" [ "test-id", "Warning" ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> Format.fprintf ppf "%s" msg))
      | Recovered msg ->
        markup "issue" [ "test-id", "Recovered" ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> Format.fprintf ppf "%s" msg))
      | Hint msg ->
        markup "issue" [ "test-id", "Hint" ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> Format.fprintf ppf "%s" msg))
      | Unrecovered msg ->
        markup "issue" [ "test-id", "Error" ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> Format.fprintf ppf "%s" msg))
      | Insert kwd ->
        markup "issue" [ "test-id", "Insert" ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
            markup "message" [] (fun _ -> Format.fprintf ppf "%s" kwd))
      | Drop ->
        markup "issue" [ "test-id", "Drop" ] (fun _ ->
            markup "location" [] (fun _ -> location loc))
      | Replace rep ->
        markup "issue" [ "test-id", "Insert" ] (fun _ ->
            markup "location" [] (fun _ -> location loc) ; br () ;
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
    Format.fprintf ppf "@?"
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
