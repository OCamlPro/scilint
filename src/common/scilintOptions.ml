(*  OCamlPro Scilab Toolbox - Common options handling module
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open Arg
open ScilabParserAst

(** Dump the ast of each treated input (mostly for debugging) *)

let print_ast = ref false
let print_ast_arg =
  ("-ast", Set print_ast, "Prints the ast of each input (debug)")
  
(** Print the time taken by long running operations *)

let print_time = ref false
let print_time_arg =
  ("-time", Set print_time, "Prints timings (debug)")
  
(** Optional time quota *)

let timeout = ref infinity
let timeout_arg =
  ("-timeout", Set_float timeout, "Sets the maximum execution time")
  
(** Dump a pretty printed AST (mostly for debugging) *)

let pretty_print = ref false
let pretty_print_arg =
  ("-pretty", Set pretty_print, "Pretty prints the ast of each input (debug)")

(** Print warnings *)

let print_warnings = ref true
let print_warnings_arg =
  ("-nowarn", Clear print_warnings, "Do not display warnings")

(** Print debug / info messages *)

let print_messages = ref true
let print_messages_arg =
  ("-quiet", Clear print_messages, "Do not display non-warning messages")

(** Toplevel mode *)

let toplevel_mode = ref false
let toplevel_mode_arg =
  ("-toplevel", Arg.Set toplevel_mode, "Launch an interactive toplevel")

(** Command line input *)

let cli_input_arg sources =
  let add str =
    sources := ScilabLocations.String ("cli-input" ,str) :: !sources
  in
  ("-expr", Arg.String add, "Add verbatim code from the command line")

let cli_input_anon sources fn =
  sources := ScilabLocations.File fn :: !sources

(** Message output format: "emacs", or "firehose" *)

type format = ScilintWarning.format = Emacs | Firehose | Human
let format = ref Human
let format_arg =
  let set_format = function
    | "emacs" -> format := Emacs
    | "human" -> format := Human
    | "firehose" -> format := Firehose
    | _ -> assert false
  in
  ("-format", Symbol ([ "emacs" ; "firehose" ; "human" ], set_format),
   " Set the format of warnings (default is \"human\")") ;

(** Parser version *)

type parser_version = Five of bool | Six
let parser = ref (Five true)
let parser_arg =
  let set_parser = function
    | "scilab-6" -> parser := Six
    | "scilab-5" -> parser := Five false
    | "scilab-5-resilient" -> parser := Five true
    | _ -> assert false
  in
  ("-parser", Symbol ([ "scilab-5" ; "scilab-5-resilient" ; "scilab-6" ], set_parser),
   " Set the input format (default is \"scilab-5-resilient\")")

(** Activated analyses *)

let passes : (string * (ast -> ast)) list ref = ref []
let passes_args : (string * Arg.spec * string) list ref = ref []

let add_pass name pass descr active =
  let callback = ref (if active then pass else (fun ast -> ast)) in
  passes := (name, (fun ast -> !callback ast)) :: !passes ;
  passes_args :=
    !passes_args
    @ [ ("-" ^ name,
         Unit (fun () -> callback := pass),
         "Activate " ^ descr
         ^ (if active then " (by default)" else "")) ;
        ("-no-" ^ name,
         Unit (fun () -> callback := pass),
         "Deactivate " ^ descr
         ^ (if not active then " (by default)" else ""))]
