(*  OCamlPro Scilab Toolbox - OcSciLab
 *  Copyright (C) 2014 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabParserAst
open ScilintWarning
open ScilintOptions
open Printf

(** a small toplevel for experimentation purposes *)
let interactive state lib=
  let rec interp prompt acc nb =
    printf "%s %!" (if prompt then "-->" else "   ") ;
    let phrase =
      try input_line stdin
      with End_of_file -> exit 0
    in
    if phrase = "" then begin
      Interp.treat_source state lib (String ("input-" ^ string_of_int nb, acc)) ;
      printf "\n%!" ;
      interp true "" (succ nb)
    end else
      let acc = if acc = "" then acc else acc ^ "\n" in
      interp false (acc ^ phrase) nb
  in
  printf "Welcome to OcSciLab's interactive mode\n%!" ;
  printf "Type your phrases, leave an empty line to submit, Ctrl-C to quit\n%!" ;
  interp true "" 0

(** where the args are passed and all the fun starts *)
let main () =
  let sources : source list ref = ref [] in
  let options =
    [ print_ast_arg ; pretty_print_arg ; print_messages_arg ; print_time_arg ;
      format_arg ; parser_arg ; toplevel_mode_arg ; cli_input_arg sources ]
    @ !passes_args
  and usage_msg =
    "Hello, I am OcSciLab, an experimental Scilab interpreter in OCaml.\n\
     Usage: scilint [OPTIONS] <file1.sci> <file2.sci> ..." ;
  in
  Arg.parse options (cli_input_anon sources) usage_msg ;
  if !sources = [] && not !toplevel_mode then
    Arg.usage options usage_msg ;
  let state = InterpCore.State.init () in
  let lib = InterpCore.Dispatcher.create () in
  InterpLib.load_libraries state lib ;
  List.iter (Interp.treat_source state lib) (List.rev !sources) ;
  if !toplevel_mode then interactive state lib

let _ = main ()
