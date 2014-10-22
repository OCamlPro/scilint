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

(** called by the main on each code source passed on th CLI *)
let treat_source state lib source =
  let parse () =
    match source with
    | File fn -> SelectedParser.parse_file fn
    | String (name, str) -> SelectedParser.parse_string name str
    | _ -> assert false
  in
  let ast =
    if !print_time then begin
      printf "Parsing %s ...%!" (string_of_source source) ;
      let t0 = Sys.time () in
      let ast = parse () in
      let t1 = Sys.time () in
      printf "\b\b\bdone in %gms.\n%!" ((t1 -. t0) *. 1000.) ;
      ast
    end else parse ()
  in
  let ast = List.fold_left (fun r (name, anal) -> anal r) ast !passes in
  if !print_ast then begin
    printf "Syntax tree:\n" ;
    Sexp.pretty_output stdout ast ;
    printf "\n"
  end ;
  if !pretty_print then begin
    printf "Pretty printed:\n" ;
    Pretty.pretty_output stdout ast ;
    printf "\n"
  end ;
  if !ScilintOptions.print_messages then begin
    let messages = collect_messages ast in
    output_messages !ScilintOptions.format messages stdout
  end ;
  if ast = [] then
    let w = (source, ((1, 0), (1, 0))),
            Unrecovered "nothing to do" in
    output_messages !ScilintOptions.format [ w ] stdout
  else
    Interp.interpret state lib ast

(** a small toplevel for experimentation purposes *)
let interactive state lib=
  let rec interp prompt acc nb =
    printf "%s %!" (if prompt then "-->" else "   ") ;
    let phrase =
      try input_line stdin
      with End_of_file -> exit 0
    in
    if phrase = "" then begin
      treat_source state lib (String ("input-" ^ string_of_int nb, acc)) ;
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
  List.iter (treat_source state lib) (List.rev !sources) ;
  if !toplevel_mode then interactive state lib

let _ = main ()
