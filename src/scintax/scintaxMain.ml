(*  OCamlPro Scilab Toolbox - Scintax, a Real time syntax checker for Scilab
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabParserAst
open ScilabFiveParser
open ScilintWarning
open Printf

(** called by the main on each code source passed on th CLI *)
let treat_source source =
  let parse () =
    match source with
    | File fn -> parse_file fn
    | String (name, str) -> parse_string name str
    | _ -> assert false
  in 
  let ast =
    if !ScilintOptions.print_time then begin
      printf "Parsing %s ...%!" (string_of_source source) ;
      let t0 = Sys.time () in
      let ast = parse () in
      let t1 = Sys.time () in
      printf "\b\b\bdone in %gms.\n%!" ((t1 -. t0) *. 1000.) ;
      ast
    end else parse ()
  in
  if !ScilintOptions.print_ast then begin
    printf "Raw syntax tree:\n" ;
    Sexp.pretty_output stdout ast ;
    printf "\n"
  end ;
  if !ScilintOptions.pretty_print then begin
    printf "Pretty printed:\n" ;
    Pretty.pretty_output stdout ast ;
    printf "\n"
  end ;
  if !ScilintOptions.print_messages then begin
    let messages = collect_messages ast in
    output_messages !ScilintOptions.format messages stdout
  end

(** a small toplevel for experimentation purposes *)
let interactive () =
  ScilintOptions.print_ast := true ;
  let rec interp acc nb =
    let open Printf in
    Printf.printf "--> %!" ;
    let phrase =
      try input_line stdin
      with End_of_file -> exit 0
    in
    if phrase = "" then begin
      treat_source (String ("input-" ^ string_of_int nb, acc)) ;
      interp "" (succ nb)
    end else
      let acc = if acc = "" then acc else acc ^ "\n" in
      interp (acc ^ phrase) nb
  in
  printf "Welcome to Scintax's interactive mode\n%!" ;
  printf "Type your phrases, leave an empty line to submit, Ctrl-C to quit\n%!" ;
  interp "" 0

(** where the args are passed and all the fun starts *)
let main () =
  let open ScilintOptions in
  let sources : source list ref = ref [] in
  let options =
      [ print_ast_arg ; pretty_print_arg ; print_messages_arg ; print_time_arg ;
        format_arg ; toplevel_mode_arg ; cli_input_arg sources ]
  and usage_msg =
    "Hello, I am Scintax, a syntax checker for Scilab.\n\
     Usage: scintax [OPTIONS] <file1.sci> <file2.sci> ..." ;
  in
  Arg.parse options (cli_input_anon sources) usage_msg ;
  if !sources = [] && not !toplevel_mode then
    Arg.usage options usage_msg ;
  List.iter treat_source (List.rev !sources) ;
  if !toplevel_mode then interactive ()

let _ = main ()
