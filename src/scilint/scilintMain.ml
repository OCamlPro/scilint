(*  OCamlPro Scilab Toolbox - Scilint, a static analyzer for Scilab
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabParserAst
open ScilintWarning
open Printf

(** activated analyses, may rewrite the original ast or change it in place *)
let passes : (ast -> ast) list ref = ref []

(** called by the main on each code source passed on th CLI *)
let treat_source source =
  let parse_file, parse_string =
    match !ScilintOptions.parser with
    | Six ->
      ScilabSixParser.parse_file,
      ScilabSixParser.parse_string
    | Five true ->
      (fun f -> ScilabFiveParser.parse_file f),
      (fun n s -> ScilabFiveParser.parse_string n s)
    | Five false ->
      let filter ast =
        let ast = ref ast in
        try
          let checker = object
            inherit ast_iterator
            method! descr : 'a.'a descr -> unit = fun descr ->
              List.iter
                (function
                  | Recovered _ as m ->
                    let error = { descr with cstr = Error ; meta = [ m ] } in
                    ast := [ { descr with cstr = Exp error ; meta = [ m ] } ]
                  | _ -> ())
                descr.meta
          end in
          checker # ast !ast ;
          !ast
        with Exit -> !ast
      in
      (fun f -> filter (ScilabFiveParser.parse_file f)),
      (fun n s -> filter (ScilabFiveParser.parse_string n s))
  in
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
  let ast = List.fold_left (fun r anal -> anal r) ast !passes in
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
  printf "Welcome to Scilint's interactive mode\n%!" ;
  printf "Type your phrases, leave an empty line to submit, Ctrl-C to quit\n%!" ;
  interp "" 0

(** where the args are passed and all the fun starts *)
let main () =
  let sources : source list ref = ref [] in
  let toplevel = ref false in
  let open Arg in
  let options =
    [ ScilintOptions.print_ast_arg ;
      ScilintOptions.pretty_print_arg ;
      ScilintOptions.print_messages_arg ;
      ScilintOptions.print_time_arg ;
      ScilintOptions.format_arg ;
      ScilintOptions.parser_arg ;
      ("-toplevel", Set toplevel,
       "Launch an interactive toplevel after other inputs have been processed") ;
      ("-s", String ( fun str -> sources := String ("argument" ,str) :: !sources),
       "Add a verbatim text input from the command line") ]
  and anon_fun fn =
    sources := File fn :: !sources
  and usage_msg =
    "Hello, I am Scilint, a syntax checker for Scilab.\n\
     Usage: scilint [OPTIONS] <file1.sci> <file2.sci> ..." ;
  in
  parse options anon_fun usage_msg ;
  if !sources = [] && not !toplevel then
    usage options usage_msg ;
  List.iter treat_source (List.rev !sources) ;
  if !toplevel then interactive ()

let _ = main ()
