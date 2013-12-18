(*  OCamlPro Scilab Toolbox - Scintax, a Real time syntax checker for Scilab
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabFiveParserAst
open ScilabFiveParser
open Printf

(** commane line options *)
module Opts = struct
  let print_ast = ref false
  let print_time = ref false
  let pretty_print = ref false
  let print_messages = ref true
  let format = ref "human" (* | "emacs" | "firehose" *)
end

(** called by the main on each code source passed on th CLI *)
let treat_source source =
  let parse, display_name =
    match source with
    | File fn -> (fun () -> parse_file fn), sprintf "file %S" fn
    | String str -> (fun () -> parse_string str), sprintf "input %S" str
    | Forged -> assert false
  in 
  let ast =
    if !Opts.print_time then begin
      printf "Parsing %s ...%!" display_name ;
      let t0 = Sys.time () in
      let ast = parse () in
      let t1 = Sys.time () in
      printf "\b\b\bdone in %gms.\n%!" ((t1 -. t0) *. 1000.) ;
      ast
    end else parse ()
  in
  if !Opts.print_ast then begin
    printf "Raw syntax tree:\n" ;
    Sexp.pretty_output stdout ast ;
    printf "\n"
  end ;
  if !Opts.pretty_print then begin
    printf "Pretty printed:\n" ;
    Pretty.pretty_output stdout ast ;
    printf "\n"
  end ;
  if !Opts.print_messages then begin
    let messages = ref [] in
    let source (source : source) =
      match source with
      | String str -> "input"
      | File name -> name
      | Forged -> "ghost"
    in
    let collect = object
      inherit ast_iterator as mom
      method! descr : 'a. 'a descr -> unit
        = fun { meta ; loc = (src, ((ls, cs), (le, ce))) } ->
          List.iter
            (function
              | Warning msg ->
                messages :=
                  (sprintf "%s:%d.%d:%d.%d: Warning: %s\n"
                     (source src) ls cs le ce msg)
                  :: !messages
              | Recovered msg ->
                messages :=
                  (sprintf "%s:%d.%d:%d.%d: Error: %s\n"
                     (source src) ls cs le ce msg)
                  :: !messages
              | Insert ((l, c), kwd, msg) ->
                messages :=
                  (sprintf "%s:%d.%d: Insert %S: %s\n"
                     (source src) l c kwd msg)
                  :: !messages
              | Drop (((ls, cs), (le, ce)), msg) ->
                messages :=
                  (sprintf "%s:%d.%d:%d.%d: Drop: %s\n"
                     (source src) ls cs le ce msg)
                  :: !messages
              | Replace (((ls, cs), (le, ce)), rep, msg) ->
                messages :=
                  (sprintf "%s:%d.%d:%d.%d: Replace %S: %s\n"
                     (source src) ls cs le ce rep msg)
                  :: !messages)
            meta
    end in
    collect # ast ast ;
    let messages = List.rev !messages in
    if messages <> [] then begin
      printf "Messages:\n" ;
      List.iter print_string messages
    end
  end

(** a small toplevel for experimentation purposes *)
let interactive () =
  Opts.print_ast := true ;
  let rec interp acc =
    let open Printf in
    Printf.printf "--> %!" ;
    let phrase =
      try input_line stdin
      with End_of_file -> exit 0
    in
    if phrase = "" then begin
      treat_source (String acc) ;
      interp ""
    end else
      let acc = if acc = "" then acc else acc ^ "\n" in
      interp (acc ^ phrase)
  in
  printf "Welcome to Scintax's interactive mode\n%!" ;
  printf "Type your phrases, leave an empty line to submit, Ctrl-C to quit\n%!" ;
  interp ""

(** where the args are passed and all the fun starts *)
let main () =
  let sources : source list ref = ref [] in
  let toplevel = ref false in
  let open Arg in
  let options =
    [ ("-ast", Set Opts.print_ast,
       "Output the syntax tree of all inputs as an S-expression") ;
      ("-pretty", Set Opts.pretty_print,
       "Output a reformatted version of all inputs") ;
      ("-toplevel", Set toplevel,
       "Launch an interactive toplevel after other inputs have been processed") ;
      ("-silent", Clear Opts.print_messages,
       "Do not display error messages") ;
      ("-time", Set Opts.print_time,
       "Display parsing time") ;
      ("-format", Symbol ([ "human" ; "emacs" ; "firehose" ], (:=) Opts.format),
       " Set the format of warnings (default is \"human\")") ;
      ("-s", String ( fun str -> sources := String str :: !sources),
       "Add a verbatim text input from the command line") ]
  and anon_fun fn =
    sources := File fn :: !sources
  and usage_msg =
    "Hello, I am Scintax, a syntax checker for Scilab.\n\
     Usage: scintax [OPTIONS] <file1.sci> <file2.sci> ..." ;
  in
  parse options anon_fun usage_msg ;
  if !sources = [] && not !toplevel then
    usage options usage_msg ;
  List.iter treat_source (List.rev !sources) ;
  if !toplevel then interactive ()

let _ = main ()
