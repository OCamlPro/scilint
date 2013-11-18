let file = ref ""
let test_flag = ref false
let analyze_flag = ref false
let type_flag = ref false
let cfg_flag = ref false
let cfg_file = ref false
let args = Arg.align [
  "-I", Arg.String ScilintProject.add_to_path,
  "DIRECTORY Add DIRECTORY to search path";
]
(* let args = [("-t", Arg.Unit (fun () -> test_flag := true), ": make stats on scilab code base"); *)
(*             ("-a", Arg.String (fun s -> analyze_flag := true; file := s), ": analyze scilab source code"); *)
(*             ("-typ", Arg.String (fun s -> type_flag := true; file := s), ": try to type a scilab program"); *)
(*             ("-cfg", Arg.Unit (fun () -> cfg_flag := true), ": try to create config file if current dir is a scilab project"); *)
(*             ("-load", Arg.String (fun s -> cfg_file := true; file := s), ": try to load config file if current dir is a scilab project")] *)
(* let usage = "Usage: " ^ Sys.argv.(0) ^ " [-t] [-a file] [-typ file] [-cfg] [-load] [file]" *)
let usage = "Usage: " ^ Sys.argv.(0) ^ " [file]"

let cpt_files = ref 0

let list_ext = [ ".sci"; ".sce"; ".tst" ]

let list_ext_bin = [ ".sci.bin"; ".sce.bin"; ".tst.bin" ]

let scilab5_modules_path = "/home/michael/scilab.5/scilab-5.4.0/modules/"

let scilab6_test_path = "/home/michael/scilab.6/scilab/test/"

let richelieu_test_path = "/home/michael/dev_sci/richelieu/"

let scilab_forge_test_path = "/home/michael/test_forge/mirror.forge.scilab.org-1.4GB/"

exception ParserError of string * string * int * int
exception LexerError of string * string * int * int

let print_error file msg line char =
  ScilabUtils.print_loc file line char msg

let print_parser_infos file token line char =
  let msg = "Error : Parsing error at token " ^ token in
  print_error file msg line char

let print_lex_infos file token line char =
  let msg = "Error : Syntax error at token " ^ token  in
  print_error file msg line char

let print_err = function
  | ParserError (file, tok, line, char) ->
      print_parser_infos file tok line char
  | LexerError (file, tok, line, char) ->
      print_lex_infos file tok line char
  | _ as err -> raise err

let get_length ic =
  let buf = Buffer.create 4 in
  Buffer.add_channel buf ic 4;
  let s = Buffer.contents buf in
  let c0 = int_of_char (String.unsafe_get s 0) in
  let c1 = int_of_char (String.unsafe_get s 1) in
  let c2 = int_of_char (String.unsafe_get s 2) in
  let c3 = int_of_char (String.unsafe_get s 3) in
  c0 + ((c1 + ((c2 + (c3 lsl 8)) lsl 8)) lsl 8)

let parse_file file =
  let ch = if file = "" then stdin else open_in file in
  let new_prog = ScilabPreParser.pre_parse ch in
  let lexbuf = Lexing.from_string new_prog in
  ScilabLexer.init_lexer_var ();
  try
    let ast = ScilabParser.program ScilabLexer.token lexbuf in
    flush stdout;
    close_in ch;
    ast
  with
    | Parsing.Parse_error ->
        let (tok, line, cnum) = ScilabUtils.get_location_from_lexbuf lexbuf in
        flush stdout;
        close_in ch;
        raise (ParserError (file, tok, line, cnum))
    | ScilabLexer.Err_str str_err ->
        let (tok, line, cnum) = ScilabUtils.get_location_from_lexbuf lexbuf in
        print_string str_err;
        flush stdout;
        close_in ch;
        raise (LexerError (file, tok, line, cnum))
    | ScilabLexer.Lex_err str_lex ->
        let (tok, line, cnum) = ScilabUtils.get_location_from_lexbuf lexbuf in
        print_string str_lex;
        flush stdout;
        close_in ch;
        raise (LexerError (file, tok, line, cnum))
    | _ as err -> raise err

let run_deff file =
  try
    let ast = parse_file file in
    match ast with
      | ScilabAst.Exp exp ->
          print_endline "-> OK\n";
          ScilabDeffRefactoring.refactor_deff exp
      | _ -> print_endline "-> Error not an Exp\n"
  with _ as err -> print_err err

let run_test file =
  try
    let ast = parse_file file in
    match ast with
      | ScilabAst.Exp exp -> print_endline "-> OK\n"
      | _ -> print_endline "-> Error not an Exp\n"
  with _ as err -> print_err err

let run_type_file file =
  Printf.printf "File %S\n%!" file;
  try
    let ast = parse_file file in
    match ast with
      | ScilabAst.Exp exp ->
          ScilabFunctionAnalyze.analyze file exp
      | _ -> print_endline "-> Error not an Exp\n"
  with _ as err -> print_err err

let run_analyze_file file =
  try
    let ast = parse_file file in
    match ast with
      | ScilabAst.Exp exp ->
          print_endline "-> OK\n";
          incr cpt_files;
          ScilabAstStats.analyze_ast exp;
          ScilabFunctionAnalyze.analyze file exp
      | _ -> print_endline "-> Error not an Exp\n"
  with _ as err -> print_err err

let rec run_tests fun_iter dirname =
  let files = Sys.readdir dirname in
  (* Printf.printf "# tests to run in %s : %i\n\n" dirname (Array.length files); *)
  Array.iter (fun file ->
    try
      let file = Filename.concat dirname file in
      if Sys.is_directory file then run_tests fun_iter file;
      if List.exists (Filename.check_suffix file) list_ext then
        begin
          fun_iter file
          (* printf.printf "%s \n" dirname *)
        end
    with _ -> ()
  ) files

let _ =
  Printf.printf "scilint: scilab code checker, by OCamlPro SAS\n%!";
  Arg.parse args (fun s -> run_type_file s) usage;
  if !test_flag
  then
    begin
      let dir_tests =
        [ (* My tests *)
          (* "test/"; *)

          (* Scilab 6' tests *)
          (* scilab6_test_path*)

          (* Scilab 5' tests *)
          (* scilab5_modules_path*)

          (* Richelieu' tests *)
          (* richelieu_test_path *)

          (* Scilab forge *)
          scilab_forge_test_path

         (* "/home/michael/git_scilab/richelieu/scilab/modules/jit_ocaml/test_stats" *)
        ] in
      List.iter (run_tests run_analyze_file) dir_tests;
      ScilabAstStats.print_fun_stats ();
      Printf.printf "\n Analyzes run on %i files.\n" !cpt_files
    end
  else
    if !analyze_flag
    then run_analyze_file !file
    else
      if !type_flag
      then run_type_file !file
      else
        if !cfg_flag
        then ScilintConfig.print_config ()
        else
          if !cfg_file
        then ScilintConfig.print_files (ScilintConfig.read_config !file)













