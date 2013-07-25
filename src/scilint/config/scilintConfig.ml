
let read_config filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  ScilintParser.config ScilintLexer.token lexbuf

let print_config () =
  let list_ext = [ ".sci"; ".sce" ] in
  let rec iter fun_iter dirname list = 
    Array.iter (fun file ->
      try
        let file = Filename.concat dirname file in
        if Sys.is_directory file 
        then iter fun_iter file (Sys.readdir file);
        if List.exists (Filename.check_suffix file) list_ext 
        then fun_iter ("  " ^ file ^ "\n")
        (* printf.printf "%s \n" dirname *)
      with _ -> ()
    ) list in
  try
    let macro_path = "macros" in
    if Sys.is_directory macro_path then 
      begin
        let ch = open_out "project.scilint" in
        output_string ch "files = [\n";
        iter (output_string ch) macro_path (Sys.readdir macro_path);
        output_string ch "]"
      end
  with _ -> failwith "No macro dir found"
