open ScilabAst

let print_header () =
  Printf.printf "<analysis>\n";
  Printf.printf "<metadata><generator name=\"scilint\" version=\"0.2\"/></metadata>\n";
  Printf.printf "<results>\n\n"

let warning_to_firehose code locs =
  let buf = Buffer.create 1024 in
  let ((file, loc), msg) = List.hd locs in
  Printf.bprintf buf "<issue test-id=\"W%03d\">\n" code;
  Printf.bprintf buf "<message>%s</message>\n" msg;
  Printf.bprintf buf "<location>\n";
  Printf.bprintf buf "<file given-path=\"%s\"></file>\n" file;
  Printf.bprintf buf "<range>\n";
  Printf.bprintf buf "<point line=\"%i\" column=\"%i\"/>\n" loc.first_line loc.first_column;
  Printf.bprintf buf "<point line=\"%i\" column=\"%i\"/>\n" loc.last_line loc.last_column;
  Printf.bprintf buf "</range>\n</location>\n</issue>\n";
  Buffer.contents buf

let print_trailer () = 
  Printf.printf "\n</results>\n</analysis>"


