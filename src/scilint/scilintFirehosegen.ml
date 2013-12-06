open ScilabAst

let print_header () =
  Printf.printf "<analysis>\n";
  Printf.printf "<metadata><generator name=\"scilint\" version=\"0.2\"/></metadata>\n";
  Printf.printf "<results>\n\n"

let warning_to_firehose code locs =
  let buf = Buffer.create 1024 in
  Printf.bprintf buf "<issue test-id=\"W%03d\">\n" code;
  List.iter (fun ((file, loc), msg) ->
    Printf.bprintf buf "<message>%s</message>\n" msg;
    Buffer.add_string buf "<location>\n";
    Printf.bprintf buf "<file given-path=\"%s\"></file>\n" file;
    Buffer.add_string buf "<range>\n";
    Printf.bprintf buf "<point line=\"%i\" column=\"%i\"/>\n" loc.first_line loc.first_column;
    Printf.bprintf buf "<point line=\"%i\" column=\"%i\"/>\n" loc.last_line loc.last_column;
    Buffer.add_string buf "</range>\n</location>\n";
  ) locs;
  Buffer.add_string buf "</issue>\n";
  Buffer.contents buf

let print_trailer () =
  Printf.printf "\n</results>\n</analysis>"


