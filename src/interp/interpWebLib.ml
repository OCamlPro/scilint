open InterpLib

let help_lines = [
  "SciWeb is a simple online interpreter for the Scilab language developed by OCamlPro. ";
  "It was developed as part of the Richelieu FUI R&D project, as an online demonstrator for OCaml parsers and interpreters for Scilab.";
"";
  "SciWeb operations are executed in your browser only, there are no interactions with any server,";
  "so the available operations are much more limited and slower  than the standard Scilab interpreter.";
  "All functions are implemented in OCaml, translated to Javascript.";
  "";
  "The following standard functions are available: argn, clear, cos, disp, error, eye, global, inttype,";
  "   list, mlist, null, poly, quit, sin, size, string, tan, tlist, type, typeof, zeros";
  "The following plotting functions are available: " ^ (String.concat ", " (List.sort compare [ "clf"; "plot"; "bar"; "xtitle"; "grid" ]));
  "";
  "Contact information:";
  "   OCamlPro SAS, Gif-sur-Yvette, France";
  "   Mail: <contact@ocamlpro.com>";
  "   Web: http://www.ocamlpro.com/";
  "";
  "SciWeb code is open-source and available: http://github.com/OCamlPro/scilint/";
  "";
  "Basic help:";
  " - Use the arrow buttons to create boxes above or below a box.";
  " - Click out of box that you just modified or hit tab to update the results.";
  " - Use the cross button to remove a box.";
]

let () =
  register_library (fun state lib ->
      register_function lib state "prompt" (string @-> string)
        (fun msg ->
           Js.Opt.case
             (Dom_html.window##prompt (Js.string msg, Js.string ""))
             (fun () -> "")
             (fun s -> Js.to_string s)) ;
      register_function lib state "alert" (string @-> void)
        (fun msg -> Dom_html.window##alert (Js.string msg)) ;
      let help l =
        Printf.printf "%s\n%!" (String.concat "\n" help_lines);
      in
      register_function lib state "help" (void @-> null) help
    );
