open InterpLib

let help_lines = [
  "SciWeb is a simple online interpreter for the Scilab language developed by OCamlPro. ";
  "It was developed as part of the Richelieu FUI R&D project, as an online demonstrator for OCaml parsers and interpreters for Scilab.";
"";
  "SciWeb operations are executed in your browser only, there are no interactions with any server,";
  "so the available operations are much more limited and slower  than the standard Scilab interpreter.";
  "All functions are implemented in OCaml, translated to Javascript.";
  "";
  "The following standard functions are available: argn, clear, disp, error, execstr, eye, global, inttype,";
  "   list, mlist, null, poly, quit, size, string, tlist, type, typeof, zeros";
  "The following math functions are available: abs, binomial, binomial_coefficient, ceil, cos, cumsum, double, exp, factorial, floor, int, log,";
  " mean, mean_vector, median_vector, ones, quartiles, rand_float, rand_int, sin, sqrt, sum, tan, tirage_entier, tirage_real, variance.";
(*  ^ (String.concat ", " (List.sort compare [
    "cos"; "sin"; "tan"; "exp"; "int"; "log"; "sqrt"; "floor"; "abs"; "ceil";
    "mean"; "ones"; "factorial"; "binomial_coefficient"; "binomial";"sum"; "cumsum" ; "median_vector";"mean_vector"; "variance"; "quartiles"; "rand_int"; "rand_float"; "tirage_entier"; "tirage_real"; "double";
    ])); *)
  "The following plotting functions are available: bar, clf, grid, plot, xtitle";
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

open InterpCore.Values

let () =
  register_library (fun state lib ->

      (*--------------------- prompt ------------------------------------*)

    register_function lib state "prompt" (string @-> string)
        (fun msg ->
           Js.Opt.case
             (Dom_html.window##prompt (Js.string msg, Js.string ""))
             (fun () -> "")
             (fun s -> Js.to_string s)) ;
      register_function lib state "alert" (string @-> void)
        (fun msg -> Dom_html.window##alert (Js.string msg)) ;


      (*--------------------- help --------------------------------------*)

      let help l =
        Printf.printf "%s\n%!" (String.concat "\n" help_lines);
      in
      register_function lib state "help" (void @-> null) help;

      (*--------------------- lycee -------------------------------------*)

      let eval state lib name s =
        Interp.treat_source ~set_ans:false
          state lib (ScilabParserAst.String (name, s))
      in
      register_function lib state "execstr" (string @-> null) (fun content ->
        eval state lib content content
      );

      register_function lib state "execstr" (Arg (Matrix String) @-> null)
        (fun m ->
          let w, h = matrix_size m in
          let b = Buffer.create 100 in
          for i = 1 to h do
            for j = 1 to w do
              Buffer.add_string b (matrix_get m j i);
              Buffer.add_char b '\n'
            done;
          done;
          let s = Buffer.contents b in
          eval state lib s s
      );

      register_function lib state "lycee" (void @-> null) (fun filename ->

        let rec iter = function
          | OCamlRes.Res.File (filename, content) ->
            eval state lib filename content
          | OCamlRes.Res.Dir (dirname, content) ->
            List.iter iter content
          | OCamlRes.Res.Error _ -> ()
        in
        List.iter iter InterpLycee.root;
        ()
      );

    );
