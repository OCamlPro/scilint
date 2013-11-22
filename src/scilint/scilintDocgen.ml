
open ScilintManual

let warnings = List.map (fun
    (warnings_title, warnings_info, warnings) ->
    (warnings_title, warnings_info,
     List.map (fun
       (number, name, version, info) ->
       (number, name, String.concat "." (List.map string_of_int version),
        info)
     ) warnings)
  ) warnings

let rec latex_of_format doc =
  match doc with
  | PAR docs ->
    Printf.sprintf
      "\n\n%s\n\n" (latex_of_formats docs)
  | S doc -> doc
  | URL url ->
    Printf.sprintf "\\url{%s}" url
  | CODE code ->
    Printf.sprintf "\\begin{verbatim}\n%s\n\\end{verbatim}\n" code

and latex_of_formats docs =
String.concat ""
        (List.map latex_of_format docs)

let rec html_of_format doc =
  match doc with
  | PAR docs ->
    Printf.sprintf
      "<p>%s</p>\n" (html_of_formats docs)
  | S doc -> doc
  | URL url ->
    Printf.sprintf "<a href=\"%s\">%s</a>" url url
  | CODE code ->
    Printf.sprintf "<pre>%s</pre>\n" code

and html_of_formats docs =
  String.concat ""
    (List.map html_of_format docs)

let warnings_to_latex filename =
  let oc = open_out filename in

  Printf.fprintf oc "\\section{Warning Tables}\n\n";

  List.iter (fun (warnings_title, warnings_info, warnings) ->

    Printf.fprintf oc "\\subsection{%s}\n" warnings_title;
    Printf.fprintf oc "\n%s\n\n" (latex_of_formats warnings_info);

    Printf.fprintf oc "\\noindent\\\\\\begin{tabular}{|c|p{8cm}|c|} \\hline\n";
    Printf.fprintf oc "Identifier & Title & Implemented      \\\\ \\hline\n";

    List.iter (fun (number, name, version, info) ->
      Printf.fprintf oc "W%03d & %s & %s \\\\ \\hline\n"
        number name version
    ) warnings;
    Printf.fprintf oc "\\end{tabular}\n\n";
  ) warnings;

  List.iter (fun (warnings_title, warnings_info, warnings) ->

    Printf.fprintf oc "\\section{%s}\n" warnings_title;
    Printf.fprintf oc "\n%s\n\n" (latex_of_formats warnings_info);

    List.iter (fun (number, name, version, info) ->
      Printf.fprintf oc "\\subsection{W%03d --- %s}\n\n" number name;
      Printf.fprintf oc "\n%s\n\n" (latex_of_formats info);
    ) warnings;

  ) warnings;


  close_out oc;
  ()


type html_header = {
  description : string;
  title : string;
}

let html_header t =
  Printf.sprintf
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
<head>
  <meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
  <link rel=\"stylesheet\" href=\"css/bootstrap.min.css\" />
  <link rel=\"stylesheet\" href=\"css/main.css\" />
  <meta name=\"description\"
  content=\"%s\" />
  <meta name=\"keywords\"
  content=\"scilint,scilab,code,analysis,bugs,detection,open-source,free-software,native\" />
   <title>%s</title>
</head>
<body>
 <div class=\"navbar navbar-inverse navbar-fixed-top\">
   <div class=\"navbar-inner\">
     <div class=\"container\">
       <a class=\"brand\" href=\"http://www.ocamlpro.com/\">OCamlPro</a>
       <a class=\"brand\" href=\"http://www.inria.fr/\">INRIA</a>
       <ul class=\"nav pull-right\">
         <li><a href=\"index.html\">Scilint</a></li>
         <li><a href=\"changelog.html\">ChangeLog</a></li>
         <li><a href=\"warnings.html\">Warnings Doc</a></li>
       </ul>
    </div>
  </div>
</div>
<div class=\"content\">
<div class=\"container\">
<h1>%s</h1>
<div class=\"row\">
<div class=\"span12\">
"
  t.description t.title t.title

let html_trailer =
"</div>
</div>
</div>
</div>
</body>
</html>
"

let warnings_header = {
  description = "The documentation of all the warnings that Scilint may display on Scilab code";
  title = "Scilint Warnings Documentation";
}

let warnings_to_html filename =
  let oc = open_out filename in

  Printf.fprintf oc "%s\n" (html_header warnings_header);

  Printf.fprintf oc "<h2>Warning Tables</h2>";

  List.iter (fun (warnings_title, warnings_info, warnings) ->

    Printf.fprintf oc "<h3>%s</h3>\n" warnings_title;
    Printf.fprintf oc "%s\n" (html_of_formats warnings_info);

    Printf.fprintf oc "<table class=\"wtable\">";
    Printf.fprintf oc "<tr><th class=\"id\">Identifier</th><th class=\"title\">Title</th><th class=\"version\">Implemented</th></tr>\n";

    List.iter (fun (number, name, version, info) ->
      if version <> "" then begin
        Printf.fprintf oc "<tr><td class=\"id\"><a href=\"#W%003d\">W%03d</a></td>"
          number number;
        Printf.fprintf oc "<td class=\"title\">%s<td class=\"version\">%s</td></tr>\n"
          name version
      end
    ) warnings;
    Printf.fprintf oc "</table>\n";
  ) warnings;

  List.iter (fun (warnings_title, warnings_info, warnings) ->

    Printf.fprintf oc "<h2>%s</h2>\n" warnings_title;
    Printf.fprintf oc "%s\n" (html_of_formats warnings_info);

    List.iter (fun (number, name, version, info) ->
      if version <> "" then begin
        Printf.fprintf oc "<a name=\"W%03d\"> </a>" number;
        Printf.fprintf oc "<h3>W%03d --- %s</h3>\n" number name;
        Printf.fprintf oc "%s\n" (html_of_formats info);
      end
    ) warnings;

  ) warnings;

  Printf.fprintf oc "%s\n" (html_trailer);

  close_out oc;
  ()

let changelog_to_text filename =
  let oc = open_out filename in
  List.iter (fun (version, date, sections) ->
    Printf.fprintf oc "Version %s, %s:\n" version date;
    List.iter (fun (section, items) ->
      Printf.fprintf oc "   %s\n" section;
      List.iter (fun item ->
        Printf.fprintf oc "      * %s\n" item
      ) items;
    ) sections;
    Printf.fprintf oc "\n\n";
  ) (List.rev changelog);

  Printf.fprintf oc "Warning: do not modify this file !\n";
  Printf.fprintf oc "  This file was generated by scilintDocGen with -changelog-txt\n";
  Printf.fprintf oc "  Changes should be done in src/scilint/scilintManual.ml\n";
  close_out oc

let changelog_header = {
  description = "The changes for each Scilint version";
  title = "Scilint Change Log";
}

let changelog_to_html filename =
  let oc = open_out filename in
  Printf.fprintf oc "%s\n" (html_header changelog_header);
  List.iter (fun (version, date, sections) ->
    Printf.fprintf oc "<h2>Version %s, %s:</h2>\n<ul>" version date;
    List.iter (fun (section, items) ->
      Printf.fprintf oc "<li>%s\n<ul>" section;
      List.iter (fun item ->
        Printf.fprintf oc "<li>%s</li>\n" item
      ) items;
      Printf.fprintf oc "</ul></li>\n";
    ) sections;
      Printf.fprintf oc "</ul>\n";
  ) (List.rev changelog);

  Printf.fprintf oc "%s\n" (html_trailer);
  close_out oc


let _ =
  Arg.parse [
    "-warnings-latex", Arg.String warnings_to_latex,
    "FILENAME Generate LaTeX manual";
    "-warnings-html", Arg.String warnings_to_html,
    "FILENAME Generate LaTeX manual";
    "-changelog-txt", Arg.String changelog_to_text,
    "FILENAME Generate ChangeLog in text";
    "-changelog-html", Arg.String changelog_to_html,
    "FILENAME Generate ChangeLog in text";
  ] (fun _ -> exit 2) ""
