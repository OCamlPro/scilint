(*  OCamlPro Scilab Toolbox - OcSciLab
 *  Copyright (C) 2014 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabParserAst
open ScilintWarning
open ScilintOptions
open Printf

(** called by the main on each code source passed on th CLI *)
let treat_source state lib source =
  let parse () =
    match source with
    | File fn -> SelectedParser.parse_file fn
    | String (name, str) -> SelectedParser.parse_string name str
    | _ -> assert false
  in
  let ast =
    if !print_time then begin
      printf "Parsing %s ...%!" (string_of_source source) ;
      let t0 = Sys.time () in
      let ast = parse () in
      let t1 = Sys.time () in
      printf "\b\b\bdone in %gms.\n%!" ((t1 -. t0) *. 1000.) ;
      ast
    end else parse ()
  in
  let ast = List.fold_left (fun r (name, anal) -> anal r) ast !passes in
  if !print_ast then begin
    printf "Syntax tree:\n" ;
    Sexp.pretty_output stdout ast ;
    printf "\n"
  end ;
  if !pretty_print then begin
    printf "Pretty printed:\n" ;
    Pretty.pretty_output stdout ast ;
    printf "\n"
  end ;
  if !ScilintOptions.print_messages then begin
    let messages = collect_messages ast in
    output_messages !ScilintOptions.format messages stdout
  end ;
  if ast = [] then
    let w = (source, ((1, 0), (1, 0))),
            Unrecovered "nothing to do" in
    output_messages !ScilintOptions.format [ w ] stdout
  else
    Interp.interpret state lib ast

type step =
  { mutable phrase: string;
    mutable answer: string;
    mutable next: step option;
    mutable updated: bool }

let update_tty contents =
  Js.Opt.case (Dom_html.document##getElementById (Js.string "scilab-tty"))
    (fun () -> failwith "scilab-tty element not found")
    (fun tty ->
       let tty = (tty :> Dom.node Js.t) in
       let tty = Tyxml_js.Of_dom.of_node contents in
       M.replaceChildren tty contents)

module D = Tyxml_js.Html5
module M = Tyxml_js_manip.Manip

let rec render step =
  let state = InterpCore.State.init () in
  let lib = InterpCore.Dispatcher.create () in
  InterpLib.load_libraries state lib ;
  let buf = Buffer.create 100 in
  Sys_js.set_channel_flusher stdout (Buffer.add_string buf) ;
  Sys_js.set_channel_flusher stderr (Buffer.add_string buf) ;
  let rec update_results step nb =
    Buffer.clear buf ;
    treat_source state lib (String ("input-" ^ string_of_int nb, step.phrase)) ;
    step.answer <- Buffer.contents buf ;
    match step.next with
    | None ->
      if step.phrase <> "" then
        step.next <- Some { phrase = "" ; answer = "" ; next = None ; updated = false }
    | Some next -> update_results next (nb + 1) in
  let rec format_results step nb =
    let textarea = D.(textarea ~a:[ a_class [ "scilab-input" ] ] (pcdata step.phrase)) in
    M.Ev.onchange textarea (fun _ev -> step.phrase <- M.value textarea ; true) ;
    let result = D.([ p ~a:[ a_class [ "scilab-output" ] ] [ pcdata step.answer ] ]) in
    textarea @ results @ match step.next with
    | None -> []
    | Some next -> format_results next (nb + 1) in
  update_results step 1 ;
  let run_button = D.(button [ entity "#9881" ]) in
  M.Ev.onclick run_button (fun _ev -> render step ; true) ;
  let contents = D.(h1 [ pcdata "Scilob" ; run_button ]) :: format_results step 1 in
  update_tty (D.div contents)

(** where the args are passed and all the fun starts *)
let main () =
  let open  Lwt in
  Lwt_js_events.onload () >>= fun _ ->
  let step = { phrase = "m = [ 3 4 ; 5 6 ] * 3" ;
               answer = "" ; next = None ; updated = false } in
  render step ;
  Lwt.return ()

let () =
  Lwt.async (fun () -> main ())
