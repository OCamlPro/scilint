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
open InterpMessages

module D = Tyxml_js.Html5
module M = Tyxml_js_manip.Manip
module A = Archimedes

type 'a step =
  { mutable phrase: string;
    mutable answer: string;
    mutable next: 'a step option;
    mutable updated: bool;
    mutable liste: (*Html5_types.div*) 'a D.elt list; }

let empty_session () =
  { phrase = "" ; answer = "" ; next = None ; updated = false ; liste = [] }

class type xmlSerializer = object
  method serializeToString : Dom_html.element Js.t -> Js.js_string Js.t Js.meth
end

let xmlSerializer  = Js.Unsafe.global ## _XMLSERIALIZER

let win = Js.Unsafe.global ## _WINDOW

class type anchorElement = object
  inherit Dom_html.anchorElement
  method download : Js.js_string Js.t Js.prop
end

let coerceanchor : Dom_html.anchorElement Js.t -> anchorElement Js.t = Js.Unsafe.coerce
let coerceback : anchorElement Js.t -> Dom_html.anchorElement Js.t  = Js.Unsafe.coerce
let coerceCanvasToNode : Dom_html.canvasElement Js.t -> Dom.node Js.t = Js.Unsafe.coerce
let coerceEltToNode : Dom_html.element Js.t -> Dom.node Js.t = Js.Unsafe.coerce
let coerceEltToDiv : Dom.node Js.t -> Dom_html.divElement Js.t = Js.Unsafe.coerce

let archimedes_plot =
  let plot_count = ref 0 in
  fun plots ->
    let nb = !plot_count in
    incr plot_count ;
    let open InterpPlotLib in
    let w = 500. and h = 300. in
    let open InterpLib in
    let draw canv plots vp =
      let rec draw_all canvas plots vp =
        match plots with
        | [] -> canvas
        | hd :: tl ->
          (* A.set_line_width vp 1.; *)
          A.Viewport.set_color vp (A.Color.rgb 0. 0. 0.);
          begin match hd.xlabel with
            | None -> ()
            | Some lab ->  A.Viewport.xlabel vp lab
          end ;
          begin match hd.ylabel with
            | None -> ()
            | Some lab ->  A.Viewport.ylabel vp lab
          end ;
          begin match hd.title with
            | None -> ()
            | Some t ->  A.Viewport.title vp t
          end ;
          let xtics = A.Tics.(Equidistants ((Number 5), 0., 1., 0)) in
          let ytics = A.Tics.(Equidistants ((Number 5), 0., 1., 0)) in
          A.Axes.y ~grid:hd.grid ~tics:ytics ~offset:(A.Axes.Relative 0.) vp;
          A.Axes.x ~grid:hd.grid ~tics:xtics ~offset:(A.Axes.Relative 0.) vp;
          A.Axes.y ~grid:false ~tics:ytics ~offset:(A.Axes.Relative 0.) vp;
          A.Viewport.set_color vp A.Color.blue;
          A.Array.xy  ~style:hd.style vp (Array.of_list hd.xvalues) (Array.of_list hd.yvalues);
          draw_all canvas tl vp in
      draw_all canv plots.plots vp;
      if (A.Viewport.xmin vp) > 0. then A.xrange vp 0. (A.Viewport.xmax vp);
      if (A.Viewport.xmax vp) < 0. then A.xrange vp (A.Viewport.xmin vp) 0.;
      if (A.Viewport.ymin vp) > 0. then A.yrange vp 0. (A.Viewport.ymax vp);
      if (A.Viewport.ymax vp) < 0. then A.yrange vp (A.Viewport.ymin vp) 0.;
      A.close vp in
    let canvas_id = "plot_" ^ string_of_int nb in
    let canvas = D.(canvas ~a:[a_id canvas_id; a_width (int_of_float w); a_height (int_of_float h)] []) in
    M.appendToBody canvas ;
    let vp = A.init ~w ~h ["canvas"; canvas_id] in
    draw canvas plots vp ;
    canvas

let save_session step name =
  match Js.Optdef.to_option (Dom_html.window##localStorage) with
  | None -> ()
  | Some locStorage ->
    let res = ref (step.phrase) in
    let rec save next =
      match next with
      | None -> ()
      | Some s ->
        if s.phrase <>"" then res := !res ^ "\000" ^ s.phrase; save s.next;
    in save step.next;
    locStorage##setItem (Js.string name, Js.string !res)

let load_session name =
  match Js.Optdef.to_option (Dom_html.window##localStorage) with
  | None -> empty_session ()
  | Some locStorage ->
    let phrases =
      match Js.Opt.to_option locStorage##getItem (Js.string name) with
      | None ->  []
      | Some phrase -> Regexp.split (Regexp.regexp "\000") (Js.to_string phrase); in
    let rec init = function
      | [] -> None
      | phrase :: rest ->
        Some { (empty_session ()) with phrase ; next = init rest }  in
    match init phrases with
    | None -> empty_session ()
    | Some session -> session

let delete_session step name =
  match Js.Optdef.to_option (Dom_html.window##localStorage) with
  | None -> ()
  | Some locStorage ->
    match Js.Opt.to_option locStorage##getItem (Js.string name) with
    | None ->  ()
    | Some phrase -> locStorage##removeItem (Js.string name);
      step.phrase <- "m = [ 3 4 ; 5 6 ] * 3" ;
      step.answer <- "" ; step.next <- None ; step.updated <- false; step.liste <- []


let download_session step file_content =
  let str = Regexp.split (Regexp.regexp "\n") (Js.to_string file_content) in
  step.phrase <- List.hd str;
  step.next <- Some (empty_session ()) ;
  let str = List.tl str in
  let rec dl cstep l =
    match l with
    | [] -> ()
    | hd :: tl ->
      match hd with
      | "" -> ()
      | _ -> match cstep with
        | None -> ()
        | Some s -> s.phrase <- hd; (match s.next with
            | None -> s.next <- Some (empty_session ())
            | Some next -> ());
          dl s.next tl
  in dl step.next str


let current_session = ref ""
exception No_input_elt;;

class type blob = object
end

let blob : (Js.js_string Js.t Js.js_array Js.t -> blob Js.t) Js.constr =
  Js.Unsafe.global ## _Blob

class type url = object
  method createObjectURL : blob Js.t -> url Js.meth
end

let url  = Js.Unsafe.global ## _URL


let ignore_int (x : int) = ()

let session_to_array step =
  let array = jsnew Js.array_empty () in
  ignore_int (array##push( Js.string step.phrase));
  ignore_int (array##push(Js.string "\n"));
  let rec push_rest cstep =
    match cstep with
    | None -> ()
    | Some n ->
      ignore_int (array##push(Js.string n.phrase));
      ignore_int (array##push(Js.string "\n"));
      push_rest n.next;
  in push_rest step.next;
  array


let format_value v =
  let open InterpCore.Values in
  let matrix_to_table m ty =
    let w, h = matrix_size m in
    let tds = ref [] and trs = ref [] in
    for i = 1 to h do
      for j = 1 to w do
        let v = matrix_get m j i in
        let t = InterpMessages.string_of_value (repr (V (Single ty, v))) in
        tds := D.td [ D.pcdata (if j < w then t ^ "," else t) ] :: !tds
      done ;
      trs := D.(tr (List.rev !tds)) :: !trs ;
      tds := []
    done;
    D.(table ~a:[a_class ["scilab-braces" ; "scilab-value"] ])
      (List.rev !trs) in
  let rec format_vlist l =
    let res = ref [] in
    for i = 1 to vlist_length l do
      if i > 1 then res := D.pcdata "," :: !res ;
      res := format_value (vlist_get l i) :: !res
    done;
    D.(div ~a:[ a_class [ "scilab-value" ] ])
      [ D.pcdata "list" ;
        D.(div ~a:[a_class ["scilab-braces"]]) (List.rev !res) ]
  and format_tlist l =
    D.(div ~a:[ a_class [ "scilab-value" ] ])
      [ D.pcdata "tlist" ;
        D.(ul ~a:[ a_class [ "scilab-braces" ] ])
          (D.(li [a D.([pcdata "type = " ; pcdata (tlist_label l)])]) ::
           List.mapi
             (fun i n ->
                let value = format_value (tlist_get l n) in
                D.(li [ pcdata n ; pcdata " = " ; value ]))
             (tlist_fields l)) ]
  and format_value v =
    match view v with
    | V (Matrix ty, m) -> matrix_to_table m ty
    | V (Vlist, l) -> format_vlist l
    | V (Tlist(lab), l) -> format_tlist l
    | _ ->
      D.(span ~a:[ a_class [ "scilab-value" ] ])
        [ D.pcdata (InterpMessages.string_of_value v) ] in
  format_value v

let rec render ?(eval = true) step =
  let state = InterpCore.State.init () in
  let lib = InterpCore.Dispatcher.create () in
  InterpLib.load_libraries state lib ;
  ScilintOptions.format := Emacs;
  let buf = Buffer.create 100 in
  Sys_js.set_channel_flusher stdout (Buffer.add_string buf) ;
  Sys_js.set_channel_flusher stderr (Buffer.add_string buf) ;

  let rec update_results cstep nb =
    let messages = ref [] in
    InterpMessages.message_hook := (fun msg -> messages := msg :: !messages) ;
    Buffer.clear buf ;
    Interp.treat_source state lib (String ("box-" ^ string_of_int nb, cstep.phrase)) ;
    let formatted = ref [] in
    let seen_results = Hashtbl.create 100 in
    List.iter
      (fun msg ->
         let msgs = InterpMessages.format (Nowhere, ((0, 0),(0, 0))) msg in
         let msgs = ScilintWarning.string_of_messages !ScilintOptions.format msgs in
         let rec skip_locs msg =
           match msg with
           | Located (_, m) -> skip_locs m
           | Result (s, v) ->
             if not (Hashtbl.mem seen_results s) then begin
               Hashtbl.add seen_results s () ;
               let data = D.[ pcdata s ; pcdata " = " ; format_value v] in
               formatted := D.(div ~a:[ a_class [ "scilab-result" ] ]) data :: !formatted
             end
           | Hint (x) ->
             formatted := D.(div ~a:[ a_class [ "scilab-hint" ] ]) D.[ pcdata msgs ] :: !formatted
           | Warning (w) ->
             formatted := D.(div ~a:[ a_class [ "scilab-warning" ] ]) D.[ pcdata msgs ] :: !formatted
           | _ ->
             formatted := D.(div ~a:[ a_class [ "scilab-error" ] ]) D.[ pcdata msgs ] :: !formatted in
         skip_locs msg)
      !messages ;
    if InterpPlotLib.all_plots.InterpPlotLib.updated = true then begin
      cstep.liste <- archimedes_plot InterpPlotLib.all_plots :: !formatted ;
      InterpPlotLib.all_plots.InterpPlotLib.updated <- false
    end else begin
      cstep.liste <- !formatted
    end ;
    cstep.answer <- Buffer.contents buf ;
    cstep.updated <- true ;
    (match cstep.next with
     | None -> ()
     | Some next -> update_results next (nb + 1)) in

  let rec format_result cstep nb invalidated =
    let invalidated = invalidated || cstep.updated in
    let code_input = D.(textarea ~a:[ a_class [ "scilab-input" ] ] (pcdata cstep.phrase)) in
    M.Ev.onchange_textarea code_input (fun _ev ->
        cstep.phrase <- M.value code_input ;
        cstep.updated <- true ;
        render ~eval: true step ;
        true) ;
    let close_button =
      D.(button [ entity "#10005" ; span ~a:[a_class ["tooltip"]] [ D.pcdata "Delete this box." ] ]) in
    let insert_before_button =
      D.(button [ entity "#8648" ; span ~a:[a_class ["tooltip"]] [ D.pcdata "Insert a new box before this one." ] ]) in
    let insert_after_button =
      D.(button [ entity "#8650" ; span ~a:[a_class ["tooltip"]] [ D.pcdata "Insert a new box after this one." ] ]) in
    M.Ev.onclick close_button (fun _ev ->
        let step =
          if step == cstep then match step.next with
            | Some nstep -> nstep
            | None -> (empty_session ())
          else
            let rec del pstep = match pstep.next with
              | None -> step
              | Some nstep when nstep == cstep ->
                pstep.next <- nstep.next ;
                step
              | Some nstep -> del nstep in
            del step in
        render ~eval: true step ;
        true) ;
    M.Ev.onclick insert_before_button (fun _ev ->
        let rec ins step =
          match step.next with
          | None -> step
          | Some nstep when nstep == cstep ->
            { step with next = Some { (empty_session ()) with next = Some nstep } }
          | Some nstep -> { step with next = Some (ins nstep) } in
        let step =
          if cstep == step then
            { (empty_session ()) with next = Some step }
          else ins step in
        render ~eval: true step ;
        true) ;
    M.Ev.onclick insert_after_button (fun _ev ->
        let rec ins step =
          if step == cstep then
            { step with next = Some { (empty_session ()) with next = step.next } }
          else
            match step.next with
            | None -> step
            | Some nstep -> { step with next = Some (ins nstep) } in
        render ~eval: true (ins step) ;
        true) ;
    D.([div ~a:[ a_class [ "scilab-block" ] ]
          ([ D.(div ~a:[ a_class [ "buttons" ; "top" ]] [ insert_before_button ; close_button ]) ; code_input ] @
           cstep.liste @
           (if cstep.answer <> "" then
              D.([ p ~a:[ a_class [ "scilab-output" ]] [ pcdata cstep.answer ] ])
            else []) @
           [ D.(div ~a:[ a_class [ "buttons" ; "bottom" ]] [ insert_after_button ]) ])]) @
    match cstep.next with
    | None -> []
    | Some next -> format_result next (nb + 1) invalidated in

  if eval then begin
    InterpPlotLib.all_plots.InterpPlotLib.plots <- [] ;
    InterpPlotLib.all_plots.InterpPlotLib.updated <- false;
    update_results step 1
  end;

  let menu = D.(div ~a: [ a_class [ "scilab-menu" ]]) [] in
  let menu_opened = ref false in
  let hide_menu () =
    menu_opened := false ;
    M.removeClass menu "scilab-menu-open" in
  M.Ev.onclick menu (fun _ev -> hide_menu () ; true) ;
  let toolbar_button icon leg help cb =
    let button =
      D.(button
           ~a:[a_class ["scilab-toolbar-button"]]
           [ span ~a:[a_class ["icon"]] [ D.img ~src:icon ~alt:leg () ] ;
             span ~a:[a_class ["legend"]] [ D.pcdata leg ] ;
             span ~a:[a_class ["tooltip"]] [ D.pcdata help ] ]) in
    M.Ev.onclick button (fun _ev -> cb () ; true) ;
    button in
  let toolbar_menu_button icon leg help ctns =
    toolbar_button icon leg help @@ fun () ->
    let ctns = ctns () in
    if ctns = [] then begin
      menu_opened := false ;
      M.removeClass menu "scilab-menu-open"
    end else begin
      menu_opened := true ;
      M.addClass menu "scilab-menu-open" ;
      let ctns = [ D.(div ~a: [ a_class [ "contents" ] ]) ctns ]  in
      M.replaceChildren menu ctns
    end in

  let dialog = D.(div ~a: [ a_class [ "scilab-dialog" ]]) [] in
  let dialog_opened = ref false in
  let hide_dialog () =
    dialog_opened := false ;
    M.removeClass dialog "scilab-dialog-open" in
  let open_dialog ?title ?buttons message =
    dialog_opened := true ;
    M.addClass dialog "scilab-dialog-open" ;
    let ctns =
      (match title with
       | Some title -> [ D.(div ~a: [ a_class [ "title" ] ]) title ]
       | None -> []) @
      [ D.(div ~a: [ a_class [ "message" ] ]) message ] @
      (match buttons with
       | Some buttons ->
         let buttons =
           List.map
             (fun (label, cb) ->
                let button = D.button label in
                M.Ev.onclick button (fun _ev -> cb () ; true) ;
                button)
             buttons in
         [ D.(div ~a: [ a_class [ "buttons" ] ]) buttons ]
       | None -> []) in
    M.replaceChildren dialog [ D.(div ~a: [ a_class [ "contents" ] ]) ctns ] in

  let guard_dialog message cb =
    open_dialog
      ~buttons: [ [ D.pcdata "Proceed" ], (fun () -> cb () ; hide_dialog ()) ;
                  [ D.pcdata "Cancel" ], hide_dialog ]
      [ D.pcdata message ] in

  let input_session_name =
    D.(input ~a:[a_class ["input-session-name"] ; a_placeholder "enter a title"; a_value !current_session; a_size (12) ] ()) in

  let sessions_menu () =
    match Js.Optdef.to_option (Dom_html.window##localStorage) with
    | None -> D.[div [pcdata "No sessions saved."]]
    | Some locStorage ->
      let ul = D.(ul ~a:[a_class ["scilab-menu-sessions"]] []) in
      let current = ref None in
      let unselect () =
        match !current with
        | None -> ()
        | Some li ->
          M.removeClass li "current" ;
          current := None in
      let select li =
        unselect () ;
        M.addClass li "current" ;
        current := Some li in
      for i = 0 to locStorage##length - 1 do
        match Js.Opt.to_option (locStorage##key(i)) with
        | None -> ()
        | Some key ->
          let button_del =
            D.(button
                 [ entity "#10005" ;
                   span ~a:[a_class ["tooltip"]] [ D.pcdata "Delete this session." ]]) in
          let name = D.span [ D.pcdata (Js.to_string key) ] in
          let li = D.(li [ name ; button_del ] ) in
          if Js.to_string (Js.Unsafe.coerce (D.toelt input_session_name))##value = Js.to_string key then begin
            select li
          end ;
          M.Ev.onclick button_del (fun _ev ->
              guard_dialog ("Are you sure you want to delete session "^(Js.to_string key)^" ?")
                (fun () ->
                   delete_session step (Js.to_string key) ;
                   select li ; unselect () ;
                   M.removeChild ul li) ;
              true) ;
          M.Ev.onclick name
            (fun _ev ->
               let step = load_session (Js.to_string key) in
               current_session := (Js.to_string key);
               (Js.Unsafe.coerce @@ (D.toelt input_session_name))##value <- key;
               select li ;
               render ~eval:true step;
               true);
          M.appendChild ul li
      done ;
      [ ul ] in

  let load_button step =
    toolbar_menu_button "load.svg" "LOAD" "Open the session menu." sessions_menu in

  let save_button =
    toolbar_menu_button "save.svg" "SAVE" "Save the current session." @@ fun () ->
    let name = M.value input_session_name in
    match name with
    | "" ->
      Dom_html.window##alert(Js.string "Please pick a name for your current session before saving.") ;
      []
    | name ->
      save_session step name ;
      sessions_menu () in

  let save_file_button =
    let link = Dom_html.createA(Dom_html.document) in
    let tyxlink = Tyxml_js.Of_dom.of_anchor(link) in
    let button = toolbar_button "export.svg" "DOWNLOAD" "Download the current session as a .sci file." @@ fun () ->
      link##onclick <- Dom_html.handler (fun e -> Js.bool ((fun _ev ->
          let var = jsnew blob ( session_to_array step) in
          let url =  url##createObjectURL(var) in
          let link = coerceanchor(link) in
          link##href <- url;
          link##download <-
            (let name = M.value input_session_name in match name with
              | "" -> Js.string "sciweb-file.sci"
              | x -> Js.string (match Regexp.string_match (Regexp.regexp ".*.sci") x 0 with
                  | None -> name^".sci"
                  | _ -> x));
          Js.Unsafe.meth_call link "click" [||]; true) e)) ;
      Js.Unsafe.meth_call link "click" [||] in
    M.appendChild button tyxlink ;
    button in

  let load_file_button =
    let input_files_load =
      D.(input ~a:[(D.(a_input_type `File)); a_id "files"; a_name "files[]"; a_style "display:none"] ()) in
    M.Ev.onchange input_files_load (fun _ev ->
        match Js.Opt.to_option _ev##target with
        | None ->  raise No_input_elt
        | Some t -> match Js.Opt.to_option (Dom_html.CoerceTo.input(t)) with
          | None -> raise No_input_elt
          | Some inp -> match Js.Optdef.to_option inp##files with
            | None -> raise No_input_elt
            | Some files ->
              match Js.Opt.to_option files##item(0) with
              | None -> raise No_input_elt
              | Some file ->
                current_session := Js.to_string file##name;
                let fileReader = jsnew File.fileReader() in

                let f _ev = match Js.Opt.to_option _ev##target with
                  | None -> false
                  | Some t -> match Js.Opt.to_option (File.CoerceTo.string(t##result)) with
                    | None -> false
                    | Some s -> download_session step s ; render ~eval:false step; true in
                fileReader##onload <- Dom.handler (fun e -> Js.bool (f e)) ;
                fileReader##readAsText(file); true);
    toolbar_button "import.svg" "IMPORT" "Load a .sci file from your computer." @@ fun () ->
    Js.Unsafe.meth_call input_files_load "click" [||] in

  let clear_button =
    toolbar_button "clear.svg" "CLEAR" "Open a fresh session." @@ fun () ->
    guard_dialog ("Are you sure you want to erase the page ?") @@ fun () ->
    render (empty_session ()) in

  let buttons =
    D.(div ~a:[a_class [ "buttons" ]])
      [save_button; load_button step; save_file_button; load_file_button; clear_button] in
  let title =
    D.(div ~a:[a_class [ "title" ]])
      [ D.h1 [ D.pcdata "SciWeb -" ] ;
        input_session_name] in

  let contents = format_result step 1 false in
  let toolbar =
    D.(div ~a: [ a_class [ "scilab-toolbar" ]])
      [ title ; buttons ] in
  let tty =
    D.(div ~a: [ a_class [ "scilab-tty" ]])
      contents in

  M.replaceChildren
    (Tyxml_js.Of_dom.of_element Dom_html.document##body)
    [ toolbar ; tty ; menu ; dialog ]

(** where the args are passed and all the fun starts *)
let main () =
  let open  Lwt in
  Lwt_js_events.onload () >>= fun _ ->
  let phr3 =
    { phrase = "dom = (-10:10)/4\nplot(dom, sin(dom))\nplot(dom, dom)" ;
      answer = "" ; next = None ; updated = false; liste=[] } in
  let phr2 =
    { phrase = "m = 1 : 2 : 8\nr = m' * m" ;
      answer = "" ; next = Some phr3 ; updated = false; liste=[] } in
  render
    { phrase = "help ()" ; answer = "" ;
      next = Some phr2 ; updated = false; liste=[] } ;
  Lwt.return ()



let () =
  Lwt.async (fun () -> main ())
