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
    messages
      (List.map (function
           | (loc, ScilintWarning.Werror m) -> Located (loc, Werror m)
           | (loc, ScilintWarning.Unrecovered m)
           | (loc, ScilintWarning.Recovered m) -> Located (loc, Error m)
           | (loc, ScilintWarning.Generic (_, None, f)) ->
             Located (loc, Generic (Format.asprintf "%a" (fun ppf () -> f ppf) ()))
           | (loc, ScilintWarning.Generic (_, Some n, f)) ->
             Located (loc, Generic (Format.asprintf "%s: %a" n (fun ppf () -> f ppf) ()))
           | (loc, ScilintWarning.Warning m) -> Located (loc, Warning m)
           | (loc, ScilintWarning.Hint msg) -> Located (loc, Hint msg)
           | (loc, ScilintWarning.Drop) -> Located (loc, Generic "drop token")
           | (loc, ScilintWarning.Insert m) -> Located (loc, Generic ("insert token " ^ m))
           | (loc, ScilintWarning.Replace m) -> Located (loc, Generic ("replace token by " ^ m)))
          (collect_messages ast))
  end ;
  if ast = [] then
    let w = Located ((source, ((1, 0), (1, 0))), Error "nothing to do") in
    messages [ w ]
  else
    Interp.interpret state lib ast


module D = Tyxml_js.Html5
module M = Tyxml_js_manip.Manip

type 'a step =
  { mutable phrase: string;
    mutable answer: string;
    mutable next: 'a step option;
    mutable updated: bool;
    mutable liste: (*Html5_types.div*) 'a D.elt list; }


let update_tty contents =
  Js.Opt.case (Dom_html.document##getElementById (Js.string "scilab-tty"))
    (fun () -> failwith "scilab-tty element not found")
    (fun tty -> M.replaceChildren (Tyxml_js.Of_dom.of_element tty) contents)


type montype =
| Tab of float array
| Func of (float -> float);;


let plot2 x_values y_values =
  let canvas = Tyxml_js.To_dom.of_canvas ( D.(canvas ~a:[a_class ["scilab-output"] ; a_width 1000 ; a_height 400] [] ) ) in
  let context = canvas##getContext (Dom_html._2d_) in
  let h = float_of_int (canvas##height) and  w = float_of_int (canvas##width) in
  context##beginPath ();
  context##moveTo(0., h/.2.); context##lineTo(w, h/.2.);
  context##moveTo(w/.2., 0.); context##lineTo(w/.2., h);
  begin
    match y_values with
    | Func f ->
      let x_start = x_values.(0) and x_end = x_values.(Array.length x_values -1) 
      and y_start = 0. and y_end = 225. in
      let w_scale = w /. ( x_end -. x_start) in
      let h_scale = h /. ( y_end -. y_start) in
      let first = ref true in
      for i=0 to int_of_float w do
	let x = ((float_of_int i) /. w_scale) +. x_start in
	let y = (f(x) -. y_start )*. h_scale in
	let y2 = h -. y in  
	if !first=true then ( context##moveTo(float_of_int i, y2); first := false; )
	else 
	  context##lineTo(float_of_int i, y2);
      done;
    | Tab t ->
      let x_start = x_values.(0) and x_end = x_values.(Array.length x_values -1) 
      and y_start = t.(0) and y_end = t.(Array.length t -1) in
      let w_scale = w /. ( x_end -. x_start) in
      let h_scale = h /. ( y_end -. y_start) in
      let first = ref true in
      for i=0 to Array.length x_values -1 do
	let x = (x_values.(i) /. w_scale) -. x_start in
	let y = (t.(i) -. y_start) *. h_scale in
	let y2 = h -. y in  
	if !first=true then ( context##moveTo(w -. w/. (float_of_int(Array.length x_values)), y2); first := false; )
	else 
	  context##lineTo(x, y2);
      done;
  end;
  context##stroke ();
  context##closePath ();
  Tyxml_js.Of_dom.of_canvas (canvas)

let plot3 liste =
  let first = List.hd liste in
  let xval = Array.make (List.length first) 0. in
  let yval = Array.make (List.length first) 0. in
  let rec sep i = function
    | [] -> ()
    | (x,y) :: tl -> xval.(i) <- x ; yval.(i) <- y ; sep (i+1) tl
  in sep 0 first ; 
  plot2 xval (Tab(yval)) ;;
    
let matrix_to_list m =
  let open InterpCore.Values in
  let w, h = matrix_size m in
  let l = ref [] in
  for i=1 to w do
    for j=1 to h do
      l := (matrix_get m i j) :: !l
    done;
  done;
  !l

let plot points = 
  let kind = `Line in
  let bindto = D.( div ~a:[ a_class [ "scilab-output" ]] [ ] ) in
  let init = C3.Line.make ~kind:`XY () in
  let rec tmp d l cpt = match l with
    | [] -> d
    | hd :: tl ->
      (*let hd = [ 1., 1. ; 3., 3. ; 5., 5. ] in*)
      let d =  C3.Line.add ~segment:(C3.Segment.make ~kind ~points:hd  ~label:((C3.Segment.string_of_kind kind)^(string_of_int cpt)) ()) d in
      tmp d tl (cpt+1)
  in
  let res = tmp init points 1 in
  ignore (C3.Line.render_in_tyxml_elt ~bindto:bindto res) ;
  bindto
    
type attr = {
  minX: float;
  minY: float;
  maxX: float;
  maxY: float;
  unitsPerTick: float;
  (* constants *)
  axisColor: string;
  font: string;
  tickSize: float;
  (* relationships *)
  rangeX: float;
  rangeY: float;
  unitX: float;
  unitY: float;
  centerY : float;
  centerX : float;
  iteration : float;
  scaleX : float;
  scaleY : float;
}
   

    
let drawXAxis context width attr = 
  context##save();
  context##beginPath();
  context##moveTo(0., attr.centerY);
  context##lineTo(width, attr.centerY);
  context##strokeStyle <- Js.string attr.axisColor;
  context##lineWidth <- 2. ;
  context##stroke();

  (* draw tick marks *)
  let xPosIncrement = attr.unitsPerTick *. attr.unitX in
  context##font <- Js.string attr.font;
  context##textAlign <- Js.string "center";
  context##textBaseline <-Js.string "top";

  (* draw left tick marks *)
  let xPos = ref ( attr.centerX -. xPosIncrement ) in
  let unit = ref ( -1. *. attr.unitsPerTick ) in
  while !xPos > 0. do
    context##moveTo(!xPos, attr.centerY -. attr.tickSize /. 2.);
    context##lineTo(!xPos, attr.centerY +. attr.tickSize /. 2.);
    context##stroke();
    context##fillText(Js.string (string_of_float !unit), !xPos, attr.centerY +. attr.tickSize /. 2. +. 3.);
    unit := !unit -. attr.unitsPerTick;
    xPos := ceil (!xPos -. xPosIncrement);
  done;

  (* draw right tick marks *)
  let xPos = ref ( attr.centerX +. xPosIncrement ) in
  let unit = ref ( attr.unitsPerTick ) in
  while !xPos < width do
    context##moveTo(!xPos, attr.centerY -. attr.tickSize /. 2.);
    context##lineTo(!xPos, attr.centerY +. attr.tickSize /. 2.);
    context##stroke();
    context##fillText(Js.string (string_of_float !unit), !xPos, attr.centerY +. attr.tickSize /. 2. +. 3.);
    unit := !unit +. attr.unitsPerTick;
    xPos := ceil (!xPos +. xPosIncrement);
  done;
  context##restore()

let drawYAxis context height attr = 
  context##save();
  context##beginPath();
  context##moveTo(attr.centerX, 0.);
  context##lineTo(attr.centerX, height);
  context##strokeStyle <- Js.string attr.axisColor;
  context##lineWidth <- 2. ;
  context##stroke();

  (* draw tick marks *)
  let yPosIncrement = attr.unitsPerTick *. attr.unitY in
  context##font <- Js.string attr.font;
  context##textAlign <- Js.string "right";
  context##textBaseline <- Js.string "middle";

  (* draw left tick marks *)
  let yPos = ref ( attr.centerY -. yPosIncrement ) in
  let unit = ref ( attr.unitsPerTick ) in
  while !yPos > 0. do
    context##moveTo(attr.centerX -. attr.tickSize /. 2., !yPos);
    context##lineTo(attr.centerX +. attr.tickSize /. 2., !yPos);
    context##stroke();
    context##fillText(Js.string (string_of_float !unit), attr.centerX -. attr.tickSize /. 2. -. 3., !yPos);
    unit := !unit +. attr.unitsPerTick;
    yPos := ceil (!yPos -. yPosIncrement);
  done;

  (* draw right tick marks *)
  let yPos = ref ( attr.centerY +. yPosIncrement ) in
  let unit = ref ( -1. *. attr.unitsPerTick ) in
  while !yPos < height do
    context##moveTo(attr.centerX -. attr.tickSize /. 2., !yPos);
    context##lineTo(attr.centerX +. attr.tickSize /. 2., !yPos);
    context##stroke();
    context##fillText(Js.string (string_of_float !unit), attr.centerX -. attr.tickSize /. 2. -. 3., !yPos);
    unit := !unit -. attr.unitsPerTick;
    yPos := ceil (!yPos +. yPosIncrement);
  done;
  context##restore()
  
let init minX minY maxX maxY unitsPerTick iteration =
   let canvas = Tyxml_js.To_dom.of_canvas ( D.(canvas ~a:[a_class ["scilab-output"] ; a_width 1000 ; a_height 400] [] ) ) in
  let context = canvas##getContext (Dom_html._2d_) in
  let h = float_of_int (canvas##height) and  w = float_of_int (canvas##width) in
  let rangeX = maxX -. minX and rangeY = maxY -. minY in
  let myattr = { 
    minX = minX; maxX = maxX;
    minY = minY; maxY = maxY;
    unitsPerTick = unitsPerTick;
    axisColor = "#aaa";
    font = "8pt Calibri";
    tickSize = 15.;
    rangeX = rangeX;
    rangeY = rangeY;
    unitX = w /. rangeX;
    unitY = h /. rangeY;
    centerY = ceil (abs_float(minY /. rangeY) *. h);
    centerX = ceil (abs_float(minX /. rangeX) *. w);
    iteration = (*iteration;*) (maxX -. minX) /. 1000.;
    scaleX = w /. rangeX;
    scaleY = h /. rangeY; } in 
  drawXAxis context w myattr;
  drawYAxis context h myattr;
  (canvas, myattr)
 
let drawEquation canvas attr equation  =
  let context = canvas##getContext (Dom_html._2d_) in
  context##save();        
  context##translate(attr.centerX, attr.centerY);
  context##scale(attr.scaleX, -.attr.scaleY);
  context##beginPath();
  context##moveTo(attr.minX, equation(attr.minX));
  let x = ref ( attr.minX +. attr.iteration) in
  while !x <= attr.maxX do
    context##lineTo(!x, equation(!x));
    x := !x +. attr.iteration;
  done;
  context##restore();
  context##lineJoin <- Js.string "round";
  context##lineWidth <- 2.;
  context##strokeStyle <- Js.string "green";
  context##stroke();
  context##restore();
  Tyxml_js.Of_dom.of_canvas (canvas)

let plot_canvas xvalues y =
  let minX = xvalues.(0) and maxX = xvalues.(Array.length xvalues -1) in
  let minY = ref 0. and maxY = ref 0. in
  minY := infinity ; maxY := neg_infinity ;
  for i=0 to Array.length xvalues -1 do
    if !minY > y(float_of_int i) then
      minY := y(float_of_int i);
    if !maxY < y(float_of_int i) then
      maxY := y(float_of_int i);
  done;
  let initial, attr = init minX !minY maxX !maxY 1. 1. in
  let canvas = drawEquation initial attr y in
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
	if s.phrase <>"" then res := !res ^ "@" ^ s.phrase; save s.next;
    in save step.next;
    locStorage##setItem (Js.string name, Js.string !res)


let load_session step name =
  match Js.Optdef.to_option (Dom_html.window##localStorage) with
  | None -> ()
  | Some locStorage ->
    let liste =
      match Js.Opt.to_option locStorage##getItem (Js.string name) with
      | None ->  []
      | Some phrase -> Regexp.split (Regexp.regexp "@") (Js.to_string phrase);
    in
    step.phrase <- List.hd liste; 
    step.next <- Some {phrase = ""; answer = ""; next = None; updated = false; liste = []};
    let liste = List.tl liste in
    let rec show s l =
      match l with
      | [] -> ()
      | hd :: tl ->
	match s with
	| None -> ()
	| Some n -> n.phrase <- hd; 
	  n.next <- Some {phrase = ""; answer = ""; next = None; updated = false; liste = []};
	  show n.next tl
    in show step.next liste

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
  step.next <- Some {phrase = ""; answer = ""; next = None; updated = false; liste = []};
  let str = List.tl str in
  let rec dl cstep l =
    match l with
    | [] -> ()
    | hd :: tl -> match cstep with
      | None -> ()
      | Some s -> s.phrase <- hd; (match s.next with 
	| None -> s.next <- Some {phrase = ""; answer = ""; next = None; updated = false; liste = []};
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

class type anchorElement = object
  inherit Dom_html.anchorElement
  method download : Js.js_string Js.t Js.prop
end 

let coerceanchor : Dom_html.anchorElement Js.t -> anchorElement Js.t = Js.Unsafe.coerce
let coerceback : anchorElement Js.t -> Dom_html.anchorElement Js.t  = Js.Unsafe.coerce
let coerceToNode : Dom_html.anchorElement Js.t -> Dom.node Js.t = Js.Unsafe.coerce

let session_to_array step =
  let array = jsnew Js.array_empty () in
  array##push( Js.string step.phrase); array##push(Js.string "\n");
  let rec push_rest cstep =
    match cstep with
    | None -> ()
    | Some n -> array##push(Js.string n.phrase); array##push(Js.string "\n"); push_rest n.next;
  in push_rest step.next;
  array


let rec render ?(eval = true) step =
  let state = InterpCore.State.init () in
  let lib = InterpCore.Dispatcher.create () in
  InterpLib.load_libraries state lib ;
  ScilintOptions.format := Emacs;
  let buf = Buffer.create 100 in
  Sys_js.set_channel_flusher stdout (Buffer.add_string buf) ;
  Sys_js.set_channel_flusher stderr (Buffer.add_string buf) ;

  let rec update_results cstep nb =
    let liste = ref [] in
    Buffer.clear buf ;
    InterpMessages.message_hook :=
      (fun msg -> 
	let localBuf = Buffer.create 1000 in 
	let msgs = format (ScilabLocations.Nowhere, ((0, 0), (0, 0))) msg in
	Buffer.add_string localBuf (ScilintWarning.string_of_messages !ScilintOptions.format msgs);
	let myBuf = Buffer.create 1000 in
	let ppf = Format.formatter_of_buffer myBuf in
	let rec func msg =
	  match msg with
	  | Hint (x) -> ("scilab-hint", D.([ pcdata (Buffer.contents localBuf) ]))
	  | Result (s, v) -> 
	    begin
	      let open InterpCore.Values in
	      let matrix_to_table m f =
		let (nbRow, nbCol) = matrix_size m in
		let td_list = ref [] and tr_list = ref [] in
		for i=1 to nbRow do
		  for j=1 to nbCol do
		    td_list := !td_list @ [(D.td [ D.pcdata (f (matrix_get m i j)) ])] 
		  done;
		  tr_list := !tr_list @ [D.(tr !td_list)] ;
		  td_list := [];
		done;
		D.([table ~a:[a_class ["scilab-matrix"]] !tr_list]) in

	      let format_vlist l =
		let res = ref [] in
		for i=1 to vlist_length l do
		  (match view (vlist_get l i) with
		  | V (Matrix (Number Real), m) -> res := !res @ (matrix_to_table m (string_of_float))
		  | _ -> Buffer.clear myBuf ;
			 InterpMessages.print_value ppf (vlist_get l i) ;
			 Format.fprintf ppf "%!" ;
			 res := !res @ D.([ pcdata (Buffer.contents myBuf) ]));
		  if i <> vlist_length l then res := !res @ D.([ pcdata ", " ]);
		done;
	        D.([ pcdata "( " ]) @ !res @ D.([ pcdata " )"]) in
	      
	      let format_tlist l =
		let res = ref [] in
	     	res := D.([li [a D.([pcdata "type = " ; pcdata (tlist_label l)])]]);
		let tab = Array.of_list (tlist_fields l) in
		for ind=0 to Array.length tab -1 do
		  let valeur = match view (tlist_get l tab.(ind)) with
		    | V (Matrix (Number Real), m) -> (matrix_to_table m (string_of_float))
		    | _ -> Buffer.clear myBuf;
			   InterpMessages.print_value ppf (tlist_get l tab.(ind)) ;
			   Format.fprintf ppf "%!" ;
			   D.([ pcdata (Buffer.contents myBuf) ])
		  in 
		  res := !res @ D.([li [a (D.([pcdata tab.(ind) ; pcdata " = "] ) @ valeur) ] ]) ;
		done;
		D.([ul !res ])
	      in 

	      let formatted_result = match view v with
		| V (Matrix (Number Real), m) -> (matrix_to_table m (string_of_float))
		| V (Matrix (Int8), m) -> (matrix_to_table m (string_of_int))
		| V (Matrix (Int16), m) -> (matrix_to_table m (string_of_int))
		| V (Matrix (Int32), m) -> (matrix_to_table m (string_of_int))
		| V (Matrix (Uint8), m) -> (matrix_to_table m (string_of_int))
		| V (Matrix (Uint16), m) -> (matrix_to_table m (string_of_int))
		| V (Matrix (Uint32), m) -> (matrix_to_table m (string_of_int))
		| V (Matrix (Bool), m) -> (matrix_to_table m (string_of_bool))
		| V (Matrix (String), m) ->  (matrix_to_table m (fun x -> x))
		| V (Matrix (Number Complex), m) ->  (*[plot (matrix_to_list m)]*)
		  (matrix_to_table m (fun x -> 
		    Buffer.clear myBuf;
		    InterpMessages.print_value ppf (inject (Single (Number Complex)) x); 
		    Format.fprintf ppf "%!" ; 
		    Buffer.contents myBuf ; ))
		| V (Vlist, l) -> (format_vlist l)
		| V (Tlist(lab), l) -> (format_tlist l)
		| _ -> D.([ pcdata (Buffer.contents localBuf)]) in
	      ("scilab-result", D.([ pcdata s ; pcdata " = " ]) @ formatted_result)
	    end
	  | Warning (w) -> ("scilab-warning", D.([ pcdata (Buffer.contents localBuf)]))
	  | Located (loc, m) -> func m
	  | _ -> ("scilab-error", D.([ pcdata (Buffer.contents localBuf)]))
	in 
	let (cl, data ) = func msg in  
	let x_values = Array.make 10 0. in
	x_values.(9) <- 15.; x_values.(0) <- -15.;
	liste := D.([ div ~a:[ a_class [ cl ] ] data ])  @ !liste );
    treat_source state lib (String ("input-" ^ string_of_int nb, cstep.phrase)) ;
    cstep.answer <- Buffer.contents buf ;
    cstep.updated <- false ;
    cstep.liste <- !liste ;
    (*let xval = Array.make 10 0. in
    for i=0 to 9 do
      xval.(i) <- float_of_int i;
    done;
    cstep.liste <- !liste @ [ plot_canvas xval sin ] ;*)
    if InterpLib.plots.updated = true then 
      ( cstep.liste <- cstep.liste @ [ plot InterpLib.plots.liste ] ; InterpLib.plots.updated <- false ) ;
    (match cstep.next with 
      | None ->
      if cstep.phrase <> "" then
	cstep.next <- Some { phrase = "" ; answer = "" ; next = None ; updated = false; liste = [] };
      | Some next -> update_results next (nb+1) ; ); in

  let rec format_result cstep nb invalidated = 
    let invalidated = invalidated || cstep.updated in
    let textarea = D.(textarea ~a:[ a_class [ "scilab-input" ] ] (pcdata cstep.phrase)) in
    M.Ev.onchange_textarea textarea (fun _ev ->
      cstep.phrase <- M.value textarea ;
      cstep.updated <- true ;
      render ~eval:false step ;
      true) ;
    let invalidated_class =
      if invalidated then [ "scilab-invalidated" ] else [] in
    let results = D.([div ~a:[ a_class invalidated_class ] cstep.liste ]) in 
    textarea :: results @ match cstep.next with
    | None -> []
    | Some next -> format_result next (nb + 1) invalidated in
  
  if eval then ( InterpLib.plots.liste <- [] ; update_results step 1 ; ());
  let run_button = D.(button ~a:[a_class ["button-run"]] [ entity "#9881" ] ) in
  M.Ev.onclick run_button (fun _ev -> render step ; true) ;

  let input_session_name = D.(input ~a:[a_class ["input-session"] ; a_placeholder "untitled"; a_size (12) ] () ) in
  let save_button = D.(button ~a:[a_class ["button-save"]] [entity "#58532"])  in
  M.Ev.onclick save_button (fun _ev -> save_session step (M.value input_session_name); true);

  let list_sessions step =
  let res = ref [] in
  match Js.Optdef.to_option (Dom_html.window##localStorage) with
  | None -> D.(div [pcdata "no sessions"])
  | Some locStorage ->
    for i=0 to (locStorage##length -1) do
      match Js.Opt.to_option (locStorage##key(i)) with
      | None -> ()
      | Some key -> let bu = D.(button [entity "#10007"]) in
		    M.Ev.onclick bu (fun _ev -> delete_session step (Js.to_string key); true);
		    let b = D.(li ~a:[a_class ["scilab-sessions"]] [D.(button [pcdata (Js.to_string key)] ) ; bu ] ) in
		    M.Ev.onclick b (fun _ev -> load_session step (Js.to_string key); current_session := (Js.to_string key); render ~eval:false step; true); 
		    res := !res @ [b] 
    done;
   D.(div ~a:[a_tabindex (0) ; a_class [ "onclick-button"]] [ D.(ul ~a:[a_class ["onclick-list"; "scilab-sessions"] ] !res) ]) in

  let selected_file = ref [] in
  let input_files = D.(input ~a:[(D.(a_input_type `File)); a_id "files"; a_name "files[]"; a_style "diplay:none"] ()) in 
  M.Ev.onchange input_files (fun _ev -> 
    match Js.Opt.to_option _ev##target with
    | None ->  raise No_input_elt
    | Some t -> match Js.Opt.to_option (Dom_html.CoerceTo.input(t)) with
      | None -> raise No_input_elt
      | Some inp -> match Js.Optdef.to_option inp##files with
	| None -> raise No_input_elt
	| Some files -> 
	  match Js.Opt.to_option files##item(0) with
	  | None -> raise No_input_elt
	  | Some file -> selected_file := [file];
	    let var = jsnew blob ( session_to_array step) in
	    let url =  url##createObjectURL(var) in
	    let link = Dom_html.createA(Dom_html.document) in
	    Dom_html.document##body##appendChild (coerceToNode(link));
	    let link = coerceanchor(link) in
	    link##href <- url;
	    link##download <- (match !selected_file with
	    | [] -> Js.string "sciweb_file"
	    | hd :: _ -> hd##name);
	    Js.Unsafe.meth_call link "click" [||]; true);

  let input_files_load = D.(input ~a:[(D.(a_input_type `File)); a_id "files"; a_name "files[]"; a_style "diplay:none"] ()) in 
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
	  | Some file -> selected_file := [file];
	    let fileReader = jsnew File.fileReader() in 
	    
	    let f _ev = match Js.Opt.to_option _ev##target with
	      | None -> false
	      | Some t -> match Js.Opt.to_option (File.CoerceTo.string(t##result)) with
		| None -> false
		| Some s -> download_session step s ; render ~eval:false step; true in
	    fileReader##onload <- Dom.handler (fun e -> Js.bool (f e)) ;
	    fileReader##readAsText(List.hd !selected_file); true);
  
  let load_file_button = D.(button [pcdata "Open file"] (*entity "#58531"*)) in
  M.Ev.onclick load_file_button (fun _ev -> Js.Unsafe.meth_call input_files_load "click" [||]);
  let save_file_button = D.(button [(*entity "#10514"*) pcdata "Save as file" ]) in
  M.Ev.onclick save_file_button (fun _ev -> Js.Unsafe.meth_call input_files "click" [||]; true);
  
  let button_box = D.(div ~a:[a_class ["div-button"]] [run_button ; save_button; list_sessions step; save_file_button; load_file_button ]) in
  let contents = D.(h1 ~a:[a_style "display:inline"] [ pcdata "Sciweb  -  "]) :: [input_session_name] @ [button_box] @ format_result step 1 false in
  update_tty contents ;
  (* prevent C3.js bug *)
  Js.Unsafe.meth_call Dom_html.window "onresize" [||]

  


(** where the args are passed and all the fun starts *)
let main () =
  let open  Lwt in
  Lwt_js_events.onload () >>= fun _ ->
  let step = { phrase = "m = [ 3 4 ; 5 6 ] * 3" ;
             answer = "" ; next = None ; updated = false; liste=[] } in
  render step ;
  Lwt.return ()

let () =
  Lwt.async (fun () -> main ())



    
