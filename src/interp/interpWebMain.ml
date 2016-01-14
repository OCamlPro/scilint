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
module A = Archimedes

type 'a step =
  { mutable phrase: string;
    mutable answer: string;
    mutable next: 'a step option;
    mutable updated: bool;
    mutable liste: (*Html5_types.div*) 'a D.elt list; }

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
  let h, w = matrix_size m in
  let l = ref [] in
  for i=1 to h do
    for j=1 to w do
      l := (matrix_get m j i) :: !l
    done;
  done;
  !l

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

let archimedes_plot plots nb =
  let open InterpPlotLib in
  let w = Js.Opt.case (Dom_html.document##getElementById(Js.string "blabla"))
      (fun () -> failwith "no div blabla")
      (fun d -> Js.Opt.case (Dom_html.CoerceTo.div(d))
          (fun () -> failwith " coercion problem")
          (fun res -> ref (float_of_int res##clientWidth) )) in
  Firebug.console##log(Js.string @@ "div width = "^ (string_of_float !w));
  (* let w = ref (match Js.Optdef.to_option Dom_html.window##innerWidth with *)
  (* 	| None -> failwith "no height" *)
  (* 	| Some x -> float_of_int x (\*500.*\)) in *)
  let h = ref 500. in
  let canvas_id = "canvas"^(string_of_int nb) in
  Js.Opt.case (Dom_html.document##getElementById (Js.string canvas_id))
    (fun () -> ())
    (fun c ->     match Js.Opt.to_option c##parentNode with
       | None -> failwith "no parent node"
       | Some p -> p##removeChild(coerceEltToNode c); ());
  let canv = D.(canvas ~a:[a_id canvas_id; a_width (int_of_float (!w)); a_height (int_of_float !h)] []) in
  M.appendToBody canv;
  let vp = A.init ~w:(!w) ~h:(!h) ["canvas"; canvas_id] in
  let open InterpLib in
  let draw canv plots vp =
    let rec draw_all canvas plots vp =
      match plots with
      | [] -> canvas
      | hd :: tl ->
        (* A.set_line_width vp 1.; *)
	      A.Viewport.set_color vp (A.Color.rgb 0. 0. 0.);
	      (match hd.xlabel with
	       | Some lab ->  A.Viewport.xlabel vp lab);
	      (match hd.ylabel with
	       | Some lab ->  A.Viewport.ylabel vp lab);
	      (match hd.title with
	       | Some t ->  A.Viewport.title vp t);

	      let xtics = A.Tics.(Equidistants ((Number 5), 0., 1., 0)) in
	      let ytics = A.Tics.(Equidistants ((Number 5), 0., 1., 0)) in
	      A.Axes.y ~grid:hd.grid ~tics:ytics ~offset:(Relative 0.) vp;
	      A.Axes.x ~grid:hd.grid ~tics:xtics ~offset:(Relative 0.) vp;
	      A.Axes.y ~grid:false ~tics:ytics ~offset:(Relative 0.) vp;

	      A.Viewport.set_color vp A.Color.blue;
	      A.Array.xy  ~style:hd.style vp (Array.of_list hd.xvalues) (Array.of_list hd.yvalues);

	      draw_all canvas tl vp in
    draw_all canv plots.plots vp;
    if (A.Viewport.xmin vp) > 0. then A.xrange vp 0. (A.Viewport.xmax vp);
    if (A.Viewport.xmax vp) < 0. then A.xrange vp (A.Viewport.xmin vp) 0.;
    if (A.Viewport.ymin vp) > 0. then A.yrange vp 0. (A.Viewport.ymax vp);
    if (A.Viewport.ymax vp) < 0. then A.yrange vp (A.Viewport.ymin vp) 0.;
    A.close vp in
  draw canv plots vp;
  let cx = ref 0 and cy = ref 0 in

  M.Ev.onmousedown canv (fun _ev ->
      cx := _ev##clientX ; cy := _ev##clientY ; true
    );
  M.Ev.onmouseup canv (fun _ev ->
      let xx = _ev##clientX - !cx and yy = _ev##clientY - !cy in
      (Tyxml_js.To_dom.of_canvas(canv))##getContext(Dom_html._2d_)##clearRect(0.,0., !w, !h);
      (Tyxml_js.To_dom.of_canvas(canv))##getContext(Dom_html._2d_)##translate(float_of_int xx, float_of_int yy);
      let vp = A.init ~w:(!w) ~h:(!h) ["canvas"; canvas_id] in
      let open InterpLib in
      draw canv plots vp;  true);
  Dom_html.document##body##removeChild(coerceCanvasToNode(Tyxml_js.To_dom.of_canvas canv));

  let zoomin = D.(button [entity "#10133"]) in
  M.Ev.onclick zoomin (fun _ev ->
      (Tyxml_js.To_dom.of_canvas(canv))##getContext(Dom_html._2d_)##clearRect(0.,0., !w, !h);
      w := !w +. 100. ; h := !h +. 100.;
      let vp = A.init ~w:(!w) ~h:(!h) ["canvas"; canvas_id] in
      let open InterpLib in
      draw canv plots vp;  true);
  let zoomout = D.(button [entity "#10134"]) in
  M.Ev.onclick zoomout (fun _ev ->
      (Tyxml_js.To_dom.of_canvas(canv))##getContext(Dom_html._2d_)##clearRect(0.,0., !w, !h);
      w := !w -. 100. ; h := !h -. 100. ;
      let vp = A.init ~w:(!w) ~h:(!h) ["canvas"; canvas_id] in
      let open InterpLib in
      draw canv plots vp;  true);
  let f _ev =
    (Tyxml_js.To_dom.of_canvas(canv))##getContext(Dom_html._2d_)##clearRect(0.,0., !w, !h);
    w := Js.Opt.case (Dom_html.document##getElementById(Js.string "blabla"))
        (fun () -> failwith "no div blabla")
        (fun d -> Js.Opt.case (Dom_html.CoerceTo.div(d))
	          (fun () -> failwith " coercion problem")
	          (fun res -> float_of_int res##clientWidth));
    h := 500. ;
    let vp = A.init ~w:(!w) ~h:(!h) ["canvas"; canvas_id] in
    let open InterpLib in
    draw canv plots vp; true in
  Dom_html.window##onresize <- (Dom_html.handler (fun e -> Js.bool (f e)));
  D.(div [ canv ; zoomin ; zoomout ])


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
  (***********************)
  context##textAlign <- Js.string "start";
  context##fillText(Js.string "xlabeloijojojijojpmk", 10., attr.centerY -. 30.);

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
    axisColor = "#000";
    font = "8pt Sans-serif";
    tickSize = 12.;
    rangeX = rangeX;
    rangeY = rangeY;
    unitX = w /. rangeX;
    unitY = h /. rangeY;
    centerY = ceil (abs_float(maxY /. rangeY) *. h);
    centerX = ceil (abs_float(minX /. rangeX) *. w);
    iteration = (*iteration;*) (maxX -. minX) /. 1000.;
    scaleX = w /. rangeX;
    scaleY = h /. rangeY; } in
  drawXAxis context w myattr;
  drawYAxis context h myattr;
  (canvas, myattr)

let drawEquation canvas attr equation xvalues =
  let context = canvas##getContext (Dom_html._2d_) in
  context##save();
  context##translate(attr.centerX, attr.centerY);
  context##scale(attr.scaleX, -.attr.scaleY);
  context##beginPath();
  context##moveTo(attr.minX, equation(attr.minX));
  (*let x = ref ( attr.minX +. attr.iteration) in
    while !x <= attr.maxX do
    context##lineTo(!x, equation(!x));
    x := !x +. attr.iteration;
    done;*)
  for i=0 to Array.length xvalues -1 do
    context##lineTo(xvalues.(i), equation(xvalues.(i)));
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
  minY := y(xvalues.(0)) ; maxY := y(xvalues.(0)) ;
  for i=1 to Array.length xvalues -1 do
    if !minY > y(xvalues.(i)) then
      minY := y(xvalues.(i));
    if !maxY < y(xvalues.(i)) then
      maxY := y(xvalues.(i));
  done;
  let initial, attr = init minX !minY maxX !maxY 1. 1. in
  let canvas = drawEquation initial attr y xvalues in
  canvas

let drawEquation2 canvas attr yvalues xvalues =
  let context = canvas##getContext (Dom_html._2d_) in
  context##save();
  context##translate(attr.centerX, attr.centerY);
  context##scale(attr.scaleX, -.attr.scaleY);
  context##beginPath();
  context##moveTo(attr.minX, yvalues.(0));
  for i=0 to Array.length xvalues -1 do
    context##lineTo(xvalues.(i), yvalues.(i));
  done;
  context##restore();
  context##lineJoin <- Js.string "round";
  context##lineWidth <- 2.;
  context##strokeStyle <- Js.string "green";
  context##stroke();
  context##restore();
  Tyxml_js.Of_dom.of_canvas (canvas)

let plot_canvas2 xvalues y =
  let minX = xvalues.(0) and maxX = xvalues.(Array.length xvalues -1) in
  let minY = ref 0. and maxY = ref 0. in
  minY := y.(0) ; maxY := y.(0) ;
  for i=1 to Array.length xvalues -1 do
    if !minY > y.(i) then
      minY := y.(i);
    if !maxY < y.(i) then
      maxY := y.(i);
  done;
  let initial, attr = init minX !minY maxX !maxY 1. 1. in
  let canvas = drawEquation2 initial attr y xvalues in
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
    | hd :: tl ->
      match hd with
      | "" -> ()
      | _ -> match cstep with
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
		             let w, h = matrix_size m in
		             let td_list = ref [] and tr_list = ref [] in
		             for i=1 to h do
		               for j=1 to w do
		                 td_list := !td_list @ [(D.td [ D.pcdata (f (matrix_get m j i)) ])]
		               done;
		               tr_list := !tr_list @ [D.(tr !td_list)] ;
		               td_list := [];
		             done;
		             D.([table ~a:[a_class ["scilab-matrix"] ] !tr_list]) in

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
		             | V (Matrix (Number Complex), m) ->
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

    if InterpPlotLib.all_plots.updated = true then
      (cstep.liste <- cstep.liste @ [ archimedes_plot InterpPlotLib.all_plots nb ];
       InterpPlotLib.all_plots.updated <- false);
    (match cstep.next with
     | None -> ()
     | Some next -> update_results next (nb+1)) in

  let rec format_result cstep nb invalidated =
    let invalidated = invalidated || cstep.updated in
    let code_input = D.(textarea ~a:[ a_class [ "scilab-input" ] ] (pcdata cstep.phrase)) in
    M.Ev.onchange_textarea code_input (fun _ev ->
        cstep.phrase <- M.value code_input ;
        cstep.updated <- true ;
        render ~eval: true step ;
        true) ;
    let buttons =
      let close_button = D.(button [ entity "#10005" ]) in
      let insert_button = D.(button [ entity "#8648" ]) in
      M.Ev.onclick close_button (fun _ev ->
          let step =
            if step == cstep then match step.next with
              | Some nstep -> nstep
              | None -> { phrase = "" ; answer = "" ; next = None ; updated = false; liste = [] }
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
      M.Ev.onclick insert_button (fun _ev ->
          let rec ins step =
            match step.next with
            | None ->
              { phrase = "" ; answer = "" ; next = Some step ; updated = false; liste = [] }
            | Some nstep when nstep == cstep ->
              { step with next = Some { phrase = "" ; answer = "" ; next = Some nstep ; updated = false; liste = [] } }
            | Some nstep -> { step with next = Some (ins nstep) } in
          render ~eval: true (ins step) ;
          true) ;
      [ insert_button ; close_button ] in
    D.([div ~a:[ a_class [ "scilab-block" ] ]
          (code_input ::
           cstep.liste @
           (if cstep.answer <> "" then
	            D.([ p ~a:[ a_class [ "scilab-output" ]] [ pcdata cstep.answer ] ])
            else []) @
           [ D.(div ~a:[ a_class [ "buttons" ]] buttons) ])]) @
    match cstep.next with
    | None -> []
    | Some next -> format_result next (nb + 1) invalidated in

  if eval then (InterpPlotLib.all_plots.plots <- [] ; InterpPlotLib.all_plots.updated <- false; update_results step 1 ; ());

  let menu =
    Js.Opt.case (Dom_html.document##getElementById (Js.string "scilab-menu"))
      (fun () -> failwith "scilab-menu element not found")
      (fun menu -> Tyxml_js.Of_dom.of_element menu) in
  let menu_opened = ref None in
  let hide_menu () =
    menu_opened := None ;
    M.removeClass menu "scilab-menu-open" in
  let toolbar_button icon leg cb =
    let button =
      D.(button
           ~a:[a_class ["scilab-toolbar-button"]]
           [ span ~a:[a_class ["icon"]] [ D.entity icon ] ;
             span ~a:[a_class ["legend"]] [ D.pcdata leg ] ]) in
    M.Ev.onclick button (fun _ev -> hide_menu () ; cb () ; true) ;
    button in
  let toolbar_menu_button icon leg ctns =
    toolbar_button icon leg @@ fun () ->
    match !menu_opened with
    | Some prev when prev = leg ->
      menu_opened := None ;
      M.removeClass menu "scilab-menu-open"
    | _ ->
      menu_opened := Some leg ;
      M.addClass menu "scilab-menu-open" ;
      M.replaceChildren menu (ctns ()) in

  let input_session_name =
    D.(input ~a:[a_class ["input-session-name"] ; a_placeholder "untitled"; a_value !current_session; a_size (12) ] ()) in

  let save_button =
    toolbar_button "#128209" "SAVE" @@ fun () ->
    let name = M.value input_session_name in
    match name with
    | "" -> Dom_html.window##alert(Js.string "Please pick a name for your current session before saving.")
    | x -> save_session step x in

  let load_button step =
    toolbar_menu_button "#128209" "LOAD" @@ fun () ->
    match Js.Optdef.to_option (Dom_html.window##localStorage) with
    | None -> D.[div [pcdata "No sessions saved."]]
    | Some locStorage ->
      let ul = D.(ul ~a:[a_class ["scilab-menu-sessions"]] []) in
      for i = 0 to locStorage##length - 1 do
        match Js.Opt.to_option (locStorage##key(i)) with
        | None -> ()
        | Some key ->
          let button_del = D.(button [ entity "#10005" ]) in
		      let li = D.(li [ pcdata (Js.to_string key) ; button_del ] ) in
		      M.Ev.onclick button_del (fun _ev ->
		          let r = Dom_html.window##confirm(Js.string ("Are you sure you want to delete session \""^(Js.to_string key)^"\" ?")) in
		          match Js.to_bool r with
		          | true ->
                delete_session step (Js.to_string key);
                M.removeChild ul li ;
                true
		          | _ -> true) ;
		      M.Ev.onclick li
            (fun _ev -> load_session step (Js.to_string key); current_session := (Js.to_string key);
		          (Js.Unsafe.coerce @@ (D.toelt input_session_name))##value <- key;
		          render ~eval:true step;
		          true);
          M.appendChild ul li
      done ;
      [ ul ] in

  let save_file_button =
    let link = Dom_html.createA(Dom_html.document) in
    let tyxlink = Tyxml_js.Of_dom.of_anchor(link) in
    let button = toolbar_button "#128190" "DOWNLOAD" @@ fun () ->
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
    toolbar_button "#128190" "IMPORT" @@ fun () ->
    Js.Unsafe.meth_call input_files_load "click" [||] in

  let clear_button =
    toolbar_button "#128293" "CLEAR" @@ fun () ->
    let r = Dom_html.window##confirm(Js.string ("Are you sure you want to erase the page ?")) in
    match Js.to_bool r with
    | true -> step.phrase <- ""; step.phrase <- ""; step.next <- None; current_session := ""; render step
    | _ -> () in

  let buttons = [save_button; load_button step; save_file_button; load_file_button; clear_button] in
  let title = D.([ h1 ~a:[a_style "display:inline"] [ pcdata "Sciweb -" ] ; input_session_name]) in
  let toolbar = title @ buttons in
  let contents = format_result step 1 false in
  Js.Opt.case (Dom_html.document##getElementById (Js.string "scilab-toolbar"))
    (fun () -> failwith "scilab-toolbar element not found")
    (fun tb -> M.replaceChildren (Tyxml_js.Of_dom.of_element tb) toolbar) ;

  Js.Opt.case (Dom_html.document##getElementById (Js.string "scilab-tty"))
    (fun () -> failwith "scilab-tty element not found")
    (fun tty ->
       M.Ev.onclick (Tyxml_js.Of_dom.of_element tty) (fun _ev -> hide_menu () ; true) ;
       M.replaceChildren (Tyxml_js.Of_dom.of_element tty) contents) ;
  Js.Unsafe.meth_call Dom_html.window "onresize" [||]

(** where the args are passed and all the fun starts *)
let main () =
  let open  Lwt in
  Lwt_js_events.onload () >>= fun _ ->
  render
    { phrase = "m = [ 3 4 ; 5 6 ] * 3" ;
      answer = "" ; next = None ; updated = false; liste=[] } ;
  Lwt.return ()

let () =
  Lwt.async (fun () -> main ())
