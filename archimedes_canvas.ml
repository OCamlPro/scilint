module A = Archimedes
module D = Tyxml_js
module P = Archimedes_internals.Path


let min a b = if (a:float) < b then a else b
let max a b = if (a:float) > b then a else b

let round x = truncate(if x < 0. then x -. 0.5 else x +. 0.5)
let round_float x = if x < 0. then x -. 0.5 else x +. 0.5

let fourth_pi = atan 1.
let half_pi = 2. *. fourth_pi
let pi = 4. *. fourth_pi
  
module C =
struct
  let name = "canvas"
    
  type state = {
    mutable color: string;
    mutable line_width : float;
    (* current line width.  When stroking, this is used to set the
       graphics line width, according to the CTM. *)
    mutable dash_offset: float;
    mutable dash: float array;
    mutable ctm : A.Matrix.t; (* current transformation matrix from the
				 user coordinates to the device ones. *)
    mutable font_slant: A.Backend.slant;
    mutable font_weight: A.Backend.weight;
    mutable font_family: string;
    mutable font_size : float;
    mutable clip : A.Matrix.rectangle;
    mutable clip_set : bool;
  }
  type t = {
    width: float;
    height: float;
    mutable closed: bool;
    hold: bool; (* on close, hold the windows until a key is pressed *)
    filename: string; (* filename to save to BMP. "" = do not save *)
    history: state Stack.t; (* saved states *)
    mutable state: state;   (* current state *)
    (* save/restore do not affect the current path. *)
    (* The current path, in device coordinates.  The path structure
       includes its extent and the current point. *)
    mutable current_path: A.Path.t;
    canvas_ctx: Dom_html.canvasRenderingContext2D Js.t }

  let check_valid_handle t =
    (* Firebug.console##log (Js.string "check_valid_handle"); *)
    if t.closed then failwith "Archimedes_graphics: handle closed"

  let make ~options width height =
    let cid  = List.hd options in
    Js.Opt.case (Dom_html.document##getElementById(Js.string cid))
      (fun () -> failwith ("mycanvas element not found : "^cid))
      (fun canvas -> 
	match Js.Opt.to_option (Dom_html.CoerceTo.canvas(canvas)) with
	| None -> failwith "no canvas"
	| Some c -> 
	  let state = {
	    color = "#000"; (* black *)
	    line_width = 1.;
	    dash_offset = 0.;
	    dash = [| |]; (* no dash *)
	    (* Identity transformation matrix *)
	    ctm = A.Matrix.make_identity(); (*{ A.Matrix.xx = 1.; A.Matrix.yx = 0.;
          A.Matrix.xy = 0.;  A.Matrix.yy = -1.; A.Matrix.x0 = 0.; A.Matrix.y0 = height };*)
	    font_slant = A.Backend.Upright;
	    font_weight = A.Backend.Normal;
	    font_family = "sans-serif";
	    font_size = 10.;
	    clip = { A.Matrix.x = nan; y = nan; w = nan; h = nan };
	    clip_set = false;
	  } in
	  
	  { width = width;  
	    height = height;
	    closed = false;
	    hold = true;
	    filename = "filename";
	    history = Stack.create(); 
	    state = state;
	    current_path = A.Path.make();
	    canvas_ctx = c##getContext(Dom_html._2d_) })


  type box_curr_pt = {
    x0: float; y0: float; x1: float; y1: float; (* clipping box *)
    must_clip: bool;
    mutable x: float;  mutable y: float; (* current point *) }

  let box st =
    (* Firebug.console##log (Js.string "box st"); *)
    let m = st.clip in
    let x0 = m.A.Matrix.x and  y0 = m.A.Matrix.y in
    let  x1 = m.A.Matrix.x +. m.A.Matrix.w and  y1 = m.A.Matrix.y +. m.A.Matrix.h in
    let (x0,x1) = if x0 <= x1 then (x0,x1) else (x1,x0) in
    let (y0,y1) = if y0 <= y1 then (y0,y1) else (y1,y0) in
    { x0; x1; y0; y1;  
      must_clip = st.clip_set;
      x = nan; y = nan; } 

  let eps = 1E-6
  let inside x0 x1 x = x0 -. eps <= x && x <= x1 +. eps
  let inside_box box x y =
    inside box.x0 box.x1 x && inside box.y0 box.y1 y

  (** Return the point of intersection between *)
  let intersection x1 y1 x2 y2 x3 y3 x4 y4 =
    (* Formula taken from Wikipedia; article "Line-line intersection" *)
    let f = 1. /. ((x1 -. x2) *. (y3 -. y4) -. (y1 -. y2) *. (x3 -. x4)) in
    let f1 = x1 *. y2 -. y1 *. x2 and f2 = x3 *. y4 -. y3 *. x4 in
    (f1 *. (x3 -. x4) -. (x1 -. x2) *. f2) *. f,
    (f1 *. (y3 -. y4) -. (y1 -. y2) *. f2) *. f

  let distance1 x y x' y' =
    abs_float (x -. x') +. abs_float (y -. y')

  let clip_point box x y x' y' =
    (* FIXME this code seems really improvable *)
    let x1, y1 = intersection box.x0 box.y0 box.x1 box.y0 x y x' y'
    and x2, y2 = intersection box.x1 box.y0 box.x1 box.y1 x y x' y'
    and x3, y3 = intersection box.x0 box.y1 box.x1 box.y1 x y x' y'
    and x4, y4 = intersection box.x0 box.y0 box.x0 box.y1 x y x' y' in
    let d1 = if inside box.x0 box.x1 x1 then distance1 x y x1 y1 else infinity
    and d2 = if inside box.y0 box.y1 y2 then distance1 x y x2 y2 else infinity
    and d3 = if inside box.x0 box.x1 x3 then distance1 x y x3 y3 else infinity
    and d4 = if inside box.y0 box.y1 y4 then distance1 x y x4 y4 else infinity in
    let data = [(x1, y1, d1); (x2, y2, d2); (x3, y3, d3); (x4, y4, d4)] in
    let third (_, _, d) = d in
    let default = (nan, nan, infinity) in
    let x, y, _ = List.fold_left
      (fun a b -> if third b < third a then b else a) default data
    in x, y

  let clipped_segment b x y x' y' =
    if b.must_clip then (
      let nx, ny =
        if inside_box b x y then x, y
        else clip_point b x y x' y'
      and nx', ny' =
        if inside_box b x' y' then x', y'
        else clip_point b x' y' x y
      in
      (* Note: If one variable (here nx) is correct, the other are also *)
      if nx < min x x' || nx > max x x' then (nan, nan, nan, nan)
      else (nx, ny, nx', ny')
    )
    else (x, y, x', y')

  let curve_nsamples = 20 (* curve_nsamples + 1 points *)
  let curve_dt = 1. /. float curve_nsamples

  let fill_line_to b x y coords =
    (* Firebug.console##log (Js.string "fill_line_to"); *)
    let cbx, cby, cx, cy = clipped_segment b b.x b.y x y in
    if cbx = cbx && cx = cx (* no NaN *) then begin
      if cbx <> b.x || cby <> b.y then
        coords := (round cbx, round cby) :: !coords;
      coords := (round cx, round cy) :: !coords
    end;
    b.x <- x;
    b.y <- y

  (* Add some points on the Bezier curve to [coords], except the 1st
     point which is supposed to be added by the previous component of
     the path. *)
  let add_curve_sampling b x0 y0  x1 y1  x2 y2  x3 y3 coords =
    for i = 1 to curve_nsamples do
      let t = float i *. curve_dt in
      let tm = 1. -. t in
      let t2 = t *. t   and tm2 = tm *. tm in
      let t3 = t2 *. t  and tm3 = tm2 *. tm in
      let t' = 3. *. t2 *. tm  and t'' = 3. *. t *. tm2 in
      let x = tm3 *. x0 +. t'' *. x1 +. t' *. x2 +. t3 *. x3
      and y = tm3 *. y0 +. t'' *. y1 +. t' *. y2 +. t3 *. y3 in
      (* FIXME: Clipping each subsegment is very expensive *)
      fill_line_to b x y coords
    done

  let id x y = (x, y)

  let fill_subpath t m = 
    (* Firebug.console##log (Js.string "fill_subpath"); *)
    let ctx = t.canvas_ctx in
    match m with
    | [] | [_] -> ()
    | [ (x1,y1); (x0,y0) ] ->
      ctx##moveTo(float_of_int x0, float_of_int y0);
      ctx##lineTo(float_of_int x1, float_of_int y1);
      ctx##stroke();
    | coords -> 
      let arr = Array.of_list coords in
      ctx##moveTo(float_of_int (fst arr.(0)), float_of_int (snd arr.(0)));
      for i=1 to Array.length arr -1 do
	ctx##lineTo(float_of_int(fst arr.(i)), float_of_int(snd arr.(i)))
      done;
      ctx##lineTo(float_of_int(fst arr.(0)), float_of_int(snd arr.(0)));
      ctx##stroke()

	
  let rec gather_subpath b to_bk coords t = function
    | P.Move_to(x,y) ->
      fill_subpath t !coords;  (* previous subpath *)
      coords := [];  (* Clean the coords of the subpath already filled. *)
      let x, y = to_bk x y in
      b.x <- x;
      b.y <- y;  (* Do no put the pt in coords in case 2 Move_to follow *)
    | P.Line_to(x,y)
    | P.Close(x,y) ->
      let x, y = to_bk x y in
      fill_line_to b x y coords
    | P.Array(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.(i) y.(i) in fill_line_to b x y coords
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.(i) y.(i) in fill_line_to b x y coords
        done
    | P.Fortran(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.{i} y.{i} in fill_line_to b x y coords
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.{i} y.{i} in fill_line_to b x y coords
        done
    | P.C(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.{i} y.{i} in fill_line_to b x y coords
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.{i} y.{i} in fill_line_to b x y coords
        done
    | P.Curve_to(x0,y0, x1,y1, x2,y2, x3,y3) ->
      let x0, y0 = to_bk x0 y0
      and x1, y1 = to_bk x1 y1
      and x2, y2 = to_bk x2 y2
      and x3, y3 = to_bk x3 y3 in
      add_curve_sampling b x0 y0 x1 y1 x2 y2 x3 y3 coords

  let fill_preserve t =
    (* Firebug.console##log (Js.string "fill_preserve"); *)
    let st = t.state in
    let coords = ref [] in
    P.iter t.current_path (gather_subpath (box st) id coords t);
    fill_subpath t !coords

  let fill t =
    (* Firebug.console##log (Js.string "fill"); *)
    fill_preserve t;
    A.Path.clear t.current_path

      
  let stroke_line_to b x y t =
   (* Firebug.console##log (Js.string "stroke_line_to");
    Firebug.console##log (Printf.kprintf Js.string "x=%f y=%f bx=%f by=%f bw=%f bh=%f" x y b.x0 b.y0 b.x1 b.y1);*)
    let cx, cy, cx', cy' = clipped_segment b b.x b.y x y in
    if cx = cx && cx' = cx' (* no NaN *) then begin
      if cx <> b.x || cy <> b.y then (
	t.canvas_ctx##moveTo(round_float cx, round_float cy);
	(*Firebug.console##log (Js.string (Printf.sprintf "cx=%d cy=%d" (round cx) (round cy)));*)
      );
      t.canvas_ctx##lineTo(round_float cx', round_float cy');
      if cx' <> x || cy' <> y then 
	t.canvas_ctx##moveTo(round_float x, round_float y);
    end
    else t.canvas_ctx##moveTo(round_float x,round_float y);
    t.canvas_ctx##stroke ();
    
    b.x <- x;
    b.y <- y

  (* FIXME: macro to avoid the call [to_bk] ? *)
  (* [to_bk x y]: transform coordinates to the graphics ones. *)
  let stroke_on_backend b to_bk t k =
    (* Firebug.console##log (Printf.kprintf Js.string "stroke_on_backend %s" t.state.color); *)
    match k with
    | P.Move_to(x,y) ->
      let x, y = to_bk x y in
      t.canvas_ctx##moveTo(round_float x, round_float y);
      t.canvas_ctx##stroke ();
      b.x <- x;
      b.y <- y;
    | P.Line_to(x,y)
    | P.Close(x, y) ->
      let x, y = to_bk x y in
      stroke_line_to b x y t
    | P.Array(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.(i) y.(i) in stroke_line_to b x y t
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.(i) y.(i) in stroke_line_to b x y t
        done
    | P.Fortran(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.{i} y.{i} in stroke_line_to b x y t
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.{i} y.{i} in stroke_line_to b x y t
        done
    | P.C(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.{i} y.{i} in stroke_line_to b x y t
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.{i} y.{i} in stroke_line_to b x y t
        done
    | P.Curve_to(_, _, x1, y1, x2, y2, x3, y3) ->
      let x1, y1 = to_bk x1 y1
      and x2, y2 = to_bk x2 y2
      and x3, y3 = to_bk x3 y3 in
      (* FIXME: clip Bézier curve *)
      t.canvas_ctx##bezierCurveTo
        (round_float x1, round_float y1, round_float x2, round_float y2, round_float x3, round_float y3);
      t.canvas_ctx##stroke()

  

  let show_text t ~rotate ~x ~y pos txt =
    (* Firebug.console##log (Js.string ("show_text " ^ txt)); *)
    let st = t.state in
    (* Compute the angle between the desired direction and the X axis
       in the device coord. system. *)
    let dx, dy = A.Matrix.transform_distance st.ctm (cos rotate) (sin rotate) in
    let angle = atan2 dy dx in
    let x', y' = A.Matrix.transform_point st.ctm x y in
    let w' = int_of_float (t.canvas_ctx##measureText(Js.string txt)##width) in
    let h' = int_of_float st.font_size in
    (* text_size returns size already in device coords.*)
    let px = match pos with
      | A.Backend.LC | A.Backend.LT | A.Backend.LB -> float w'
      | A.Backend.CC | A.Backend.CT | A.Backend.CB -> float w' *. 0.5
      | A.Backend.RC | A.Backend.RT | A.Backend.RB -> 0.
    and py = match pos with
      | A.Backend.CB | A.Backend.RB | A.Backend.LB -> float h'
      | A.Backend.CC | A.Backend.RC | A.Backend.LC -> float h' *. 0.5
      | A.Backend.CT | A.Backend.RT | A.Backend.LT -> 0.
    in
    if w' > 0 && h' > 0 then (
      if abs_float angle <= 1e-6 then
        (t.canvas_ctx##moveTo(round_float(x' -. px), round_float(y' +. py)); 
         t.canvas_ctx##fillText(Js.string txt, round_float(x' -. px), round_float (y' +. py)))
      else (
	t.canvas_ctx##translate(round_float(x' -. px), round_float (y' -. py));
	t.canvas_ctx##rotate(-.rotate); 
        t.canvas_ctx##fillText(Js.string txt, 0., 20.);
	t.canvas_ctx##setTransform(1., 0., 0., 1., 0., 0.))
      )
       

  let set_color t c =
    let st = t.state in
    let r = int_of_float (A.Color.r c *. 255.)
    and g = int_of_float (A.Color.g c *. 255.)
    and b = int_of_float (A.Color.b c *. 255.) in
    let color = Printf.sprintf "#%02X%02X%02X" r g b in
    st.color <- color ;
    t.canvas_ctx##strokeStyle <- Js.string st.color

  let set_line_width t w =
    if w < 0. then invalid_arg "set_line_width";
    t.state.line_width <- w
    
  let get_line_width t = t.state.line_width

  let set_dash t ofs arr =
    let st = t.state in
    st.dash_offset <- ofs;
    st.dash <- arr

  let get_dash t = (t.state.dash, t.state.dash_offset)

  let set_line_cap t _ = check_valid_handle t
  let get_line_cap  t = check_valid_handle t; A.Backend.ROUND
  let set_line_join t _ = check_valid_handle t
  let get_line_join t = check_valid_handle t; A.Backend.JOIN_MITER


  let move_to t ~x ~y = 
    let st = t.state in
    let x', y' = A.Matrix.transform_point st.ctm x y in
    A.Path.move_to t.current_path x' y'

  let line_to t ~x ~y = 
    let st = t.state in
    let x', y' = A.Matrix.transform_point st.ctm x y in
    A.Path.line_to t.current_path x' y'

  let rel_move_to t ~x ~y =
    let st = t.state in
    let x, y = A.Matrix.transform_distance st.ctm x y in
    A.Path.rel_move_to t.current_path x y

  let rel_line_to t ~x ~y =
    let st = t.state in
    let x',y' = A.Matrix.transform_distance st.ctm x y in
    A.Path.rel_line_to t.current_path x' y'


  let internal_curve_to t st ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    (* Suffices to transform the control point by the affine
       transformation to have the affine image of the curve *)
    let x1', y1' = A.Matrix.transform_point st.ctm x1 y1 in
    let x2', y2' = A.Matrix.transform_point st.ctm x2 y2 in
    let x3', y3' = A.Matrix.transform_point st.ctm x3 y3 in
    A.Path.curve_to t.current_path x1' y1' x2' y2' x3' y3'

  let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    internal_curve_to t t.state ~x1 ~y1 ~x2 ~y2 ~x3 ~y3
 
    
  let rectangle t ~x ~y ~w ~h =
    let st = t.state in
    let x', y' = A.Matrix.transform_point st.ctm x y
    and w'x, w'y = A.Matrix.transform_distance st.ctm w 0.
    and h'x, h'y = A.Matrix.transform_distance st.ctm 0. h in
    A.Path.move_to t.current_path x' y';
    A.Path.rel_line_to t.current_path w'x w'y;
    A.Path.rel_line_to t.current_path h'x h'y;
    A.Path.rel_line_to t.current_path (-. w'x) (-. w'y);
    A.Path.close t.current_path


  let arc_add_piece t st ~x0 ~y0 ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    internal_curve_to t st ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

  let arc t ~r ~a1 ~a2 =
    let st = t.state in
    (* One must transform the arc to bezier curves before acting with
       the CTM as it may deform the arc. *)
    let x, y =
      try A.Path.current_point t.current_path
      with _ -> failwith "archimedes_graphics.arc: no current point" in
    let x0, y0 = A.Matrix.inv_transform_point st.ctm x y in
    P.bezier_of_arc st (arc_add_piece t) ~x0 ~y0 ~r ~a1 ~a2


  let close_path t =
    check_valid_handle t;
    A.Path.close t.current_path
    
  let clear_path t = 
    check_valid_handle t;
    A.Path.close t.current_path


  let path_extents t = A.Path.extents t.current_path
    
  let stroke_preserve t =
    t.canvas_ctx##beginPath();
    (* Firebug.console##log (Printf.kprintf Js.string "stroke_preserve lw=%f" t.state.line_width); *)
    t.canvas_ctx##lineWidth <- t.state.line_width ;
    P.iter t.current_path (stroke_on_backend (box t.state) id t)

  let stroke t = 
    t.canvas_ctx##beginPath();
    (* Firebug.console##log (Js.string "stroke"); *)
    stroke_preserve t;
    A.Path.clear t.current_path

  let stroke_path_preserve t path =
    t.canvas_ctx##beginPath();
    (* Firebug.console##log(Js.string "stroke_path_preserve "); *)
    t.canvas_ctx##lineWidth <- t.state.line_width ;
    let to_bk x y = A.Matrix.transform_point t.state.ctm x y in
    P.iter path (stroke_on_backend (box t.state) to_bk t)

  let fill_path_preserve t path =
    (* Firebug.console##log(Js.string "fill_path_preserve"); *)
    let to_bk x y = A.Matrix.transform_point t.state.ctm x y in
    P.iter path (stroke_on_backend (box t.state) to_bk t)

  let fill_with_color t c = 
    (* Firebug.console##log (Js.string "fill_with_color"); *)
    let color = t.state.color in
    set_color t c;  
    fill t;
    t.state.color <- color;
    t.canvas_ctx##strokeStyle <- Js.string color
    
  let show t = Firebug.console##log (Js.string "show"); ()
    
  let clip_rectangle t ~x ~y ~w ~h =
    let st = t.state in
    let x, y = A.Matrix.transform_point st.ctm x y in
    let w, h = A.Matrix.transform_distance st.ctm w h in
    st.clip <- { A.Matrix.x = x; y = y; w = w; h = h };
    st.clip_set <- true

  let save t = 
    let st = t.state in
    (* Make a copy of the record so that further actions do not modify
       it. We need to store a *copy* of the ctm, because
       scaling/translation/rotation mustn't modify this stored
       matrix.*)
    let state_copy = { st with ctm = A.Matrix.copy st.ctm} in
    Stack.push state_copy t.history
  let restore t = 
     check_valid_handle t;
    try
      let st = Stack.pop t.history in
      t.state <- st;
      (* Re-enable previous settings in case they were changed *)
      
    with Stack.Empty ->
      invalid_arg "Archimedes_graphics.restore: no previous save issued."

  let translate t ~x ~y = A.Matrix.translate t.state.ctm x y 
  let scale t ~x ~y = A.Matrix.scale t.state.ctm x y
  let rotate t ~angle = A.Matrix.rotate t.state.ctm ~angle
  let set_matrix t m = t.state.ctm <- A.Matrix.copy m
  let get_matrix t = A.Matrix.copy t.state.ctm

  let flipy t = true

  let close ~options:_ t = ()

  let text_extents t txt = { A.Matrix.x = 0.; y = 0.; w = 10. ; h = 10. }

  let text_string t =
    let str =
      String.concat " "
	((match t.state.font_slant with A.Backend.Italic -> [ "italic" ] | A.Backend.Upright ->  [])
	 @ (match t.state.font_weight with A.Backend.Bold -> [ "bold" ] | A.Backend.Normal ->  [])
	 @ [ string_of_int (int_of_float t.state.font_size) ^ "px" ]
	 @ [ t.state.font_family ]) in
    (* Firebug.console##log (Js.string str) ; *)
    str

  let set_font_size t size =
    t.state.font_size <- size;
    t.canvas_ctx##font <- Js.string (text_string t)

  let select_font_face t slant weight family =
    t.state.font_slant <- slant;
    t.state.font_weight <- weight;
    t.state.font_family <- family;
    t.canvas_ctx##font <- Js.string (text_string t)
   
end

let () =
  let module U = A.Backend.Register(C) in ()
