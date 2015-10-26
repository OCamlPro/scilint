open InterpLib
open InterpCore.Values

(*--- Matrices ---*)

let () =
  register_library (fun state lib ->
      register_function lib state "mean" (matrix real @* opt (flag [ "r", `R ; "c", `C ; "m", `M ]) @-> matrix real)
        (fun mat mode ->
           let w, h = matrix_size mat in
           let mode = match mode with
             | Some `R -> `Columnwise
             | Some `C -> `Rowwise
             | Some `M when w > 1 && h > 1 -> `Columnwise
             | Some `M | None -> `Full in
           match mode with
           | `Full ->
             let w, h = matrix_size mat in
             let sum = ref 0.0 in
             for i = 1 to w do
               for j = 1 to h do
                 sum := !sum +. matrix_get mat i j
               done
             done ;
             let res = matrix_create (Number Real) 1 1 in
             matrix_set res 1 1 (!sum /. float (w * h)) ;
             res
           | `Columnwise ->
             let res = matrix_create (Number Real) w 1 in
             for i = 1 to w do
               let sum = ref 0.0 in
               for j = 1 to h do
                 sum := !sum +. matrix_get mat i j
               done ;
               matrix_set res i 1 (!sum /. float h)
             done ;
             res
           | `Rowwise ->
             let res = matrix_create (Number Real) 1 h in
             for j = 1 to h do
               let sum = ref 0.0 in
               for i = 1 to w do
                 sum := !sum +. matrix_get mat i j
               done ;
               matrix_set res 1 j (!sum /. float w)
             done ;
             res)
    )

(* transpose of a matrix *)
let () =
  register_library (fun state lib ->
    register_unop lib InterpCore.Ast.Transpose_conjugate (matrix real) (matrix real)
      (fun mat ->
	let w, h = matrix_size mat in
	let res = matrix_create (Number Real) h w in
	for i=1 to h do
	  for j=1 to w do
	    matrix_set res i j (matrix_get mat j i)
	  done;
	done;
	res)
  )


let mult_matrix mat1 mat2 =
  let w1, h1 = matrix_size mat1 in
  let w2, h2 = matrix_size mat2 in
  if w1 <> h2 then raise Bad_type;
  let res = matrix_create (Number Real) h1 w2 in
  for i=1 to h1 do
    for j=1 to w2 do
      let sum = ref 0.0 in
      for k=1 to h2 do
	sum := !sum +. (matrix_get mat1 k i) *. (matrix_get mat2 j k)
	    done;
      matrix_set res j i !sum
    done;
	done;
  res


let () =
  register_library (fun state lib ->
    register_binop lib InterpCore.Ast.Times (matrix real) (matrix real) (matrix real)
      (fun mat1 mat2 -> mult_matrix mat1 mat2 )
  )

let () =
  register_library (fun state lib ->
    register_binop lib InterpCore.Ast.Power (matrix real) (real) (matrix real)
      (fun mat r ->
	let w, h = matrix_size mat in
	if w <> h || r < 0. then raise Bad_type;
	begin
	  match r with
	  | 0. ->  let res = matrix_create (Number Real) 1 1 in
		  matrix_set res 1 1 1. ; res
	  | 1. -> mat
	  | n -> let res = ref (mult_matrix mat mat) in
		 for i=3 to int_of_float n do
		   res := mult_matrix !res mat
		 done; !res ;
	end ;
	)
  )

let () =
  register_library (fun state lib ->
    register_function lib state "ones" (real @* real @-> matrix real)
      (fun h w ->
	if h < 1. || w < 1. then 
	  matrix_create (Number Real) 0 0 
	else begin
	  let width = int_of_float(floor w) and height = int_of_float(floor h) in
	  let res = matrix_create (Number Real) height width in
	  for i=1 to width do
	    for j=1 to height do
	      matrix_set res j i 1. 
	    done;
	  done;
	  res;
	end)
  )
	
(*--- Basic math functions ---*)

let () = 
  register_library (fun state lib ->
    register_function lib state "sqrt" (real @-> real)
      (fun r -> sqrt r )) (* a changer : si negatif, complex *)

let () = 
  register_library (fun state lib ->
    register_function lib state "floor" (real @-> real)
      (fun r -> floor(r) ))


(*--- definir des lois ---*)

let factorial x =
  let rec loop x acc =
    if x <= 1 then 
      acc
    else 
      loop (x - 1) (acc * x)
  in
  loop x 1

let () =
  register_library (fun state lib ->
    register_function lib state "factorial" (int32 @-> int32)
      (fun x -> factorial x)
  )

let binomial_coefficient n k =
  float (factorial n) /. float (factorial k * factorial (n - k))

let () =
  register_library (fun state lib ->
    register_function lib state "binomial_coefficient" (int32 @* int32 @-> real)
      (fun n k -> binomial_coefficient n k)
  )
  
let () =
  register_library (fun state lib ->
    register_function lib state "binomial" (int32 @* int32 @* real @-> real)
      (fun n k p ->
	(binomial_coefficient n k) *. (p ** float k) *. ((1. -. p) ** float (n-k)))
  )


(*--- Charts ---*)

(*let () =
  register_library (fun state lib ->
    register_function lib state "plot" (matrix real @* matrix real @-> void)
      (fun xMat yMat ->
	let w, h = matrix_size xMat in
	let l = ref [] in
	for i=1 to w do
	  for j=1 to h do
	    l := !l @ [ (matrix_get xMat i j, matrix_get yMat i j) ]
	  done;
	done;
	let open InterpLib in
	InterpLib.plots.liste <- 
	  InterpLib.plots.liste @ [{kind = `L; x_label=None; y_label=None; title=None; points = !l}] ;
	InterpLib.plots.updated <- true)
  )*)

let () =
  register_library (fun state lib ->
    register_function lib state "plot_canvas" (matrix real @* matrix real @-> void)
      (fun xMat yMat ->
	let w, h = matrix_size xMat in
	let xval = ref []  and yval = ref [] in
	for i=1 to w do
	  for j=1 to h do
	    xval := !xval @ [matrix_get xMat i j];
	    yval := !yval @ [matrix_get yMat i j];
	  done;
	done;
	let open InterpLib in
	InterpLib.plots_in_canvas.liste <- 
	  InterpLib.plots_in_canvas.liste @ [{xvalues = !xval; yvalues = !yval}] ;
	InterpLib.plots_in_canvas.updated <- true)
  )

let () =
  register_library (fun state lib ->
    register_function lib state "clf" (void @-> void)
      (fun () ->
	InterpLib.all_plots.plots <- [];
	(*InterpLib.plots.updated <- true;*)
      )
  )

let () =
  register_library (fun state lib ->
    register_function lib state "plot" (matrix real @* matrix real @-> void)
      (fun xMat yMat ->
	let w, h = matrix_size xMat in
	let xval = ref []  and yval = ref [] in
	for i=1 to w do
	  for j=1 to h do
	    xval := !xval @ [matrix_get xMat i j];
	    yval := !yval @ [matrix_get yMat i j];
	  done;
	done;
	let open InterpLib in
	InterpLib.all_plots.plots <- 
	  InterpLib.all_plots.plots @ [{style =`Lines; xvalues = !xval; yvalues = !yval;
			    grid=false; title=Some ""; xlabel=Some ""; ylabel=Some ""}] ;
	InterpLib.all_plots.updated <- true)
  )

let () =
  register_library (fun state lib ->
    register_function lib state "bar" (matrix real @* matrix real @-> void)
      (fun xMat yMat ->
	let w, h = matrix_size xMat in
	let xval = ref []  and yval = ref [] in
	for i=1 to w do
	  for j=1 to h do
	    xval := !xval @ [matrix_get xMat i j];
	    yval := !yval @ [matrix_get yMat i j];
	  done;
	done;
	let open InterpLib in
	InterpLib.all_plots.plots <- 
	  InterpLib.all_plots.plots @ [{style =(`Bars 0.1); xvalues = !xval; yvalues = !yval;
			    grid=false; title=Some ""; xlabel=Some ""; ylabel=Some ""}] ;
	InterpLib.all_plots.updated <- true)
  )


let () =
  register_library (fun state lib ->
    register_function lib state "xtitle" (string @* opt string @* opt string @-> void)
      (fun title xlabel ylabel ->
	let hd = List.hd InterpLib.all_plots.plots in
	let tl = List.tl InterpLib.all_plots.plots in
	let new_hd = match xlabel, ylabel with
	  | None, None -> {hd with title=Some title}
	  | Some x, Some y -> {hd with xlabel=Some x; ylabel=Some y; title=Some title}
	  | None, Some y -> {hd with ylabel=Some y; title=Some title}
	  | Some x, None -> {hd with xlabel=Some x; title=Some title} in
	InterpLib.all_plots.plots <- new_hd :: tl;
	InterpLib.all_plots.updated <- true)
  )

let () =
  register_library (fun state lib ->
    register_function lib state "grid" (void @-> void)
      (fun () -> 
	let hd = List.hd InterpLib.all_plots.plots in
	let tl = List.tl InterpLib.all_plots.plots in
	let new_hd = {hd with grid = not hd.grid} in
	InterpLib.all_plots.plots <- new_hd :: tl;
	InterpLib.all_plots.updated <- true)
  )


(*let () =
  register_library (fun state lib ->
    register_function lib state "bar" ( matrix real @* opt(matrix real) @-> void)
      (fun xMat yMat ->
	let w, h = matrix_size xMat in
	let l = ref [] in begin
	match yMat with
	| None -> 
	  for i=1 to w do
	    for j=1 to h do
	      l := !l @ [ (float_of_int i, matrix_get xMat i j) ]
	    done;
	  done;
	| Some y ->
	  for i=1 to w do
	    for j=1 to h do
	      l := !l @ [ (matrix_get xMat i j, matrix_get y i j) ]
	    done;
	  done; end;
	InterpLib.plots.liste <- 
	  InterpLib.plots.liste @ [{kind=`B; x_label=None; y_label=None; title=None; points= !l}] ;
	InterpLib.plots.updated <- true)
  )*)

(*let () =
  register_library (fun state lib ->
    register_function lib state "xtitles" (string @* opt string @* opt string @-> void)
      (fun title x_label y_label -> 
	let hd = List.hd InterpLib.plots.liste in
	let tl = List.tl InterpLib.plots.liste in
	let new_hd = match x_label, y_label with
	  | None, None -> {hd with title=Some title}
	  | Some x, Some y -> {hd with x_label=Some x; y_label=Some y; title=Some title}
	  | _, _ -> raise Bad_type in
	InterpLib.plots.liste <- new_hd :: tl;
	InterpLib.plots.updated <- true)
  )*)

(*--- Statistiques ---*)

let sum v =
  let w,h = matrix_size v in
  let sum = ref 0.0 in
  for i=1 to w do
    sum := !sum +. matrix_get v i 1
  done;
  !sum

let () =
  register_library (fun state lib ->
    register_function lib state "sum" (matrix real @-> real)
      (fun v -> sum v)
  )

let () =
  register_library (fun state lib ->
    register_function lib state "median_vector" (matrix real @-> real)
      (fun m ->
	let w, h = matrix_size m in
	let v = Array.make w 0.0 in
	for i=1 to w do
	  v.(i-1) <- matrix_get m i 1
	done;
	Array.sort (fun a b -> if a >= b then 1 else -1) v;
	let res = (if (Array.length v) mod 2 = 0 then
	  let n = Array.length v / 2 in
	  ( v.(n) +. v.(n+1) ) /. 2.
	else 
	  v.( Array.length v / 2)) in
	res)
  )

let mean_vector v =
  let w, h = matrix_size v in
  (sum v) /. (float_of_int w)

let () =
  register_library (fun state lib ->
    register_function lib state "mean_vector" (matrix real @-> real)
      (fun v -> mean_vector v)
  ) 
 
let variance v =
  let w, h = matrix_size v in
  let moy = mean_vector v in
  let sum = ref 0. in
  for i=1 to w do
    let current = matrix_get v i 1 in
    sum := !sum +. current *. current
  done;
  (!sum /. (float w)) -. (moy *. moy)


let () =
  register_library (fun state lib ->
    register_function lib state "variance" (matrix real @-> real)
      (fun v -> variance v)
  ) 

(* ecart-type *)
let () =
  register_library (fun state lib ->
    register_function lib state "stdev" (matrix real @-> real)
      (fun v -> Pervasives.sqrt (variance v))
  )

let () =
  register_library (fun state lib ->
    register_function lib state "quartiles" (matrix real @-> real)
      (fun v -> 
	let w, h = matrix_size v in
	let q1 = ref 0. and q3 = ref 0. in
	q1 := matrix_get v (int_of_float (ceil ((float_of_int w)/.4.))) 1;
	q3 := matrix_get v (int_of_float (ceil ( 3.*.(float_of_int w)/.4.))) 1;
	!q1)
  )



(*--- Simulation ---*)


let () =
  register_library (fun state lib ->
    register_function lib state "rand_int" (int32 @* int32 @-> int32 )
      (fun a b -> Random.int (b - a + 1) + a)
  ) 

let () =
  register_library (fun state lib ->
    register_function lib state "rand_float" (real @* real @-> real)
      (fun a b -> Random.float (b -. a +. 1.) +. a)
  )

let () =
  register_library (fun state lib ->
    register_function lib state "tirage_entier" (int32 @* int32 @* int32 @-> matrix real)
      (fun p a b ->
	let res = matrix_create (Number Real) p 1 in
	for i=1 to p do
	  matrix_set res i 1 (float_of_int (Random.int (b - a + 1) + a))
	done;
	res)
  )

let () =
  register_library (fun state lib ->
    register_function lib state "tirage_real" (int32 @* real @* real @-> matrix real)
      (fun p a b ->
	let res = matrix_create (Number Real) p 1 in
	for i=1 to p do
	  matrix_set res i 1 (Random.float (b -. a +. 1.) +. a)
	done;
	res)
  )


let () =
  register_library (fun state lib ->
    register_function lib state "sin" (matrix real @-> matrix real)
      (fun mat -> 
	let w, h = matrix_size mat in
	let res = matrix_create (Number Real) w h in
	for i=1 to w do
	  for j=1 to h do
	    matrix_set res i j (sin ( matrix_get mat i j))
	  done;
	done;
	res)
  ) 
