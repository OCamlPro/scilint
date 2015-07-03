open InterpLib
open InterpCore.Values

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

(* Transpose of a matrix *)
let () =
  register_library (fun state lib ->
    register_unop lib InterpCore.Ast.Transpose_conjugate (matrix real) (matrix real)
      (fun mat ->
	let w, h = matrix_size mat in
	let res = matrix_create (Number Real) w h in
	for i=1 to w do
	  for j=1 to h do
	    matrix_set res j i (matrix_get mat i j)
	  done;
	done;
	res)
  )


let mult_matrix mat1 mat2 =
  let w1, h1 = matrix_size mat1 in
  let w2, h2 = matrix_size mat2 in
  if h1 <> w2 then raise Bad_type;
  let res = matrix_create (Number Real) w1 h2 in
  for i=1 to w1 do
    for j=1 to h2 do
      let sum = ref 0.0 in
      for m=1 to h1 do
	sum := !sum +. (matrix_get mat1 i m) *. (matrix_get mat2 m j)
	    done;
      matrix_set res i j !sum
    done;
	done;
  res


(* Matrix product *)
let ()=
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
    register_function lib state "sqrt" (real @-> real)
      (fun r -> sqrt r ))

let () =
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
	InterpLib.plots.liste <- InterpLib.plots.liste @ [!l] ;
	InterpLib.plots.updated <- true
	)
  )

let () =
  register_library (fun state lib ->
    register_function lib state "clf" (void @-> void)
      (fun () ->
	InterpLib.plots.liste <- [];
	(*InterpLib.plots.updated <- true;*)
      )
  )
