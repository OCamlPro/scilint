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
