let _ = print_endline "OCaml's running"

type sci_type = 
  | Double 
  | Poly 
  | Boolean 
  | Sparse
  | Boolean_sparse 
  | Int 
  | String 
  | List of sci_type list 
  | TList of sci_type list 
  | MList of sci_type list
  | Non_supported
  | NotFoundType

let string_of_type = function 
  | Double -> "double"
  | Poly -> "poly"
  | Boolean -> "boolean"
  | Sparse -> "sparse"
  | Boolean_sparse -> "bsparse"
  | Int -> "int"
  | String -> "string"
  | List _ -> "list"
  | TList _ -> "tlist"
  | MList _ -> "mlist"
  | Non_supported -> "not supported"
  | NotFoundType -> "type not found"
    
let type_of_int = function
  | -1 -> NotFoundType
  | 0 -> Double
  | 1 -> Poly
  | 2 -> Boolean
  | 3 -> Sparse
  | 4 -> Boolean_sparse
  | 5 -> Int
  | 6 -> String
  | _ -> Non_supported

let jit_expr expr vars types =
  print_endline ("OCamlJIT expr: " ^ expr);
  print_endline "Free Variables :";
  Array.iteri (fun i var -> 
      print_string "  ";
      print_string var; 
      print_string " : "; 
      print_endline (string_of_type (type_of_int (Array.get types i)))) vars

let _ = Callback.register "jit_expr" jit_expr

