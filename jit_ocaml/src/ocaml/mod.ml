let _ = print_endline "OCaml running"

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
  | Not_supported
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
  | Not_supported -> "not supported"
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
  | _ -> Not_supported

let jit_test ctx = 
  Context.test ctx

let jit_expr expr vars types complex dims =
  print_endline ("OCamlJIT expr: " ^ expr);
  Array.iteri (fun i var -> 
      print_string "  ";
      print_string var; 
      print_string " : "; 
      print_string (string_of_type (type_of_int (Array.get types i)));
      print_string "(";
      print_string (string_of_int (Array.get dims (i * 2)));
      print_string "x";
      print_string (string_of_int (Array.get dims ((i * 2) + 1)));
      print_string "); isComplex : ";
      print_endline (string_of_int (Array.get complex i))) vars

let _ = Callback.register "jit_expr" jit_expr
let _ = Callback.register "jit_test" jit_test
