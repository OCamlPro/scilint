open ScilabParserAst
open Ollvm
open Llvm

let _ = print_endline "OCaml running"

let table = ref (Hashtbl.create 5)

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

let cpt = ref (-1)

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

let make_jit_node node nbr =
  let jit_call = ghost (Call (ghost (Var (ghost "jit")), [(None, ghost (Num (float_of_int nbr)))], Tuplified)) in
  let cond = ghost (Unop (Not,jit_call)) in
  If (cond, (ghost node.cstr), None)

let is_jit_exp exp = match exp.cstr with
  | Call (exp1, arg_list, call_kind) -> false
  | Identity exp_list -> false
  | Range (exp1, exp2_option, exp3) -> false
  | Bool bool -> false
  | Num float -> false
  | String string -> false
  | Var var -> false
  | Colon -> false
  | Error -> false
  | Matrix matrix_contents -> false
  | Cell_array matrix_contents -> false
  | Unop (unop, exp1) -> false
  | Op (op, exp1, exp2) ->
    begin
      match op, exp1.cstr, exp2.cstr with
      | Plus, Num num1, Num num2 -> true
      | _ -> false
    end

let ast_to_jit_ast ast = 
  cpt := -1;
  let accu = !table in
  List.fold_left (fun accu stmt -> match stmt.cstr with
    | Assign (exp_list, exp) -> 
      if is_jit_exp exp
      then
        begin
          incr cpt;
          let jit_node = make_jit_node stmt !cpt in
          stmt.cstr <- jit_node;
          (* TODO : Copy Node before Adding *)
          Hashtbl.add accu !cpt (ghost (Assign (exp_list, exp)));
          accu
        end
      else accu
    | Defun defun_params -> accu
    | Exp exp -> accu
    | Comment str -> accu
    | Seq stmt_list -> accu
    | Break -> accu
    | Continue -> accu
    | For (var, exp, stm1) -> accu
    | If (exp, stmt1, stmt2_opt) -> accu
    | Return -> accu
    | Select select_params -> accu
    | Try (stmt1, stmt2) -> accu
    | While (exp, stmt1, stmt2_opt) -> accu
  ) accu ast

let jit_load ctx filename = 
  Printf.printf "OCamlJIT : loading '%s' \n%!" filename;
  try 
    let ast = ScilabSixParser.parse_file filename in
    table := ast_to_jit_ast ast;
    let ch = open_out (filename ^ ".jit") in
    Pretty.pretty_output ch ast;
    close_out ch;
    Printf.printf "\n%!";
  with _ -> print_endline ("OCamlJIT : an error occured while parsing '" ^ filename ^ "'")
  
let jit_node nbr = 
  Printf.printf "OCamlJIT : jitting node '%f' \n%!" nbr;
  let node = Hashtbl.find !table (int_of_float nbr) in
  Pretty.pretty_output stdout [node];
  Printf.fprintf stdout "\n%!"

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

let _ = Callback.register "jit" jit_node
let _ = Callback.register "jit_expr" jit_expr
let _ = Callback.register "jit_test" jit_test
let _ = Callback.register "jit_load" jit_load
