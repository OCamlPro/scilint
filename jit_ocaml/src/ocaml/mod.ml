open ScilabParserAst
open Ollvm
open Llvm

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


let table = Hashtbl.create 5

let index = ref (-1)


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
  let accu = table in
  List.fold_left (fun accu stmt -> match stmt.cstr with
    | Assign (exp_list, exp) -> 
      if is_jit_exp exp
      then
        begin
          incr index;
          let jit_node = make_jit_node stmt !index in
          stmt.cstr <- jit_node;
          (* TODO : Copy Node before Adding *)
          Hashtbl.add accu !index (ghost (Assign (exp_list, exp)));
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
  Printf.printf "OCamlJIT : loading %S\n%!" filename;
  try 
    let ast = ScilabSixParser.parse_file filename in
    ast_to_jit_ast ast;
    let ch = open_out (filename ^ ".jit") in
    Pretty.pretty_output ch ast;
    close_out ch;
    Printf.printf "\n%!";
  with _ -> Printf.printf "OCamlJIT : an error occured while parsing %S\n%!" filename
  
let jit_node function_id = 
  Printf.printf "OCamlJIT : jitting node '%f' \n%!" function_id;
  let node = Hashtbl.find table (int_of_float function_id) in
  Pretty.pretty_output stdout [node];
  Printf.printf "\n%!"

let jit_test ctx = 
  Context.test ctx

let jit_expr expr vars types complex dims =
  Printf.printf "OCamlJIT expr: %s" expr;
  Array.iteri (fun i var -> 
      Printf.printf " %s : %s(%ix%i); isComplex : %i\n%!" 
        var 
        (string_of_type (type_of_int (Array.get types i))) 
        (Array.get dims (i * 2)) 
        (Array.get dims ((i * 2) + 1)) 
        (Array.get complex i)) vars

let _ = 
  Printf.printf "OCaml running";
  Callback.register "jit" jit_node;
  Callback.register "jit_expr" jit_expr;
  Callback.register "jit_test" jit_test;
  Callback.register "jit_load" jit_load
