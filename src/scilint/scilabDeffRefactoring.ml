open ScilabAst

let is_deff_call name = match name.exp_desc with
  | Var v ->
      begin
        match v.var_desc with
          | SimpleVar n -> ScilabSymbol.symbol_name n = "deff"
          | _ -> false
      end
  | _ -> false

let string_of_function_start arg = match arg.exp_desc with
  | ConstExp constexp ->
      begin
        match constexp with
          | StringExp stringexp -> stringexp.stringExp_value ^ "\n"
          | _ -> failwith "fst arg not string cnst ??"
      end
  | _ -> failwith "fst arg not cnst ??"

let string_of_function_body_exp e = match e.exp_desc with
  | ConstExp constexp ->
      begin
        match constexp with
          | StringExp stringexp -> stringexp.stringExp_value ^ "\n"
          | _ -> failwith "array elt not string cnst ??"
      end
  | _ -> failwith "array elt not cnst ??"

let string_of_function_body arr = match arr.exp_desc with
  | MathExp m ->
      begin
        match m with
          | MatrixExp mexp ->
              Array.fold_left
                (fun acc l ->
                  Array.fold_left (fun acc e ->
                    acc ^ (string_of_function_body_exp e)) acc l.matrixLineExp_columns)
                "" mexp.matrixExp_lines
          | _ -> failwith "array elt not string cnst ??"
      end
  | ConstExp constexp ->
      begin
        match constexp with
          | StringExp stringexp -> stringexp.stringExp_value ^ "\n"
          | _ -> failwith "array elt cnst not str ??"
      end
  | _ -> failwith "array elt not cnst ??"

let rec refactor_deff e = match e.exp_desc with
  | CallExp callexp ->
      if is_deff_call callexp.callExp_name
      then
        begin
          let s = "function " in
          let s = s ^ (string_of_function_start (Array.get callexp.callExp_args 0)) in
          let s = s ^ (string_of_function_body (Array.get callexp.callExp_args 1)) in
          let s = s ^ "endfunction\n\n" in
          print_endline s
        end
  | SeqExp expl -> List.iter refactor_deff expl
  | _ -> ()










