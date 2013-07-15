open ScilabAst
open ScilabSymbol

(* TODO : Use of typed context... make context modulable 
   We dont need type nor value here *)

module SetSy = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = ScilabSymbol.symbol
  end )
  

module UnsafeFunSy = Map.Make(
  struct
    let compare = Pervasives.compare
    type t = ScilabSymbol.symbol
  end )

(* table of escaped symbol *)
let escaped_sy = ref SetSy.empty

(* table of returned symbol *)
let returned_sy = ref SetSy.empty

(* table of init symbol *)
let init_sy = ref SetSy.empty

(* table of unsafe fun with unsafe sy *)
let table_unsafe_fun = ref UnsafeFunSy.empty

let cpt_analyze_fun = ref 0

let add_unsafeFun sy escaped returned =
  table_unsafe_fun := UnsafeFunSy.add sy (escaped, returned) !table_unsafe_fun

let rec get_assign_ident e = match e.exp_desc with
  | Var var ->
      begin
        match var.var_desc with
          | ColonVar  -> failwith "Can't extract ident from COLONVAR"
          | DollarVar -> failwith "Can't extract ident from DOLLARVAR"
          | SimpleVar symbol -> symbol
          | ArrayListVar arr -> failwith "Can't extract ident from ARRAYLISTVAR"
      end
  | FieldExp { fieldExp_head; fieldExp_tail } -> get_assign_ident fieldExp_head
  | CallExp exp ->  
      begin 
        match exp.callExp_name.exp_desc with
          | Var { var_desc = SimpleVar sy } -> sy
          | _ -> failwith "can't ident from fun name"
      end
  | _ -> failwith "Can't extract ident from this"

let is_return_call e = match e.exp_desc with
  | ControlExp (ReturnExp { returnExp_exp }) -> 
      begin
        match returnExp_exp with
          | Some _ -> true
          | _ -> false
      end
  | _ -> false

let rec analyze_ast e = match e.exp_desc with
  | SeqExp list -> List.iter analyze_ast list
  | ConstExp exp -> ()
  | CallExp exp | CellCallExp exp ->  
      begin
        match exp.callExp_name.exp_desc with
          | Var { var_desc = SimpleVar sy } ->
              if not (SetSy.mem sy !init_sy)
              then escaped_sy := SetSy.add sy !escaped_sy;
              Array.iter analyze_ast exp.callExp_args
          | _ -> failwith "can't extract fun name"
      end
  | AssignExp { assignExp_left_exp; assignExp_right_exp } ->
      let arr_sy = match assignExp_left_exp.exp_desc with
        | AssignListExp vars -> Array.map (get_assign_ident) vars
        | _ -> [| get_assign_ident assignExp_left_exp |]
      in
      analyze_ast assignExp_right_exp;
      init_sy := Array.fold_left (fun acc sy -> SetSy.add sy acc) !init_sy arr_sy;
      if is_return_call assignExp_right_exp
      then 
        returned_sy := Array.fold_left (fun acc sy -> 
          SetSy.add sy acc) !returned_sy arr_sy
  | ControlExp controlExp -> analyze_cntrl controlExp
  | FieldExp { fieldExp_head; fieldExp_tail } -> ()
  | ListExp { listExp_start; listExp_step; listExp_end } -> 
      analyze_ast listExp_start;
      analyze_ast listExp_step;
      analyze_ast listExp_end
  | Var var -> analyze_var var
  | ArrayListExp array -> Array.iter analyze_ast array
  | AssignListExp array -> Array.iter analyze_ast array
  | MathExp mathExp -> analyze_math mathExp
  | Dec dec -> analyze_dec dec

and analyze_cntrl = function
  | BreakExp -> ()
  | ContinueExp -> () 
  | ForExp forExp -> 
      let varDec = forExp.forExp_vardec in (* name, init, kind *)
      let sy = varDec.varDec_name in
      analyze_ast varDec.varDec_init;
      init_sy := SetSy.add sy !init_sy;
      analyze_ast forExp.forExp_body
  | IfExp ifExp ->
      analyze_ast ifExp.ifExp_test;
      analyze_ast ifExp.ifExp_then;
      begin
        match ifExp.ifExp_else with
          | Some exp -> analyze_ast exp
          | None -> ()
      end
  | ReturnExp returnExp -> 
      begin
        match returnExp.returnExp_exp with
          | Some exp -> analyze_ast exp
          | None -> ()
      end
  | SelectExp selectExp -> 
      analyze_ast selectExp.selectExp_selectme;
      Array.iter (fun case_exp ->
        analyze_ast case_exp.caseExp_test;
        List.iter analyze_ast case_exp.caseExp_body
      ) selectExp.selectExp_cases;
      begin
        match selectExp.selectExp_default with
          | Some (_, se) -> List.iter analyze_ast se
          | None -> ()
      end
  | TryCatchExp tryCatchExp -> 
      List.iter analyze_ast tryCatchExp.tryCatchExp_tryme;
      List.iter analyze_ast tryCatchExp.tryCatchExp_catchme
  | WhileExp whileExp -> 
      analyze_ast whileExp.whileExp_test;
      analyze_ast whileExp.whileExp_body

and analyze_dec = function
  | VarDec vd -> ()
  | FunctionDec fd ->
      incr cpt_analyze_fun;
      let cur_escaped_sy = !escaped_sy in
      escaped_sy := SetSy.empty;
      let cur_init_sy = !init_sy in
      init_sy := SetSy.empty;
      let cur_returned_sy = !returned_sy in
      returned_sy := SetSy.empty;
      let sy = fd.functionDec_symbol in
      let body = fd.functionDec_body in
      let args = fd.functionDec_args.arrayListVar_vars in
      init_sy := 
        Array.fold_left (fun acc arg -> 
          match arg.var_desc with
            | SimpleVar sy_arg -> SetSy.add sy_arg acc
            | _ -> failwith "FunctionDecArgs : Not suppose to happen"
        ) SetSy.empty args;
      analyze_ast body;
      if (SetSy.cardinal !escaped_sy) <> 0 || (SetSy.cardinal !returned_sy) <> 0
      then 
        begin 
          add_unsafeFun sy !escaped_sy !returned_sy;
          init_sy := SetSy.add sy cur_init_sy;
          escaped_sy := cur_escaped_sy;
          returned_sy := cur_returned_sy;
        end
      else 
        begin
          init_sy := SetSy.add sy cur_init_sy;
          escaped_sy := cur_escaped_sy;
          returned_sy := cur_returned_sy;
        end
        
and analyze_var v = match v.var_desc with
  | ColonVar -> ()
  | DollarVar -> ()
  | SimpleVar symbol -> 
      if not (SetSy.mem symbol !init_sy)
      then escaped_sy := SetSy.add symbol !escaped_sy
  | ArrayListVar arr -> Array.iter analyze_var arr

and analyze_math = function
  | MatrixExp matrixExp -> analyze_matrixExp matrixExp 
  | CellExp matrixExp ->analyze_matrixExp matrixExp 
  | NotExp notExp -> analyze_ast notExp.notExp_exp
  | OpExp (opExp_Oper, opExp_args) -> 
      analyze_ast opExp_args.opExp_left;
      analyze_ast opExp_args.opExp_right
  | LogicalOpExp (opLogicalExp_Oper, opExp_args) -> 
      analyze_ast opExp_args.opExp_left;
      analyze_ast opExp_args.opExp_right
  | TransposeExp transposeExp -> analyze_ast transposeExp.transposeExp_exp

and analyze_matrixExp me =
  Array.iter (fun line ->
    Array.iter (fun e -> analyze_ast e
    ) line.matrixLineExp_columns
  ) me.matrixExp_lines

let analyze ast =
  init_sy := SetSy.empty;
  escaped_sy := SetSy.empty;
  returned_sy := SetSy.empty;
  analyze_ast ast

let print () =
  print_endline "========= Unsafe Fun =========";
  UnsafeFunSy.iter (fun fsy (esy, rsy) ->
    print_endline (fsy.symbol_name ^ " :");
    SetSy.iter (fun sy -> print_endline ("  > " ^ sy.symbol_name)) esy;
    SetSy.iter (fun sy -> print_endline ("  < " ^ sy.symbol_name)) rsy
  ) !table_unsafe_fun;
  print_endline "=============================="








