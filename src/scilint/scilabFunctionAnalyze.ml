open ScilabAst
open ScilabSymbol

(* TODO : Use of typed context... make context modulable 
   We dont need type nor value here *)

type warning_ty = 
  | Uninitialized_var of string (* W001 *)
  | Unused_arg of string (* W002 *)
  | Duplicate_arg of string (* W003 *)
  | Duplicate_return of string (* W004 *)
  | Var_arg_ret of string (* W005 *)
  | Unset_ret of string (* W006 *)
  | Return_as_var of string (* W007 *)

type code = string

type message = string

type warning_loc = string * ScilabAst.Location.t

type warning = code * message * warning_loc

let create_warning loc = function
  | Uninitialized_var s -> ("W001", "\"" ^ s ^ "\" not initialized\n", loc)
  | Unused_arg s -> ("W002", "\"" ^ s ^ "\" not used\n", loc)
  | Duplicate_arg s -> ("W003", "argument \"" ^ s ^ "\" appears several times\n", loc)
  | Duplicate_return s -> ("W004", "return variable \"" ^ s ^ "\" appears several times\n", loc)
  | Var_arg_ret s -> ("W005", "return variable \"" ^ s ^ "\" is also an argument\n", loc)
  | Unset_ret s -> ("W006", "return variable \"" ^ s ^ "\" is never set\n", loc)
  | Return_as_var s -> ("W007", "return variable \"" ^ s ^ "\" is used as a local variable\n", loc)


module SetSy = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = ScilabSymbol.symbol
  end )


module SetSyWithLoc = Set.Make(
  struct
    let compare (sy1, loc1) (sy2, loc2) = Pervasives.compare sy1 sy2
    type t = ScilabSymbol.symbol * ScilabAst.Location.t
  end )
  

module UnsafeFunSy = Map.Make(
  struct
    let compare = Pervasives.compare
    type t = ScilabSymbol.symbol
  end )

(* To print warning with location *)
let file = ref ""

(* table of escaped symbol *)
let escaped_sy = ref SetSyWithLoc.empty

(* table of returned symbol *)
let returned_sy = ref SetSyWithLoc.empty

(* table of init symbol *)
let init_sy = ref SetSy.empty

(* table of used args *)
let args_sy = ref SetSyWithLoc.empty

let used_sy = ref SetSyWithLoc.empty

(* table of unsafe fun with unsafe sy *)
let table_unsafe_fun = ref UnsafeFunSy.empty

let cpt_analyze_fun = ref 0

let level = ref 0

let print_warning (code, msg, (file, loc)) = 
  ScilabUtils.print_warning (code ^ " : " ^ msg) file loc

let get_unused args used =
  SetSyWithLoc.filter (fun (sy, loc) -> not (SetSyWithLoc.mem (sy, loc) used)) args

let add_unsafeFun sy escaped returned =
  table_unsafe_fun := UnsafeFunSy.add sy (escaped, returned) !table_unsafe_fun

let rec get_assign_ident e = match e.exp_desc with
  | Var var ->
      begin
        match var.var_desc with
          | ColonVar  -> failwith "Can't extract ident from COLONVAR"
          | DollarVar -> failwith "Can't extract ident from DOLLARVAR"
          | SimpleVar symbol -> (symbol, var.var_location)
          | ArrayListVar arr -> failwith "Can't extract ident from ARRAYLISTVAR"
      end
  | FieldExp { fieldExp_head; fieldExp_tail } -> get_assign_ident fieldExp_head
  | CallExp exp ->  
      begin 
        match exp.callExp_name.exp_desc with
          | Var { var_desc = SimpleVar sy; var_location = loc } -> (sy, loc)
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
          | Var { var_desc = SimpleVar sy; var_location = loc } ->
              used_sy := SetSyWithLoc.add (sy, loc) !used_sy;
              if not (SetSy.mem sy !init_sy)
              then escaped_sy := SetSyWithLoc.add (sy, loc) !escaped_sy;
              if SetSyWithLoc.mem (sy, loc) !args_sy
              then args_sy := SetSyWithLoc.remove (sy, loc) !args_sy;
              Array.iter analyze_ast exp.callExp_args
          | _ -> failwith "can't extract fun name"
      end
  | AssignExp { assignExp_left_exp; assignExp_right_exp } ->
      let arr_sy = match assignExp_left_exp.exp_desc with
        | AssignListExp vars -> Array.map (get_assign_ident) vars
        | _ -> [| get_assign_ident assignExp_left_exp |]
      in
      analyze_ast assignExp_right_exp;
      init_sy := Array.fold_left (fun acc (sy, _) -> SetSy.add sy acc) !init_sy arr_sy;
      if is_return_call assignExp_right_exp
      then 
        returned_sy := Array.fold_left (fun acc sy -> 
          SetSyWithLoc.add sy acc) !returned_sy arr_sy
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
      incr level;
      incr cpt_analyze_fun;
      let cur_escaped_sy = !escaped_sy in
      escaped_sy := SetSyWithLoc.empty;
      let cur_init_sy = !init_sy in
      let cur_returned_sy = !returned_sy in
      returned_sy := SetSyWithLoc.empty;
      let cur_args_sy = !args_sy in
      let cur_used_sy = !used_sy in
      used_sy := SetSyWithLoc.empty;
      let sy = fd.functionDec_symbol in
      let body = fd.functionDec_body in
      let args = fd.functionDec_args.arrayListVar_vars in
      let ret = fd.functionDec_returns.arrayListVar_vars in
      let ini, args = 
        Array.fold_left (fun (acc1, acc2) arg -> 
          match arg.var_desc with
            | SimpleVar sy_arg -> 
                if SetSy.mem sy_arg acc1
                then 
                  begin
                    (* W003 *)
                    let w = create_warning 
                      (!file, arg.var_location) 
                      (Duplicate_arg sy_arg.symbol_name) in
                    print_warning w
                  end;
                (SetSy.add sy_arg acc1, SetSyWithLoc.add (sy_arg, arg.var_location) acc2)
            | _ -> failwith "FunctionDecArgs : Not suppose to happen"
        ) (SetSy.singleton sy, SetSyWithLoc.empty) args in
      let ret_vars = Array.fold_left (fun acc ret_var -> 
        match ret_var.var_desc with
          | SimpleVar sy_arg ->
              (* W005 *)
              if SetSy.mem sy_arg ini
              then 
                begin
                  let w = create_warning 
                    (!file, ret_var.var_location) 
                    (Var_arg_ret sy_arg.symbol_name) in
                  print_warning w
                end;
              (* W004 *)
              if SetSyWithLoc.mem (sy_arg, ret_var.var_location) acc
              then 
                let w = create_warning 
                  (!file, ret_var.var_location) 
                  (Duplicate_return sy_arg.symbol_name) in
                print_warning w;
                acc
              else SetSyWithLoc.add (sy_arg, ret_var.var_location) acc
          | _ -> failwith "FunctionDecRet : Not suppose to happen"
      ) SetSyWithLoc.empty ret in
      init_sy := SetSy.union !init_sy ini;
      args_sy := args;
      analyze_ast body;
      let unused = get_unused !args_sy !used_sy in
      (* W006 and W007 *)
      SetSyWithLoc.iter (fun (sy, loc_ret) ->
        if SetSyWithLoc.mem (sy, loc_ret) !used_sy
        then
          begin
            (* Ineffective *)
            let (_, loc) = SetSyWithLoc.choose 
              (SetSyWithLoc.filter (fun (sy_ret, _) -> sy = sy_ret) !used_sy) in
            let w = create_warning (!file, loc) (Return_as_var sy.symbol_name) in
            print_warning w
          end;
        if not (SetSy.mem sy !init_sy)
        then
          let w = create_warning (!file, loc_ret) (Unset_ret sy.symbol_name) in
          print_warning w
      ) ret_vars;
      if (SetSyWithLoc.cardinal unused <> 0) 
      then 
        begin
          (* W002 *)
          let list_w = SetSyWithLoc.fold (fun (sy, loc) acc -> 
            let w = create_warning (!file, loc) (Unused_arg sy.symbol_name) in
            w::acc) unused [] in
          List.iter print_warning list_w
        end;
      if (SetSyWithLoc.cardinal !escaped_sy) <> 0 || (SetSyWithLoc.cardinal !returned_sy) <> 0
      then 
        begin 
          (* W001 *)
          let list_w = SetSyWithLoc.fold (fun (sy, loc) acc -> 
            let w = 
              create_warning (!file, loc) (Uninitialized_var sy.symbol_name) in
            w::acc) !escaped_sy [] in
          decr level;
          List.iter print_warning list_w;
          add_unsafeFun sy !escaped_sy !returned_sy;
          if !level <> 0 
          then init_sy := SetSy.add sy cur_init_sy
          else init_sy := cur_init_sy;
          args_sy := get_unused cur_args_sy !used_sy;
          escaped_sy := cur_escaped_sy;
          returned_sy := cur_returned_sy;
          used_sy := cur_used_sy;
        end
      else 
        begin
          decr level;
          if !level <> 0 
          then init_sy := SetSy.add sy cur_init_sy
          else init_sy := cur_init_sy;
          escaped_sy := cur_escaped_sy;
          returned_sy := cur_returned_sy;
          args_sy := get_unused cur_args_sy !used_sy;
          used_sy := cur_used_sy;
        end
        
and analyze_var v = match v.var_desc with
  | ColonVar -> ()
  | DollarVar -> ()
  | SimpleVar symbol -> 
      if not (SetSy.mem symbol !init_sy)
      then escaped_sy := SetSyWithLoc.add (symbol, v.var_location) !escaped_sy;
      if SetSyWithLoc.mem (symbol, v.var_location) !args_sy
      then args_sy := SetSyWithLoc.remove (symbol, v.var_location) !args_sy;
      used_sy := SetSyWithLoc.add (symbol, v.var_location) !used_sy;
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

let analyze fn ast =
  file := fn;
  init_sy := SetSy.empty;
  escaped_sy := SetSyWithLoc.empty;
  returned_sy := SetSyWithLoc.empty;
  analyze_ast ast

let print () =
  UnsafeFunSy.iter (fun fsy (esy, rsy) ->
    print_endline (fsy.symbol_name ^ " :");
    SetSyWithLoc.iter (fun (sy, loc) -> print_endline ("  > " ^ sy.symbol_name)) esy;
    SetSyWithLoc.iter (fun (sy, loc) -> print_endline ("  < " ^ sy.symbol_name)) rsy
  ) !table_unsafe_fun








