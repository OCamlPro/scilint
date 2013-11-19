open ScilintProject

open ScilabAst
open ScilabSymbol
open ScilintWarning

(* TODO : Use of typed context... make context modulable
   We dont need type nor value here *)

exception IdentExtractError of string * int * int

module SetSy = Set.Make(
  struct
    let compare = ScilabSymbol.compare
    type t = ScilabSymbol.symbol
  end )


module SetSyWithLoc = Set.Make(
  struct
    let compare (sy1, loc1) (sy2, loc2) = ScilabSymbol.compare sy1 sy2
    type t = ScilabSymbol.symbol * ScilabAst.Location.t
  end )


module UnsafeFunSy = Map.Make(
  struct
    let compare = ScilabSymbol.compare
    type t = ScilabSymbol.symbol
  end )

(* To print warning with location *)
let file = ref ""

type state = { mutable escaped_sy : SetSyWithLoc.t ; 
               mutable returned_sy : SetSyWithLoc.t;
               mutable init_sy : SetSy.t; 
               mutable args_sy : SetSyWithLoc.t; 
               mutable used_sy : SetSyWithLoc.t;
               mutable level_fun : int;
               mutable level_for : int }

let new_state level_fun = 
  { escaped_sy = SetSyWithLoc.empty ; returned_sy = SetSyWithLoc.empty;
    init_sy = SetSy.empty; args_sy = SetSyWithLoc.empty; 
    used_sy = SetSyWithLoc.empty;
    level_fun = level_fun; level_for = 0; }

let table_unsafe_fun = ref UnsafeFunSy.empty

let cpt_analyze_fun = ref 0

let get_unused args used =
  SetSyWithLoc.filter (fun (sy, loc) -> not (SetSyWithLoc.mem (sy, loc) used)) args

let add_unsafeFun sy escaped returned =
  table_unsafe_fun := UnsafeFunSy.add sy (escaped, returned) !table_unsafe_fun

let print_extract_error (str, line, cnum) =
  ScilabUtils.print_loc !file line cnum str

let print_assert_error (str, line, cnum) =
  ScilabUtils.print_loc !file line cnum str

let get_location_from_var var =
  (var.var_location.first_line, var.var_location.first_column)

let rec add_used st sy loc =
  st.used_sy <- SetSyWithLoc.add (sy, loc) st.used_sy;
  if not (SetSy.mem sy st.init_sy)
  then st.escaped_sy <- SetSyWithLoc.add (sy, loc) st.escaped_sy;
  if SetSyWithLoc.mem (sy, loc) st.args_sy
  then st.args_sy <- SetSyWithLoc.remove (sy, loc) st.args_sy

let is_return_call e = match e.exp_desc with
  | ControlExp (ReturnExp { returnExp_exp }) ->
      begin
        match returnExp_exp with
          | Some _ -> true
          | _ -> false
      end
  | _ -> false

let exit = Exit
let array_forall f t =
  try
    for i = 0 to Array.length t - 1 do
      if not (f t.(i)) then raise exit
    done;
    true
  with _ -> false

let rec get_assign_ident st e = match e.exp_desc with
  | Var var ->
      begin
        match var.var_desc with
          | ColonVar  ->
              let line, cnum = get_location_from_var var in
              raise (IdentExtractError
                       ("Can't extract ident from COLONVAR", line, cnum))
          | DollarVar ->
              let line, cnum = get_location_from_var var in
              raise (IdentExtractError
                       ("Can't extract ident from DOLLARVAR", line, cnum))
          | SimpleVar symbol -> (symbol, var.var_location)
          | ArrayListVar arr ->
              let line, cnum = get_location_from_var var in
              raise (IdentExtractError
                       ("Can't extract ident from ArrayListVar", line, cnum))
      end
  | FieldExp { fieldExp_head; fieldExp_tail } -> get_assign_ident st fieldExp_head
  | CallExp exp ->
      Array.iter (analyze_ast st) exp.callExp_args;
      begin
        match exp.callExp_name.exp_desc with
          | Var { var_desc = SimpleVar sy; var_location = loc } -> (sy, loc)
          | FieldExp { fieldExp_head; fieldExp_tail } -> 
              let (sy, loc) = get_assign_ident st fieldExp_head in
              add_used st sy loc;
              sy, loc
          | Var _ -> assert false
          | AssignExp _ -> assert false
          | CallExp _ -> assert false
          | CellCallExp _ -> assert false
          | ConstExp _ -> assert false
          | ControlExp _ -> assert false
          | Dec _ -> assert false
          | ListExp _ -> assert false
          | MathExp _ -> assert false
          | SeqExp _ -> assert false
          | ArrayListExp _ -> assert false
          | AssignListExp _ -> assert false
      end
  | AssignExp _ -> assert false 
  | CellCallExp _ -> assert false
  | ConstExp _  -> assert false
  | ControlExp _ -> assert false
  | Dec _  -> assert false
  | ListExp _ -> assert false
  | MathExp _  -> assert false
  | SeqExp _ -> assert false 
  | ArrayListExp _  -> assert false
  | AssignListExp _ -> assert false 

and analyze_ast st e = match e.exp_desc with
  | SeqExp list -> List.iter (analyze_ast st) list
  | ConstExp exp -> ()
  | CallExp exp | CellCallExp exp ->
      begin
        match exp.callExp_name.exp_desc with
          | Var { var_desc = SimpleVar sy; var_location = loc } ->
              let fun_name = symbol_name sy in
              begin match find_function fun_name with
                  FunUnknown -> add_used st sy loc
                |  _ -> ()
              end;
              Array.iter (analyze_ast st) exp.callExp_args;

              begin try

                match fun_name with
                "tlist" ->
                  let max_args =
                    match exp.callExp_args.(0).exp_desc with

                    | MathExp (MatrixExp
                          { matrixExp_lines =
                              [| { matrixLineExp_columns = array } |] } ) when
                      array_forall (fun exp ->
                        match exp.exp_desc with
                         ConstExp (StringExp _) -> true
                        | _ -> false
                      ) array ->

                          Array.length array

                    | MathExp (MatrixExp
                          { matrixExp_lines = array }) when
                        array_forall (fun line ->
                          match line.matrixLineExp_columns with
                            [| { exp_desc = ConstExp (StringExp _) } |] -> true
                          | _ -> false
                        ) array ->
                          Array.length array

                      | Var _ -> max_int
                      | ConstExp (StringExp _) -> 1

                      | _ -> max_int (* assert false *)
                  in
                  if Array.length exp.callExp_args > max_args then
                    local_warning (!file,
                      exp.callExp_args.(max_args).exp_location)
                      (Primitive_with_too_many_arguments (fun_name, max_args));

                | _ ->

                  Array.iteri (fun i arg ->
                    begin match find_argument fun_name i with
                        None -> ()
                      | Some spec ->
                    match spec, arg.exp_desc with
                      StrEnum list,
                      ConstExp (StringExp { stringExp_value = s }) ->
                      let s = ScilabUtils.string_of_string s in
                      if not (List.mem s list) then
                        local_warning (!file, arg.exp_location)
                          (Unexpected_string_argument
                             (fun_name, i, s, list))
                    | StrEnum _, _ -> ()
                    | TooMany, _ ->
                      local_warning (!file, arg.exp_location)
                        (Primitive_with_too_many_arguments (fun_name, i));
                      raise Exit
                end;
              ) exp.callExp_args;
              with Exit -> ()
              end;
          | FieldExp { fieldExp_head; fieldExp_tail } ->
              analyze_ast st fieldExp_head
          (* TODO : tail should be a field of head *)
          | _ -> assert false
      end
  | AssignExp { assignExp_left_exp; assignExp_right_exp } ->
      let arr_sy = match assignExp_left_exp.exp_desc with
        | AssignListExp vars -> Array.map (get_assign_ident st) vars
        | _ -> [| get_assign_ident st assignExp_left_exp |]
      in
      analyze_ast st assignExp_right_exp;
      st.init_sy <- 
        Array.fold_left (fun acc (sy, _) ->  SetSy.add sy acc) st.init_sy arr_sy;
      if is_return_call assignExp_right_exp
      then
        st.returned_sy <- Array.fold_left (fun acc sy ->
          SetSyWithLoc.add sy acc) st.returned_sy arr_sy
  | ControlExp controlExp -> analyze_cntrl st controlExp
  | FieldExp { fieldExp_head; fieldExp_tail } -> ()
  | ListExp { listExp_start; listExp_step; listExp_end } ->
      analyze_ast st listExp_start;
      analyze_ast st listExp_step;
      analyze_ast st listExp_end
  | Var var -> analyze_var st var
  | ArrayListExp array -> Array.iter (analyze_ast st)array
  | AssignListExp array -> Array.iter (analyze_ast st)array
  | MathExp mathExp -> analyze_math st mathExp
  | Dec dec -> analyze_dec st dec

and analyze_cntrl st = function
  | BreakExp -> ()
  | ContinueExp -> ()
  | ForExp forExp ->
      let varDec = forExp.forExp_vardec in (* name, init, kind *)
      let sy = varDec.varDec_name in
      analyze_ast st varDec.varDec_init;
      st.init_sy <- SetSy.add sy st.init_sy;
      analyze_ast st forExp.forExp_body
  | IfExp ifExp ->
      analyze_ast st ifExp.ifExp_test;
      analyze_ast st ifExp.ifExp_then;
      begin
        match ifExp.ifExp_else with
          | Some exp -> analyze_ast st exp
          | None -> ()
      end
  | ReturnExp returnExp ->
      begin
        match returnExp.returnExp_exp with
          | Some exp -> analyze_ast st exp
          | None -> ()
      end
  | SelectExp selectExp ->
      analyze_ast st selectExp.selectExp_selectme;
      Array.iter (fun case_exp ->
        analyze_ast st case_exp.caseExp_test;
        List.iter (analyze_ast st) case_exp.caseExp_body
      ) selectExp.selectExp_cases;
      begin
        match selectExp.selectExp_default with
          | Some (_, se) -> List.iter (analyze_ast st) se
          | None -> ()
      end
  | TryCatchExp tryCatchExp ->
      List.iter (analyze_ast st) tryCatchExp.tryCatchExp_tryme;
      List.iter (analyze_ast st) tryCatchExp.tryCatchExp_catchme
  | WhileExp whileExp ->
      analyze_ast st whileExp.whileExp_test;
      analyze_ast st whileExp.whileExp_body

and analyze_dec st = function
  | VarDec vd -> ()
  | FunctionDec fd ->
      incr cpt_analyze_fun;
      let new_st = new_state (st.level_fun+1) in
      let sy = fd.functionDec_symbol in
      let fun_name = symbol_name sy in
      let body = fd.functionDec_body in
      let args = fd.functionDec_args.arrayListVar_vars in
      let ret = fd.functionDec_returns.arrayListVar_vars in

      let args = Array.map (fun arg ->
          match arg.var_desc with
          | SimpleVar sy_arg ->
            (sy_arg, arg.var_location)
          | _ -> failwith "FunctionDecArgs : Not supposed to happen"
        ) args in
      let ini, args_set =
        Array.fold_left (fun (ini, args_set) (sy_arg, loc) ->
          if SetSy.mem sy_arg ini then
            (* W003 *)
            local_warning (!file, loc)
              (Duplicate_arg sy_arg.symbol_name);
                (SetSy.add sy_arg ini,
                 SetSyWithLoc.add (sy_arg, loc) args_set)
        ) (SetSy.singleton sy, SetSyWithLoc.empty) args in
      let ret_vars = Array.fold_left (fun acc ret_var ->
        match ret_var.var_desc with
          | SimpleVar sy_arg ->
              (* W005 *)
              if SetSy.mem sy_arg ini
              then
                local_warning
                    (!file, ret_var.var_location)
                    (Var_arg_ret sy_arg.symbol_name);
              (* W004 *)
              if SetSyWithLoc.mem (sy_arg, ret_var.var_location) acc
              then begin
                local_warning
                  (!file, ret_var.var_location)
                  (Duplicate_return sy_arg.symbol_name);
                acc
              end else SetSyWithLoc.add (sy_arg, ret_var.var_location) acc
          | _ -> failwith "FunctionDecRet : Not suppose to happen"
      ) SetSyWithLoc.empty ret in
      new_st.init_sy <- ini;
      new_st.args_sy <- args_set;

      begin match ScilintProject.find_function fun_name with
          FunPrimitive ->
          local_warning
            (!file, fd.functionDec_location)
            (Overriding_primitive fun_name)

        | FunDeclared fun_decl ->
          local_warning
            (!file, fd.functionDec_location)
            (Overriding_declared_function (fun_name,fun_decl.fun_loc))

        | FunFile old_file ->
          let is_current_file = try
            Unix.lstat old_file = Unix.lstat !file
          with _ -> true (* Skip warning in case of system error *)
          in
          if not is_current_file then
          local_warning
            (!file, fd.functionDec_location)
            (Overriding_toplevel_function (fun_name, old_file))

        | FunUnknown -> ()

      end;

      if st.level_fun = 0 then begin
        let fun_decl = {
          fun_name = symbol_name sy;
          fun_args = Array.map (fun (sy, _) ->
              symbol_name sy
            ) args;
          fun_loc= (!file, fd.functionDec_location);
        } in
        declare_function fun_decl
      end;

      begin
        try analyze_ast new_st body with
          | IdentExtractError (msg, lnum, cnum) ->
              print_extract_error (msg, lnum, cnum)
          | Assert_failure _ as err -> print_endline (Printexc.to_string err)
      end;
      let unused = get_unused new_st.args_sy new_st.used_sy in
      (* W006 and W007 *)
      SetSyWithLoc.iter (fun (sy, loc_ret) ->
        if SetSyWithLoc.mem (sy, loc_ret) new_st.used_sy
        then
          begin
            (* Ineffective *)
            let (_, loc) = SetSyWithLoc.choose
              (SetSyWithLoc.filter (fun (sy_ret, _) -> sy = sy_ret) new_st.used_sy) in
            local_warning (!file, loc) (Return_as_var sy.symbol_name)
          end;
        if not (SetSy.mem sy new_st.init_sy)
        then
          local_warning (!file, loc_ret) (Unset_ret sy.symbol_name)
      ) ret_vars;
      if (SetSyWithLoc.cardinal unused <> 0)
      then
        (* W002 *)
        SetSyWithLoc.iter (fun (sy, loc) ->
          local_warning (!file, loc) (Unused_arg sy.symbol_name)
        ) unused;
      if (SetSyWithLoc.cardinal new_st.escaped_sy) <> 0 
        || (SetSyWithLoc.cardinal new_st.returned_sy) <> 0
      then
        begin
          (* W001 *)
          SetSyWithLoc.iter (fun (sy, loc) ->
            local_warning (!file, loc) (Uninitialized_var sy.symbol_name)
          ) new_st.escaped_sy;
          add_unsafeFun sy new_st.escaped_sy new_st.returned_sy;
          if st.level_fun <> 0
          then st.init_sy <- SetSy.add sy st.init_sy;
          st.args_sy <- get_unused st.args_sy new_st.used_sy;
        end
      else
        begin
          if st.level_fun <> 0
          then st.init_sy <- SetSy.add sy st.init_sy;
          st.args_sy <- get_unused st.args_sy new_st.used_sy;
        end

and analyze_var st v = match v.var_desc with
  | ColonVar -> ()
  | DollarVar -> ()
  | SimpleVar symbol ->
      add_used st symbol v.var_location;
      st.used_sy <- SetSyWithLoc.add (symbol, v.var_location) st.used_sy;
  | ArrayListVar arr -> Array.iter (analyze_var st) arr

and analyze_math st = function
  | MatrixExp matrixExp -> analyze_matrixExp st matrixExp
  | CellExp matrixExp -> analyze_matrixExp st matrixExp
  | NotExp notExp -> analyze_ast st notExp.notExp_exp
  | OpExp (opExp_Oper, opExp_args) ->
      analyze_ast st opExp_args.opExp_left;
      analyze_ast st opExp_args.opExp_right
  | LogicalOpExp (opLogicalExp_Oper, opExp_args) ->
      analyze_ast st opExp_args.opExp_left;
      analyze_ast st opExp_args.opExp_right
  | TransposeExp transposeExp -> analyze_ast st transposeExp.transposeExp_exp

and analyze_matrixExp st me =
  Array.iter (fun line ->
    Array.iter (fun e -> analyze_ast st e
    ) line.matrixLineExp_columns
  ) me.matrixExp_lines

let analyze fn ast =
  file := fn;
  let st = new_state 0 in
  try analyze_ast st ast with
    | IdentExtractError (msg, lnum, cnum) -> print_extract_error (msg, lnum, cnum)
    | Assert_failure _ as err -> print_endline (Printexc.to_string err)


let print () =
  UnsafeFunSy.iter (fun fsy (esy, rsy) ->
    print_endline (fsy.symbol_name ^ " :");
    SetSyWithLoc.iter (fun (sy, loc) -> print_endline ("  > " ^ sy.symbol_name)) esy;
    SetSyWithLoc.iter (fun (sy, loc) -> print_endline ("  < " ^ sy.symbol_name)) rsy
  ) !table_unsafe_fun


