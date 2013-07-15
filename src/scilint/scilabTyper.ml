open ScilabAst
open ScilabTypedContext
open ScilabFunctionType

exception TypeError
exception TooMuchSolutions


(* Scilab primitives function *)

let string_of_opExp_Oper = function
  | OpExp_plus -> " + "
  | OpExp_minus -> " - "
  | OpExp_times -> " * "
  | OpExp_rdivide -> " / "
  | OpExp_ldivide -> " \\ "
  | OpExp_power -> " ^ "
  | OpExp_unaryMinus -> " - "
  | OpExp_dottimes -> " OpExp_dottimes "
  | OpExp_dotrdivide -> " OpExp_dotrdivide "
  | OpExp_dotldivide -> " OpExp_dotldivide "
  | OpExp_dotpower -> " OpExp_dotpower "
  | OpExp_krontimes -> " OpExp_krontimes "
  | OpExp_kronrdivide -> " OpExp_kronrdivide "
  | OpExp_kronldivide -> " OpExp_kronldivide "
  | OpExp_controltimes -> " OpExp_controltimes "
  | OpExp_controlrdivide -> " OpExp_controlrdivide "
  | OpExp_controlldivide -> " OpExp_controlldivide "
  | OpExp_eq -> " = "
  | OpExp_ne -> " ~= "
  | OpExp_lt -> " < "
  | OpExp_le -> " <= "
  | OpExp_gt -> " > "
  | OpExp_ge -> " >= "

let get_var_scope sy = 
  let b = get_symbol_binding sy in
  match b.binding_global with
    | Some _ -> -1
    | _ ->
        match b.binding_locals with
          | l0 :: _ -> l0.local_scope.scope_level
          | [] -> failwith "scope : Should'nt happen"

let is_var_in_fun sy =
  let ctx = getInstance () in
  let cur_scope = (current_scope ctx).scope_level in
  let var_scope = get_var_scope sy in
  cur_scope = var_scope

let merge_constraints targs trets =
  let ctx = getInstance () in
  Array.map (fun t -> match t with
    | Type_constraint l -> t
    | _ -> t
  ) trets
    

let rec get_stronger_type t1 t2 = match (t1, t2) with
  | Type_base bt1, Type_base bt2  ->
      begin
        match bt1, bt2 with
          | Type_bool, _ -> Type_base bt2
          | Type_real, Type_bool -> Type_base bt1
          | Type_real, Type_real -> Type_base bt1
          | Type_real, Type_complex -> Type_base bt2
          | Type_complex, _ -> Type_base bt1
      end
  | Type_matrix mt1, Type_matrix mt2 -> Type_matrix (get_stronger_type mt1 mt2)
  | Type_string, Type_string -> Type_string
  | _, _ -> failwith ("Matrix' elements have different types : " ^ string_of_t t1 ^ " and " ^ string_of_t t2)

let add_constraint t1 const = match t1 with
  | Type_constraint l -> Type_constraint (const::l)
  | _ -> failwith "can't add constraint to plain type"


let rec generate_constraint_plus ft (sy1, t1) (sy2, t2) = match t1 with
  | Type_base _ | Type_matrix _ | Type_string ->
      begin
        match t2 with
          | Type_base _ | Type_matrix _ | Type_string ->
              (* Filter with the fst argument's type *)
              let list_interm = match_nth_arg 0 t1 ft in
              (* Filter with the snd argument's type *)
              let list = match_nth_arg 1 t2 list_interm in
              let len = List.length list in
              if len = 1
              then 
                let args, rets = get_unique (List.hd list) in
                t1, t2, rets.(0)
              else 
                if len = 0
                then raise TypeError
                else raise TooMuchSolutions
          | Type_constraint l ->
              (* Filter with the fst argument's type *)
              let list = match_nth_arg 0 t1 ft in
              let len = List.length list in
              if len = 1
              then 
                let args, rets = get_unique (List.hd list) in
                t1, args.(1), rets.(0)
              else 
                if len = 0
                then raise TypeError
                else 
                  let arg_pos = get_nth_arg 1 list in
                  let ret_pos = get_ret [(1,sy2)] list in
                  t1, Type_constraint arg_pos, Type_constraint ret_pos
          | _ -> raise TypeError
      end
  | Type_polynome -> raise TypeError
  | Type_arrow (t1, t2) -> raise TypeError
  | Type_null -> raise TypeError
  | Type_constraint l -> 
      begin
        match t2 with
          | Type_base _ | Type_matrix _ | Type_string ->
              (* Filter with the snd argument's type *)
              let list = match_nth_arg 1 t2 ft in
              let len = List.length list in
              if len = 1
              then 
                let args, rets = get_unique (List.hd list) in
                args.(0), t2, rets.(0)
              else 
                if len = 0
                then raise TypeError
                else 
                  let arg_pos = get_nth_arg 0 list in
                  let ret_pos = get_ret [(0,sy1)] list in
                  Type_constraint arg_pos, t2, Type_constraint ret_pos
          | Type_constraint l -> failwith "Not Yet"
          | _ -> raise TypeError
      end
      
let rec generate_constraint_op op (sy1, t1) (sy2, t2) = match op with
  | OpExp_plus ->
      begin
        try generate_constraint_plus ScilabFunctionType.type_plus (sy1, t1) (sy2, t2)
        with TypeError -> 
          failwith ("Can't add " ^ (string_of_t t1) ^ " with " ^ (string_of_t t2))
      end
  | OpExp_minus -> 
      begin
        try generate_constraint_plus ScilabFunctionType.type_plus (sy1, t1) (sy2, t2)
        with TypeError -> 
          failwith ("Can't add " ^ (string_of_t t1) ^ " with " ^ (string_of_t t2))
      end
  | _ -> failwith ("can't generate constraints for " ^ string_of_opExp_Oper op)

      
let rec type_compare_op str_op t1 t2 = match (t1, t2) with
  | Type_base bt1, Type_base bt2  -> 
      begin 
        match bt1, bt2 with
          | Type_real, Type_real -> Type_base Type_bool
          | _, _ -> failwith ("Can't type " ^ string_of_t t1 ^ str_op ^ string_of_t t2)
      end
  | Type_matrix mt1, Type_base bt2 -> type_compare_op str_op mt1 t2
  | Type_base bt1, Type_matrix mt2 -> type_compare_op str_op t1 mt2
  | Type_matrix mt1, Type_matrix mt2 -> type_compare_op str_op mt1 mt2
  | _, _ -> failwith ("Can't type " ^ string_of_t t1 ^ str_op ^ string_of_t t2)

let get_assign_ident e = match e.exp_desc with
  | Var var ->
      begin
        match var.var_desc with
          | ColonVar  -> failwith "Can't extract ident from COLONVAR"
          | DollarVar -> failwith "Can't extract ident from DOLLARVAR"
          | SimpleVar symbol -> symbol
          | ArrayListVar arr -> failwith "Can't extract ident from ARRAYLISTVAR"
      end
  | FieldExp _ -> failwith "Can't extract ident from FielExp"
  | _ -> failwith "Can't extract ident from this"

(* Scilab AST typer *)

exception Not_null

let rec type_exp e = match e.exp_desc with
  | SeqExp list ->
      let l = List.map (fun e -> type_exp e) list in
      List.hd (List.rev l)
  | ConstExp exp -> type_const exp
  | CallExp exp -> type_call exp
  | CellCallExp exp -> type_cell_call exp
  | AssignExp { assignExp_left_exp; assignExp_right_exp } ->
      let arr_sy = match assignExp_left_exp.exp_desc with
        | AssignListExp vars -> Array.map (get_assign_ident) vars
        | _ -> [| get_assign_ident assignExp_left_exp |]
      in
      let arr_typ = match assignExp_right_exp.exp_desc with 
        | ArrayListExp exps -> Array.map (type_exp) exps
        | _ -> [| type_exp assignExp_right_exp |]
      in
      let sy_length = Array.length arr_sy and
          ty_length = Array.length arr_typ in
      if sy_length <> ty_length 
      then failwith "TODO : Warning pour le nombre de vars et de rhs; interp : nvars > 1 => ok ??";
      let ctx = getInstance () in
      Array.iteri (fun i sy -> put ctx sy arr_typ.(i)) arr_sy;
      Type_null
  | ControlExp controlExp -> type_cntrl controlExp
  | Dec dec ->
      type_dec dec;
      Type_null
  | FieldExp { fieldExp_head; fieldExp_tail } -> Type_null
  | ListExp { listExp_start; listExp_step; listExp_end } -> Type_null
  | Var var -> type_var var
  | ArrayListExp array -> Type_null
  | AssignListExp array -> Type_null
  | MathExp mathExp -> type_math mathExp

and type_const = function
  | BoolExp boolExp -> Type_base Type_bool
  | CommentExp commentExp -> Type_null
  | DoubleExp doubleExp -> Type_base Type_real
  | FloatExp floatExp -> Type_base Type_real
  | IntExp intExp -> Type_base Type_real
  | NilExp -> Type_null
  | StringExp stringExp -> Type_string

and type_call e = Type_null

and type_cell_call e = Type_null

and type_cntrl = function
  | BreakExp -> Type_null
  | ContinueExp -> Type_null
  | ForExp forExp -> Type_null
  | IfExp ifExp -> Type_null
  | ReturnExp returnExp -> Type_null
  | SelectExp selectExp -> Type_null
  | TryCatchExp tryCatchExp -> Type_null
  | WhileExp  whileExp -> Type_null

and type_dec = function
  | VarDec vd ->
      let ctx = getInstance () in
      let sy = vd.varDec_name in
      let ty = type_exp vd.varDec_init in
      put ctx sy ty
  | FunctionDec fd ->
      let ctx = getInstance () in
      let sy = fd.functionDec_symbol in
      let args = fd.functionDec_args.arrayListVar_vars in
      let rets = fd.functionDec_returns.arrayListVar_vars in
      let body = fd.functionDec_body in
      begin_scope ctx;
      Array.iter (fun arg -> 
        match arg.var_desc with
          | SimpleVar sy_arg -> put ctx sy_arg (Type_constraint [])
          | _ -> failwith "FunctionDecArgs : Not suppose to happen"
      ) args;
      Array.iter (fun ret -> 
        match ret.var_desc with
          | SimpleVar sy_ret -> put ctx sy_ret (Type_constraint [])
          | _ -> failwith "FunctionDecRetunrs : Not suppose to happen"
      ) rets;
      let ty_body = type_exp body in
      let ty_args = Array.map (fun var ->
        match var.var_desc with
          | SimpleVar a_sy -> get ctx a_sy
          | _ -> failwith "FunctionDecArgs : Not suppose to happen"
      ) args in
      let ty_returns = Array.map (fun var ->
        match var.var_desc with
          | SimpleVar r_sy -> get ctx r_sy
          | _ -> failwith "FunctionDecReturns : Not suppose to happen"
      ) rets in
      if ty_body <> Type_null then print_endline "TODO : WARNING BODY NOT NULL";
      print_endline "=== CTX IN FUN ===\n";
      print_endline (to_string());
      print_endline "==================\n";
      let tfun = merge_constraints ty_args ty_returns in
      end_scope ctx;
      let length_ret = Array.length ty_returns in
      if length_ret = 0
      then 
        let ty_returns = [| Type_null |] in
        put ctx sy (Type_arrow (ty_args, ty_returns))
      else put ctx sy (Type_arrow (ty_args, ty_returns))

and type_var v = match v.var_desc with
  | ColonVar -> failwith "COLONVAR"
  | DollarVar -> failwith "DOLLARVAR"
  | SimpleVar symbol -> 
      let ctx = getInstance () in
      get ctx symbol
  | ArrayListVar arr -> failwith "ARRAYLISTVAR"

and type_math = function
  | MatrixExp matrixExp -> Type_matrix (type_matrixExp matrixExp)
  | CellExp matrixExp -> Type_null
  | NotExp notExp -> Type_null
  | OpExp (opExp_Oper, opExp_args) -> type_opexp opExp_Oper opExp_args
  | LogicalOpExp (opLogicalExp_Oper, opExp_args) -> Type_null
  | TransposeExp transposeExp -> Type_null

and type_matrixExp me =
  let arr_type =
    Array.map (fun line ->
      Array.map (fun e ->
        let t = type_exp e in
        match t with
          | Type_matrix ty -> ty
          | _ -> t
      ) line.matrixLineExp_columns
    ) me.matrixExp_lines in
  let fst_type = Array.get (Array.get arr_type 0) 0 in
  Array.fold_left (fun typ l ->
    Array.fold_left (fun typ_ref typ_elm ->
      get_stronger_type typ_ref typ_elm
    ) typ l
  ) fst_type arr_type

and type_opexp op args = match op with
  | OpExp_plus | OpExp_minus | OpExp_times
  | OpExp_rdivide | OpExp_ldivide | OpExp_power ->
      begin
        let t_left = type_exp args.opExp_left in
        let t_right = type_exp args.opExp_right in
        match t_left, t_right with
          | Type_constraint _, Type_constraint _ ->
              let ctx = getInstance () in
              let sy1 = get_assign_ident args.opExp_left and
                  sy2 = get_assign_ident args.opExp_right in
              let t1, t2, tr = generate_constraint_op op (sy1, t_left) (sy2, t_right) in
              put ctx sy1 t1;
              put ctx sy2 t2;
              tr
          | Type_constraint _, _ ->
              let ctx = getInstance () in
              let sy1 = get_assign_ident args.opExp_left and
                  sy2 = new_symbol "dumb" in
              let t1, _, tr = generate_constraint_op op (sy1, t_left) (sy2, t_right) in
              put ctx sy1 t1;
              tr
          | _, Type_constraint _ -> 
              let ctx = getInstance () in
              let sy1 = new_symbol "dumb" and
                  sy2 = get_assign_ident args.opExp_right in
              let _, t2, tr = generate_constraint_op op (sy1, t_left) (sy2, t_right) in
              put ctx sy2 t2;
              tr
          | _, _ -> 
              let sy_dumb = new_symbol "dumb" in
              let _, _, tr = generate_constraint_op op (sy_dumb, t_left) (sy_dumb, t_right) in
              tr
      end
  (* unaryMinus have a dump expression as left operand *)
  | OpExp_unaryMinus -> 
      let t_right = type_exp args.opExp_right in 
      (*type_unary_minus t_right*)
      t_right
  | OpExp_eq | OpExp_ne | OpExp_lt 
  | OpExp_le | OpExp_gt | OpExp_ge ->
      let t_left = type_exp args.opExp_left in
      let t_right = type_exp args.opExp_right in
      (* let t1, t2, tr = generate_constraint_op op t_left t_right in *)
      (* tr *)
      t_left
  | _ -> failwith "OpExp TODO"

let type_ast e =
  let typ = type_exp e in
  print_endline (string_of_t typ);
  print_endline (to_string ())


















