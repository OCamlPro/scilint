open ScilabParserAst
open ScilintWarning
open ScilintProject
open ScilabAst

exception IdentExtractError

module SetSy = Set.Make(
  struct
    let compare (sy1, _, _) (sy2, _, _) = compare sy1 sy2
    type t = Parameters.symbol * Parameters.loc * int
  end )

module UnsafeFunSy = Map.Make(
  struct
    let compare = compare
    type t = Parameters.symbol
  end )

let get_file_name f = match f with
  | File n -> n
  | _ -> "non_file_input"

type state = { mutable escaped_sy : SetSy.t ;
               mutable returned_sy : SetSy.t;
               mutable init_sy : SetSy.t;
               mutable args_sy : SetSy.t;
               mutable used_sy : SetSy.t;
               mutable level_fun : int;
               mutable level_for : int;
               mutable for_sy : SetSy.t;
               mutable level_while : int;
               mutable level_control : int;
             }

let new_state level_fun =
  { escaped_sy = SetSy.empty ; returned_sy = SetSy.empty;
    init_sy = SetSy.empty; args_sy = SetSy.empty;
    used_sy = SetSy.empty;
    level_fun = level_fun;
    level_for = 0; for_sy = SetSy.empty;
    level_while = 0;
    level_control = 0;
  }

let print_state st =
  Printf.printf "level_control = %i\n" st.level_control;
  Printf.printf "init :\n";
  SetSy.iter (fun (sy, _, i) ->
    Printf.printf "  - %s(%i)\n" sy i) st.init_sy;
  Printf.printf "\nused :\n";
  SetSy.iter (fun (sy, _, i) ->
    Printf.printf "  - %s(%i)\n" sy i) st.used_sy;
  Printf.printf "\nescaped :\n";
  SetSy.iter (fun (sy, _, i) ->
    Printf.printf "  - %s(%i)\n" sy i) st.escaped_sy;
  Printf.printf "\n"

let is_in_control_level sy set control =
  SetSy.exists (fun (symb, loc, i) -> symb = sy && i = control) set

let merge_state_ife st st1 st2 =
  (* We can merge with intersection to be able to have warning
     on variable init/used in only one branch *)
  st.used_sy <- SetSy.union st1.used_sy st2.used_sy;
  st.init_sy <- SetSy.union st1.init_sy st2.init_sy;
  st.escaped_sy <- SetSy.union st1.escaped_sy st2.escaped_sy;
  st

let merge_state st st1 =
  st.used_sy <- SetSy.union st.used_sy st1.used_sy;
  st.init_sy <- SetSy.union st.init_sy st1.init_sy;
  st.escaped_sy <- SetSy.union st.escaped_sy st1.escaped_sy;
  st

let get_unused args used =
  SetSy.filter (fun (sy, loc, ctrl) -> not (SetSy.mem (sy, loc, ctrl) used)) args

let rec add_used st sy loc =
  st.used_sy <- SetSy.add (sy, loc, st.level_control) st.used_sy;
  if not (SetSy.mem (sy, loc, 0) st.init_sy)
  then st.escaped_sy <- SetSy.add (sy, loc, st.level_control) st.escaped_sy;
  if SetSy.mem (sy, loc, 0) st.args_sy
  then st.args_sy <- SetSy.remove (sy, loc, 0) st.args_sy

let function_call_analysis = Hashtbl.create 113

let rec get_assign_ident state e = match e.cstr with
    | Call (name, args, Field) -> get_assign_ident state name
    | Call (name, args, Tuplified) ->
        let state =
          List.fold_left (fun acc (_, arg) ->
            analyze_exp acc arg) state args in
        begin match name.cstr with
          | Var sym -> (sym.cstr, sym.loc)
          | Call (name, args, Field) ->
              let (sy, loc) = get_assign_ident state name in
              add_used state sy loc;
              (sy, loc)
          | _ -> raise IdentExtractError
        end
    | Var sym -> (sym.cstr, e.loc)
    | Call (name, args, _) ->  raise IdentExtractError
    | Identity args -> raise IdentExtractError
    | Bool _ -> raise IdentExtractError
    | Num f -> raise IdentExtractError
    | String str -> raise IdentExtractError
    | Colon -> raise IdentExtractError
    | Matrix rows -> raise IdentExtractError
    | Range (_, _, _) -> raise IdentExtractError
    | Cell_array rows -> raise IdentExtractError
    | Unop (op, exp) -> raise IdentExtractError
    | Op (op, lexp, rexp) -> raise IdentExtractError
    | Error -> raise IdentExtractError

and analyze_stmt state stmt = match stmt.cstr with
  | Assign ([left], right) ->
      begin
        try
          match left.cstr with
            | Call (name, args, Tuplified) ->
                let state = analyze_exp state right in
                let sy_name, sy_loc = get_assign_ident state name in
                if not (SetSy.mem (sy_name, sy_loc, 0) state.init_sy)
                then state.init_sy <-
                  SetSy.add (sy_name, sy_loc, state.level_control) state.init_sy;
                List.fold_left (fun acc (_, arg) ->
                  analyze_exp acc arg) state args
            | _ ->
                let state = analyze_exp state right in
                let (sy, loc) = get_assign_ident state left in
                if state.level_for <> 0 && SetSy.mem (sy, loc, 0) state.for_sy
                then left.meta <- (loc, Warning (L For_var_modif))::left.meta;
                if is_in_control_level sy state.init_sy state.level_control &&
                  not (SetSy.mem (sy, loc, 0) state.used_sy)
                then left.meta <-
                  (loc, Warning (L (Var_redef_not_used sy)))::left.meta;
                state.init_sy <- SetSy.remove (sy, loc, 0) state.init_sy;
                state.init_sy <-
                  SetSy.add (sy, loc, state.level_control) state.init_sy;
                state
        with IdentExtractError ->
          stmt.meta <-
            (stmt.loc, Warning
              (W ("Not implemented",
                  "Cannot find ident for this statement")))::stmt.meta;
          state
      end
  | Assign (lefts, right) ->
      begin
        try
          let list_sy = List.map (get_assign_ident state) lefts in
          let state = analyze_exp state right in
          state.init_sy <-
            List.fold_left (fun acc (sy, loc) ->
              if state.level_for <> 0 && SetSy.mem (sy, loc, 0) state.for_sy
              then stmt.meta <- (loc, Warning (L For_var_modif))::stmt.meta;
              if is_in_control_level sy acc state.level_control &&
                not (SetSy.mem (sy, loc, 0) state.used_sy)
              then stmt.meta <-
                (loc, Warning (L (Var_redef_not_used sy)))::stmt.meta;
              let acc = SetSy.remove (sy, loc, 0) acc in
              SetSy.add (sy, loc, state.level_control) acc)
            state.init_sy list_sy;
          state
        with IdentExtractError ->
            (* TODO: we should always succeed at finding the assigned exp,
               even if we don't use the information afterwards. *)
          stmt.meta <-
            (stmt.loc, Warning
              (W ("Not implemented",
                  "Cannot find ident for this statement")))::stmt.meta;
          state
      end
  | Seq stmts -> List.fold_left (fun acc stmt -> analyze_stmt acc stmt) state stmts
  | Defun { name ; args ; rets ; body } ->
      let fun_state = new_state (state.level_fun + 1) in
      let args_set =
        List.fold_left (fun acc arg ->
          if SetSy.mem (arg.cstr, arg.loc, 0) acc
          (* W003 *)
          then arg.meta <-
            (arg.loc, Warning (L (Duplicate_arg arg.cstr)))::arg.meta;
          SetSy.add (arg.cstr, arg.loc, state.level_control) acc
        ) SetSy.empty args in
      let ret_set =
        List.fold_left (fun acc arg ->
          if SetSy.mem (arg.cstr, arg.loc, 0) args_set
          (* W005 *)
          then arg.meta <- (arg.loc, Warning (L (Var_arg_ret arg.cstr)))::arg.meta;
          if SetSy.mem (arg.cstr, arg.loc, 0) acc
          (* W004 *)
          then arg.meta <-
            (arg.loc, Warning (L (Duplicate_return arg.cstr)))::arg.meta;
          SetSy.add (arg.cstr, arg.loc, state.level_control) acc
        ) SetSy.empty rets in
      fun_state.init_sy <-
        SetSy.add (name.cstr, name.loc, fun_state.level_control) args_set;
      fun_state.args_sy <- args_set;

      begin match ScilintProject.find_function name.cstr with
          FunPrimitive ->
            name.meta <-
              (name.loc, Warning (L (Overriding_primitive name.cstr)))::name.meta

        | FunDeclared fun_decl ->
            name.meta <-
              (name.loc, Warning (L (Overriding_declared_function (name.cstr, fun_decl.fun_loc))))::name.meta

        | FunFile old_file ->
            let is_current_file =
              try
                Unix.lstat old_file = Unix.lstat (get_file_name (fst name.loc))
              with _ -> true (* Skip warning in case of system error *)
            in
            if not is_current_file then
              name.meta <-
                (name.loc, Warning
                  (L (Overriding_toplevel_function (name.cstr, old_file))))::name.meta

        | FunUnknown -> ()

      end;

      if fun_state.level_fun = 0 then begin
        let fun_decl = {
          fun_name = name.cstr;
          fun_args = Array.of_list (List.map (fun (sy, _, _) -> sy) (SetSy.elements args_set));
          fun_loc = name.loc;
        } in
        declare_function fun_decl
      end;

      let fun_state = analyze_stmt fun_state body in
      (* W016 *)
      SetSy.iter (fun (sy, loc, i) ->
        if sy <> name.cstr
          && not (SetSy.mem (sy, loc, i) fun_state.args_sy)
          && not (SetSy.mem (sy, loc, i) ret_set)
          && not (SetSy.mem (sy, loc, i) fun_state.used_sy)
          && not ((String.get sy 0) = '_')
        then body.meta <- (loc, Warning (L (Var_def_not_used sy)))::body.meta
      ) fun_state.init_sy;
      let unused = get_unused fun_state.args_sy fun_state.used_sy in
      (* W006 and W007 *)
      SetSy.iter (fun (sy, loc_ret, i) ->
        if SetSy.mem (sy, loc_ret, i) fun_state.used_sy
        then
          begin
            (* Ineffective *)
            let (_, loc, _) = SetSy.choose
              (SetSy.filter (fun (sy_ret, _, _) -> sy = sy_ret) fun_state.used_sy) in
            body.meta <- (loc, Warning (L (Return_as_var sy)))::body.meta
          end;
        if not (SetSy.mem (sy, loc_ret, i) fun_state.init_sy)
        then body.meta <- (loc_ret, Warning (L (Unset_ret sy)))::body.meta
      ) ret_set;
      if (SetSy.cardinal unused <> 0)
      then
        (* W002 *)
        SetSy.iter (fun (sy, loc, i) ->
          body.meta <- (loc, Warning (L (Unused_arg sy)))::body.meta
        ) unused;
      if (SetSy.cardinal fun_state.escaped_sy) <> 0
        || (SetSy.cardinal fun_state.returned_sy) <> 0
      then
        begin
          (* W001 *)
          SetSy.iter (fun (sy, loc, i) ->
            body.meta <- (loc, Warning (L (Uninitialized_var sy)))::body.meta
          ) fun_state.escaped_sy;
          if state.level_fun <> 0
          then state.init_sy <- SetSy.add (name.cstr, name.loc, state.level_control) state.init_sy;
          state.args_sy <- get_unused state.args_sy fun_state.used_sy;
          state
        end
      else
        begin
          if state.level_fun <> 0
          then state.init_sy <- SetSy.add (name.cstr, name.loc, state.level_control) state.init_sy;
          state.args_sy <- get_unused state.args_sy fun_state.used_sy;
          state
        end
  | Exp exp -> analyze_exp state exp
  | Break ->
      if state.level_for = 0 && state.level_while = 0
      then stmt.meta <- (stmt.loc, Warning (L Break_outside_loop))::stmt.meta;
      state
  | Continue ->
      if state.level_for = 0 && state.level_while = 0
      then stmt.meta <- (stmt.loc, Warning (L Continue_outside_loop))::stmt.meta;
      state
  | Comment text -> state
  | For (it, range, body) ->
      let sy = it.cstr in
      let loc = it.loc in
      let state = analyze_exp state range in
      state.init_sy <- SetSy.add (sy, loc, state.level_control) state.init_sy;
      state.for_sy <- SetSy.add (sy, loc, state.level_control) state.for_sy;
      state.level_for <- state.level_for + 1;
      state.level_control <- state.level_control + 1;
      let state = analyze_stmt state body in
      state.level_for <- state.level_for - 1;
      state.level_control <- state.level_control - 1;
      state.for_sy <- SetSy.remove (sy, loc, 0) state.for_sy;
      state
  | If (cond, tbody, Some fbody)  ->
      let state = analyze_exp state cond in
      let stthen = { state with level_control = state.level_control + 1 } and
          stelse =  { state with level_control = state.level_control + 1 } in
      let stthen = analyze_stmt stthen tbody in
      let stelse = analyze_stmt stelse fbody in
      merge_state_ife state stthen stelse
  | If (cond, tbody, None)  ->
      let state = analyze_exp state cond in
      let stthen = { state with level_control = state.level_control + 1 } in
      let stelse =  { state with level_control = state.level_control + 1 } in
      let stthen = analyze_stmt stthen tbody in
      merge_state_ife state stthen stelse
  | Return  -> state
  | Select { cond ; cases ; default = None }  ->
      let state = analyze_exp state cond in
      fst (List.fold_left (fun (acc, i) (exp,stm) ->
        let acctest = analyze_exp acc exp in
        let accbody = analyze_stmt {acc with level_control = acc.level_control + i} stm in
        (merge_state acctest accbody, i + 1)
      ) (state, 1) cases)
  | Select { cond ; cases ; default = Some d }  ->
      let lvl = state.level_control in
      let state = analyze_exp state cond in
      let new_state, i = List.fold_left (fun (acc, i) (exp,stm) ->
        let acctest = analyze_exp acc exp in
        let accbody = analyze_stmt { acc with level_control = acc.level_control + i } stm in
        (merge_state acctest accbody, i + 1)
      ) (state, 1) cases in
      let state =
        analyze_stmt { new_state with level_control = new_state.level_control + i } d in
      state.level_control <- lvl;
      state
  | Try (tbody, cbody)  ->
      state.level_control <- state.level_control + 1;
      let state = analyze_stmt state tbody in
      let state = analyze_stmt state cbody in
      state.level_control <- state.level_control - 1;
      state
  | While (cond, body, None)  ->
      let state = analyze_exp state cond in
      state.level_while <- state.level_while + 1;
      state.level_control <- state.level_control + 1;
      let state = analyze_stmt state body in
      state.level_while <- state.level_while - 1;
      state.level_control <- state.level_control - 1;
      state
  | While (cond, body, Some elsebody)  ->
      let state = analyze_exp state cond in
      state.level_while <- state.level_while + 1;
      state.level_control <- state.level_control + 1;
      let state = analyze_stmt state body in
      state.level_while <- state.level_while - 1;
      state.level_control <- state.level_control - 1;
      analyze_stmt state elsebody

and analyze_exp state exp = match exp.cstr with
  | Call (name, args, _) ->
      begin
        match name.cstr with
          | Var sym ->
              let fun_name = sym.cstr in
              begin match find_function fun_name with
                  FunUnknown -> add_used state sym.cstr sym.loc
                |  _ -> ()
              end;
              let state =
                List.fold_left (fun acc (_, arg) ->
                  analyze_exp acc arg) state args in
              begin try
                  List.iteri (fun i (_, arg) ->
                    begin match find_argument fun_name i with
                        None -> ()
                      | Some spec ->
                        match spec, arg.cstr with

                          | StrEnum list, String s  ->
                              if not (List.mem s list) then
                                arg.meta <-
                                  (arg.loc, Warning
                                    (L (Unexpected_string_argument
                                          (fun_name, i, s, list))))::arg.meta
                          | StrEnum _, _ -> ()

                          | Type TString, String _ -> ()
                          | Type TString, Bool _ | Type TString, Num _ ->
                              arg.meta <-
                                (arg.loc, Warning
                                  (L (Unexpected_argument_type
                                          (fun_name, i, "string"))))::arg.meta
                          | Type TString, _ -> ()

                          | Type TInt, Num _ -> ()
                          | Type TInt, Bool _ | Type TInt, String _ ->
                              arg.meta <-
                                (arg.loc, Warning
                                  (L (Unexpected_argument_type
                                        (fun_name, i, "int"))))::arg.meta
                          | Type TInt, _ -> ()

                          | IntEnum (min, max), Num v  ->
                              let v' = int_of_float (v +. 0.1) in
                              if v' < min || v' > max then
                                arg.meta <-
                                  (arg.loc, Warning
                                    (L (Int_argument_out_of_range
                                          (fun_name, i, v, min, max))))::arg.meta
                          | IntEnum _, _ -> ()

                          | TooMany, _ ->
                              arg.meta <-
                                (arg.loc, Warning
                                  (L (Primitive_with_too_many_arguments
                                        (fun_name, i))))::arg.meta;
                              raise Exit

                    end;
                  ) args;
              with Exit -> ()
              end;

              begin try
                let fs = Hashtbl.find_all function_call_analysis fun_name in
                List.iter (fun f -> try
                  f fun_name exp
                with _ -> ()
                ) fs;
                state
              with Not_found -> state
              end;

          | Call (name, args, Field) -> analyze_exp state name
          (* TODO : tail should be a field of head *)
          (* | ArrayListExp _ -> () *) (* TODO:
            dace-scilab/macros/dacefit.sci:92
     92   ij(ll,:) = [(ones(m-k,1) .*. k) (k+1 : m)']; // indices for sparse matrix

           *)
          (* | CallExp _ -> () *) (* TODO:
            cuter/macros/%tabul_e.sci:13
             a(f(k))=a(f(k))(i)
           *)

          (* | MathExp(TransposeExp _) -> () (\* TODO ? *\) *)
          (*
          | ConstExp(DoubleExp _) -> ()
          *)
          | _ ->
              (* TODO: use this to print unexepected expression and debug the parser *)
              (* ScilabUtils.print_warning *)
              (*   (Printf.sprintf "Unexpected CallExp(%s)\n%!" "") *)
              (*   (get_file_name (fst name.loc)) *)
              (*   (loc_from_bounds (snd name.loc)); *)
              state
      (* () *)
      end
  | Var sym when sym.cstr = "$" -> state
  | Var sym -> add_used state sym.cstr sym.loc; state
  | Unop (op, exp) -> analyze_exp state exp
  | Op (op, lexp, rexp) ->
      let state = analyze_exp state lexp in
      analyze_exp state rexp
  | Identity args -> List.fold_left (fun acc arg -> analyze_exp acc arg) state args
  | Matrix contents ->
      List.fold_left (fun acc list ->
        List.fold_left (fun st itm ->
          analyze_exp st itm) acc list.cstr) state contents
  | Range (e1, Some e2, e3) ->
      let state = analyze_exp state e1 in
      let state = analyze_exp state e2 in
      analyze_exp state e3
  | Range (e1, None, e3) ->
      let state = analyze_exp state e1 in
      analyze_exp state e3
  | Cell_array contents ->
      List.fold_left (fun acc list ->
        List.fold_left (fun st itm ->
          analyze_exp st itm) acc list.cstr) state contents
  | Bool _ -> state
  | Num _ -> state
  | String _ -> state
  | Colon -> state
  | Error -> state

let analyze_ast ast =
  let st = new_state 0 in
  ignore (List.fold_left (fun acc stm -> analyze_stmt acc stm) st ast);
  ast

let register () = (* plug it in *)
  ScilintOptions.add_pass
    "analyzes" analyze_ast
    "local warnings generation"
    true
