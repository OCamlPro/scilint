open ScilabAst

module StatsNode = Map.Make(
  struct
    let compare = Pervasives.compare
    type t = string
  end )

module StatsFun = Map.Make(
  struct
    let compare = Pervasives.compare
    type t = string
  end )

module Stats = Map.Make(
  struct
    let compare = Pervasives.compare
    type t = string
  end )

module StatsCnst = Map.Make(
  struct
    let compare = Pervasives.compare
    type t = string
  end )

module IdentSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = string
  end )

let init_stats () =
  let init_list =
    [ ("CellExp",0);("StringExp",0);("CommentExp",0);
      ("IntExp",0);("FloatExp",0);("DoubleExp",0);("BoolExp",0);
      ("SimpleVar",0);("ColonVar",0);("DollarVar",0);("ArrayListVar",0);
      ("FieldExp",0);("IfExp",0);("TryCatchExp",0);("WhileExp",0);
      ("ForExp",0);("BreakExp",0);("ContinueExp",0);("ReturnExp",0);
      ("SelectExp",0);("CaseExp",0);("SeqExp",0);("ArrayListExp",0);
      ("AssignListExp",0);("NotExp",0);("TransposeExp",0);("VarDec",0);
      ("FunctionDec",0);("ListExp",0);("AssignExp",0);("OpExp",0);
      ("LogicalOpExp",0);("MatrixExp",0);("CallExp",0);("Deff",0);
      ("Execstr",0) ] in
  List.fold_left (fun st (nd,nb) -> StatsNode.add nd nb st) StatsNode.empty init_list

let node_visited = ref 0

let stats = ref (init_stats ())

let stats_id = ref Stats.empty

let stats_fun = ref StatsFun.empty

let stats_cnst = ref StatsCnst.empty

let stats_ret = ref IdentSet.empty

let flag_assign = ref false

let flag_body_assign = ref false

let flag_args = ref false

let flag_return = ref false

let flag_fun = ref false

let flag_fun_body = ref false

let name_fun = ref ""

let update_stats str =
  if StatsNode.mem str !stats
  then
    let cpt = StatsNode.find str !stats in
    let new_stats = StatsNode.remove str !stats in
    incr node_visited;
    StatsNode.add str (cpt + 1) new_stats
  else failwith ("no [" ^ str ^ "] in the map.")

let update_stats_id_call st str =
  if Stats.mem str st
  then
    let (cpt1, cpt2, cpt3) = Stats.find str st in
    let new_stats = Stats.remove str st in
    Stats.add str (cpt1, cpt2, cpt3 + 1) new_stats
  else Stats.add str (0, 0, 1) st

let update_stats_id_var st str =
  if Stats.mem str st
  then
    let (cpt1, cpt2, cpt3) = Stats.find str st in
    let new_stats = Stats.remove str st in
    Stats.add str (cpt1 + 1, cpt2, cpt3) new_stats
  else Stats.add str (1, 0, 0) st

let update_stats_id_fun st str =
  if Stats.mem str st
  then
    let (cpt1, cpt2, cpt3) = Stats.find str st in
    let new_stats = Stats.remove str st in
    Stats.add str (cpt1, cpt2 + 1, cpt3) new_stats
  else Stats.add str (0, 1, 0) st

let update_stats_fun_init st fun_name str =
  if StatsFun.mem fun_name st
  then
    let (l1, l2) = StatsFun.find fun_name st in
    let new_stats = StatsFun.remove fun_name st in
    StatsFun.add fun_name (IdentSet.add str l1, l2) new_stats
  else StatsFun.add fun_name (IdentSet.singleton str, IdentSet.empty) st

let update_stats_fun_use st fun_name str =
  if StatsFun.mem fun_name st
  then
    let (l1, l2) = StatsFun.find fun_name st in
    let new_stats = StatsFun.remove fun_name st in
    StatsFun.add fun_name (l1, IdentSet.add str l2) new_stats
  else StatsFun.add fun_name (IdentSet.empty , IdentSet.singleton str) st

let update_stats_cnst st name flag =
  if StatsCnst.mem name st
  then
    let (cpt1, cpt2) = StatsCnst.find name st in
    let new_stats = StatsCnst.remove name st in
    if flag
    then StatsCnst.add name (cpt1 + 1, cpt2) new_stats
    else StatsCnst.add name (cpt1, cpt2 + 1) new_stats
  else
    if flag
    then StatsCnst.add name (1, 0) st
    else StatsCnst.add name (0, 1) st

let rec analyze_ast e = match e.exp_desc with
  | AssignExp assignexp ->
      stats := update_stats "AssignExp";
      flag_assign := true;
      analyze_ast assignexp.assignExp_left_exp;
      flag_assign := false;
      flag_body_assign := true;
      analyze_ast assignexp.assignExp_right_exp;
      flag_body_assign := false
  | CallExp callexp ->
      analyze_callexp e;
      stats := update_stats "CallExp";
      flag_fun := true;
      analyze_ast callexp.callExp_name;
      flag_fun := false;
      Array.iter analyze_ast callexp.callExp_args
  | CellCallExp cellcallexp ->
      stats := update_stats "CellCallExp";
      analyze_ast cellcallexp.callExp_name;
      Array.iter analyze_ast cellcallexp.callExp_args
  | ConstExp constexp ->
      analyze_const_exp constexp
  | ControlExp controlexp ->
      analyze_control_exp controlexp
  | Dec dec ->
      analyze_dec dec
  | FieldExp fieldexp ->
      stats := update_stats "FieldExp";
      analyze_ast fieldexp.fieldExp_head;
      analyze_ast fieldexp.fieldExp_tail
  | ListExp listexp ->
      stats := update_stats "ListExp";
      analyze_ast listexp.listExp_start;
      analyze_ast listexp.listExp_step;
      analyze_ast listexp.listExp_end
  | MathExp mathexp ->
      analyze_mathexp mathexp
  | Var var ->
      analyze_var var
  | SeqExp seqexp ->
      stats := update_stats "SeqExp";
      List.iter analyze_ast seqexp
  | ArrayListExp exp_array ->
      stats := update_stats "ArrayListExp";
      Array.iter analyze_ast exp_array
  | AssignListExp exp_array -> stats :=
      update_stats "AssignListExp";
      Array.iter analyze_ast exp_array

and analyze_const_exp e = match e with
  | BoolExp boolexp -> stats := update_stats "BoolExp"
  | CommentExp commentexp -> stats := update_stats "CommentExp"
  | DoubleExp doubleexp -> stats := update_stats "DoubleExp"
  | FloatExp floatexp -> stats := update_stats "FloatExp"
  | IntExp intexp -> stats := update_stats "IntExp"
  | NilExp -> stats := update_stats "NilExp"
  | StringExp stringexp -> stats := update_stats "StringExp"


and analyze_control_exp e = match e with
  | BreakExp -> stats := update_stats "BreakExp"
  | ContinueExp -> stats := update_stats "ContinueExp"
  | ForExp forexp ->
      stats := update_stats "ForExp";
      analyze_var_dec forexp.forExp_vardec;
      analyze_ast forexp.forExp_body
  | IfExp ifexp ->
      stats := update_stats "IfExp";
      analyze_ast ifexp.ifExp_test;
      analyze_ast ifexp.ifExp_then;
      begin
        match ifexp.ifExp_else with
          | Some e -> analyze_ast e
          | None -> ()
      end
  | ReturnExp returnexp ->
      stats := update_stats "ReturnExp";
      if !flag_fun_body & !flag_body_assign
      then 
        begin
          stats_ret := IdentSet.add !name_fun !stats_ret;
        end;
      begin
        match returnexp.returnExp_exp with
          | Some e -> analyze_ast e
          | None -> ()
      end
  | SelectExp selectexp ->
      stats := update_stats "SelectExp";
      analyze_ast selectexp.selectExp_selectme;
      Array.iter analyze_case selectexp.selectExp_cases;
      begin
        match selectexp.selectExp_default with
          | Some (_,e) -> List.iter analyze_ast e
          | None -> ()
      end
  | TryCatchExp trycatchexp ->
      stats := update_stats "TryCatchExp";
      List.iter analyze_ast trycatchexp.tryCatchExp_tryme;
      List.iter analyze_ast trycatchexp.tryCatchExp_catchme
  | WhileExp whileexp ->
      stats := update_stats "WhileExp";
      analyze_ast whileexp.whileExp_test;
      analyze_ast whileexp.whileExp_body

and analyze_var_dec e =
  stats := update_stats "VarDec";
  analyze_ast e.varDec_init

and analyze_case e =
  stats := update_stats "CaseExp";
  analyze_ast e.caseExp_test;
  List.iter analyze_ast e.caseExp_body

and analyze_mathexp e = match e with
  | MatrixExp matrixexp ->
      stats := update_stats "MatrixExp";
      analyze_matrix_exp matrixexp
  | CellExp matrixexp ->
      stats := update_stats "CellExp";
      analyze_matrix_exp matrixexp
  | NotExp notexp ->
      stats := update_stats "NotExp";
      analyze_not_exp notexp
  | OpExp (_, args) ->
      stats := update_stats "OpExp";
      analyze_ast args.opExp_left;
      analyze_ast args.opExp_right
  | LogicalOpExp (_, args) ->
      stats := update_stats "LogicalOpExp";
      analyze_ast args.opExp_left;
      analyze_ast args.opExp_right
  | TransposeExp transposeexp ->
      stats := update_stats "TransposeExp";
      analyze_ast transposeexp.transposeExp_exp

and analyze_dec d = match d with
  | FunctionDec fd -> analyze_fundec fd
  | VarDec d -> analyze_var_dec d

and analyze_fundec fd =
  stats := update_stats "FunctionDec";
  stats_id := update_stats_id_fun !stats_id (ScilabSymbol.symbol_name fd.functionDec_symbol);
  name_fun := ScilabSymbol.symbol_name fd.functionDec_symbol;
  flag_args := true;
  Array.iter analyze_var fd.functionDec_args.arrayListVar_vars;
  flag_args := false;
  flag_return := true;
  Array.iter analyze_var fd.functionDec_returns.arrayListVar_vars;
  flag_return := false;
  flag_fun_body := true;
  analyze_ast fd.functionDec_body;
  flag_fun_body := false

and analyze_matrix_exp e =
  Array.iter analyze_matrix_line_exp e.matrixExp_lines

and analyze_matrix_line_exp e =
  Array.iter analyze_ast e.matrixLineExp_columns

and analyze_not_exp e =
  analyze_ast e.notExp_exp

and analyze_var v = match v.var_desc with
  | ColonVar -> stats := update_stats "ColonVar"
  | DollarVar -> stats := update_stats "DollarVar";
  | SimpleVar v ->
      stats := update_stats "SimpleVar";
      if !flag_fun & (ScilabSymbol.symbol_name v) = "deff" then stats := update_stats "Deff";
      if !flag_fun & (ScilabSymbol.symbol_name v) = "execstr" then stats := update_stats "Execstr";
      if !flag_assign then stats_id := update_stats_id_var !stats_id (ScilabSymbol.symbol_name v);
      if !flag_args then stats_id := update_stats_id_var !stats_id (ScilabSymbol.symbol_name v);
      if !flag_return then stats_id := update_stats_id_var !stats_id (ScilabSymbol.symbol_name v);
      if !flag_fun then stats_id := update_stats_id_call !stats_id (ScilabSymbol.symbol_name v);
      if !flag_assign & !flag_fun_body then stats_fun := update_stats_fun_init !stats_fun !name_fun (ScilabSymbol.symbol_name v);
      if (not !flag_fun) & (not !flag_assign) & !flag_fun_body then stats_fun := update_stats_fun_use !stats_fun !name_fun (ScilabSymbol.symbol_name v);
  | ArrayListVar var_array ->
      stats := update_stats "ArrayListVar";
      Array.iter analyze_var var_array


and analyze_callexp e =
  match e.exp_desc with
    | CallExp ce ->
        begin
          let flag_cnst = ref true in
          match ce.callExp_name.exp_desc with
            | Var n ->
                begin
                  match n.var_desc with
                    | SimpleVar v ->
                        let fun_name = ScilabSymbol.symbol_name v in
                        if fun_name = "deff" or fun_name = "execstr"
                        then
                        begin
                          Array.iter (fun arg ->
                            match arg.exp_desc with
                              | ConstExp _ -> ()
                              | _ -> flag_cnst := false
                          ) ce.callExp_args;
                          (* if not !flag_cnst then Printf.printf "%s\n" (ScilabAstPrinter.to_string e); *)
                          stats_cnst := update_stats_cnst !stats_cnst fun_name !flag_cnst
                        end
                    | _ -> ()
                end
            | _ -> ()
        end
    | _ -> ()

let print_stats () =
  let total = ref 0. in
  StatsNode.iter (fun _ v -> total := !total +. (float_of_int v)) !stats;
  Printf.printf "Node visited : %.0f\n" !total;
  StatsNode.iter (fun k v ->
    Printf.printf "%s : %i(%.3f%%)\n" k v (((float_of_int v)/.(!total)) *. 100.)) !stats


let print_stats_id () =
  let nbr_unique_fun_name = ref 0 in
  let nbr_fun_name = ref 0 in
  let nbr_unique_fun_call = ref 0 in
  let nbr_fun_call = ref 0 in
  let nbr_fun_call_guest = ref 0 in
  let len_fun_name_min = ref (900) in
  let len_fun_name_max = ref (-1) in
  let len_total = ref 0 in
  let nbr_ident_fun_body_init = ref 0 in
  let nbr_ident_fun_body_no_init = ref 0 in
  let nbr_ident_fun_body_no_init_unique = ref 0 in
  let nbr_ident_fun_body_use = ref 0 in
  let avr_ident_init = ref 0.0 in
  let nbr_fun_total = ref 0 in
  let str = ref "" in
  Stats.iter (fun k (v1, v2, v3) ->
    (* Printf.printf "%s %i" k (String.length k); *)
    let len = String.length k in
    (* if ((len/4) > 20)  *)
    (* then str := String.sub k 0 80 *)
    (* else str := k ^ (String.make (20 - (len/4)) ' '); *)
    if ((len) > 30)
    then str := String.sub k 0 30
    else str := k ^ (String.make (30 - (len)) ' ');
    if (v1 = 0 & v2 = 1) then
      begin
        incr nbr_unique_fun_name;
        nbr_unique_fun_call := !nbr_unique_fun_call + v3;
        if len < !len_fun_name_min then len_fun_name_min := len;
        if len > !len_fun_name_max then len_fun_name_max := len;
        (* len_total := !len_total + (len/4); *)
        len_total := !len_total + (len);
      end;
    if (v1 = 0 & v2 = 0 & v3 > 0) then nbr_unique_fun_call := !nbr_unique_fun_call + v3;
    if (v1 = 0 & v2 = 1 & v3 > 0 & len > 10) then incr nbr_fun_call_guest;
    nbr_fun_name := !nbr_fun_name + v2;
    nbr_fun_call := !nbr_fun_call + v3;
    Printf.printf "%s | %i | %i | %i\n"
      !str
      v1
      v2
      v3) !stats_id;
  Printf.printf
    "Unique function name : %i(%.3f%%)\n"
    !nbr_unique_fun_name
    (((float_of_int !nbr_unique_fun_name)/.(float_of_int !nbr_fun_name)) *. 100.);
  Printf.printf
    "Min length : %i - Max length %i - Average length : %f\n"
    (!len_fun_name_min/4)
    (!len_fun_name_max/4)
    ((float_of_int !len_total)/.(float_of_int !nbr_unique_fun_name));
  Printf.printf
    "%% call to unique function detected with length : %.3f%%\n"
    (((float_of_int !nbr_fun_call_guest)/.(float_of_int !nbr_unique_fun_name)) *. 100.);
  Printf.printf
    "Call to unique function : %i(%.3f%%)\n"
    !nbr_unique_fun_call
    (((float_of_int !nbr_unique_fun_call)/.(float_of_int !nbr_fun_call)) *. 100.);
  StatsFun.iter (fun k (l1, l2) ->
    nbr_ident_fun_body_init := 0;
    nbr_ident_fun_body_use := 0;
    Printf.printf "%s :\n" k;
    IdentSet.iter (fun x ->
      Printf.printf " -> %s\n" x;
      if IdentSet.exists (fun y -> x = y) l2
      then incr nbr_ident_fun_body_init
      else
        begin
          incr nbr_ident_fun_body_no_init;
          if Stats.mem k !stats_id
          then
            let (v1, v2, v3) = Stats.find x !stats_id in
            (* Printf.printf "%s %i %i %i" k v1 v2 v3; *)
            if v1 = 1 & v2 = 0 then incr nbr_ident_fun_body_no_init_unique;
            if v1 = 0 & v2 = 1 then incr nbr_ident_fun_body_no_init_unique;
        end) l1;
    Printf.printf "\n";
    IdentSet.iter (fun x ->
      Printf.printf " <- %s\n" x;
      incr nbr_ident_fun_body_use) l2;
    if !nbr_ident_fun_body_init <> 0 & !nbr_ident_fun_body_use <> 0 then
      let pourc = (((float_of_int !nbr_ident_fun_body_init)/.(float_of_int !nbr_ident_fun_body_use)) *. 100.) in
      incr nbr_fun_total;
      avr_ident_init := !avr_ident_init +. pourc;
      Printf.printf
        "\n%% ident used in %s that have been initialised in the function body : %f %%\n\n"
        k
        pourc;
    else
      begin
        incr nbr_fun_total;
        Printf.printf
          "\n%% ident used in %s that have been initialised in the function body : %f %%\n\n"
          k
          100.0
      end
  ) !stats_fun;
  Printf.printf
    "\nAverage %% ident used that have been initialised in the function body : %f %%\n\n"
    ((!avr_ident_init)/.(float_of_int !nbr_fun_total));
  Printf.printf
    "\n%% unique ident used that haven't been initialised in the function body : %i %i %f %%\n\n"
    !nbr_ident_fun_body_no_init_unique
    !nbr_ident_fun_body_no_init
    (((float_of_int !nbr_ident_fun_body_no_init_unique)/.(float_of_int !nbr_ident_fun_body_no_init)) *. 100.);
  StatsCnst.iter (fun n (cpt1, cpt2) ->
    Printf.printf "\n %s : %i %i\n" n cpt1 cpt2;
    Printf.printf "%% call with String Constant as argument : %.3f%%" (((float_of_int cpt1)/.(float_of_int (cpt1 + cpt2))) *. 100.);
  ) !stats_cnst;
  Printf.printf "\n%i(%.3f%%) functions use \"[vars] = return()\" statement\n" (IdentSet.cardinal !stats_ret) (((float_of_int (IdentSet.cardinal !stats_ret))/.(float_of_int !nbr_fun_name)) *. 100.)


let print_fun_stats () =
  Printf.printf "\n  ================== Unsafe Functions Stats ===================== \n";
  let cpt_total = !ScilabFunctionAnalyze.cpt_analyze_fun in
  let cpt_fun = ref 0 in
  let cpt_fun_unique = ref 0 in
  let cpt_fun_ret = ref 0 in
  let cpt_escaped_sy = ref 0 in
  let cpt_escaped_sy_unique = ref 0 in
  let cpt_returned_sy = ref 0 in
  let unsf_fun = !ScilabFunctionAnalyze.table_unsafe_fun in
  ScilabFunctionAnalyze.UnsafeFunSy.iter (fun fsy (esc_set, ret_set) ->
    incr cpt_fun;
    let fn = ScilabSymbol.symbol_name fsy in
    if Stats.mem fn !stats_id then
      begin
        let (v1, v2, _) = Stats.find fn !stats_id in
        if (v1 + v2) = 1 then incr cpt_fun_unique;
        ScilabFunctionAnalyze.SetSy.iter (fun sy -> 
          incr cpt_escaped_sy;
          let syn = ScilabSymbol.symbol_name sy in
          if Stats.mem syn !stats_id then
            begin
              let (v1, v2, _) = Stats.find syn !stats_id in
              if (v1 + v2) = 1 then incr cpt_escaped_sy_unique;
            end
          else incr cpt_escaped_sy_unique
        ) esc_set;
        if ScilabFunctionAnalyze.SetSy.cardinal ret_set <> 0 
        then
          begin
            incr cpt_fun_ret;
            ScilabFunctionAnalyze.SetSy.iter (fun sy -> incr cpt_returned_sy) ret_set
          end
      end
    else incr cpt_fun_unique
  ) unsf_fun;
  Printf.printf 
    "\n Unsafe functions : %i out of %i (%.3f%%) // Escaped var \n" 
    !cpt_fun 
    cpt_total 
    (((float_of_int !cpt_fun)/.(float_of_int cpt_total)) *. 100.);
  Printf.printf 
    "\n Unsafe functions : %i out of %i (%.3f%%) // Returned var \n" 
    !cpt_fun_ret 
    cpt_total 
    (((float_of_int !cpt_fun_ret)/.(float_of_int cpt_total)) *. 100.);
  Printf.printf
    "\n Unique named unsafe functions : %i(%.3f%%)\n"
    !cpt_fun_unique
    (((float_of_int !cpt_fun_unique)/.(float_of_int !cpt_fun)) *. 100.);
  Printf.printf
    "\n Unique named escaped symbols : %i(%.3f%%)\n"
    !cpt_escaped_sy_unique
    (((float_of_int !cpt_escaped_sy_unique)/.(float_of_int !cpt_escaped_sy)) *. 100.)
