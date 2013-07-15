open ScilabAst

exception Not_equal of string

let to_string_err_ctrl ctrl = match ctrl with
  | BreakExp -> "BreakExp"
  | ContinueExp -> "ContinueExp"
  | ForExp _ -> "ForExp"
  | IfExp _ -> "IfExp"
  | ReturnExp _ -> "ReturnExp"
  | SelectExp _ -> "SelectExp"
  | TryCatchExp _ -> "TryCatchExp"
  | WhileExp _ -> "WhileExp"

let to_string_err_const cnst = match cnst with
  | BoolExp _ -> "BoolExp"
  | CommentExp _ -> "CommentExp"
  | DoubleExp _ -> "DoubleExp"
  | FloatExp _ -> "FloatExp"
  | IntExp _ -> "IntExp"
  | NilExp -> "NilExp"
  | StringExp _ -> "StringExp"

let to_string_err_var var = match var.var_desc with
  | ColonVar -> "ColonVar"
  | DollarVar -> "DollarVar"
  | SimpleVar _ -> "SimpleVar"
  | ArrayListVar _ -> "ArrayListVar"

let to_string_err_dec d = match d with
  | FunctionDec _ -> "FunctionDec"
  | VarDec _ -> "VarDec"

let to_string_err_mathexp mathexp = match mathexp with
  | MatrixExp _ -> "MatrixExp"
  | CellExp _ -> "CellExp"
  | NotExp _ -> "NotExp"
  | OpExp _ -> "OpExp"
  | LogicalOpExp _ -> "LogicalOpExp"
  | TransposeExp _ -> "TransposeExp"

let to_string_err exp = match exp.exp_desc with
  | AssignExp _ -> "AssignExp"
  | CallExp _ -> "CallExp"
  | CellCallExp _ -> "CellCallExp"
  | ConstExp c -> "ConstExp(" ^ (to_string_err_const c) ^ ")"
  | ControlExp c -> "ControlExp(" ^ (to_string_err_ctrl c) ^ ")"
  | Dec _ -> "Dec"
  | FieldExp _ -> "FieldExp"
  | ListExp _ -> "ListExp"
  | MathExp m -> "MathExp(" ^ (to_string_err_mathexp m) ^ ")"
  | Var _ -> "Var"
  | SeqExp _ -> "SeqExp"
  | ArrayListExp _ -> "ArrayListExp"
  | AssignListExp _ -> "AssignListExp"

let to_string_err_oper op = match op with
  | OpExp_plus -> "plus"
  | OpExp_minus -> "minus"
  | OpExp_times -> "times"
  | OpExp_rdivide -> "rdivide"
  | OpExp_ldivide -> "ldivide"
  | OpExp_power -> "power"
  | OpExp_unaryMinus -> "unaryMinus"
  | OpExp_dottimes -> "dottimes"
  | OpExp_dotrdivide -> "dotrdivide"
  | OpExp_dotldivide -> "dotldivide"
  | OpExp_dotpower -> "dotpower"
  | OpExp_krontimes -> "krontimes"
  | OpExp_kronrdivide -> "kronrdivide"
  | OpExp_kronldivide -> "kronldivide"
  | OpExp_controltimes -> "controltimes"
  | OpExp_controlrdivide -> "controlrdivide"
  | OpExp_controlldivide -> "controlldivide"
  | OpExp_eq -> "eq"
  | OpExp_ne -> "ne"
  | OpExp_lt -> "lt"
  | OpExp_le -> "le"
  | OpExp_gt -> "gt"
  | OpExp_ge -> "ge"

let to_string_err_logoper op = match op with
  | OpLogicalExp_logicalAnd -> "logicalAnd"
  | OpLogicalExp_logicalOr -> "logicalOr"
  | OpLogicalExp_logicalShortCutAnd -> "logicalShortCutAnd"
  | OpLogicalExp_logicalShortCutOr -> "logicalShortCutOr"

let raise_err str1 str2 =
  let msg = str1 ^ " <> " ^ str2 in
  Printf.printf "[%s]\n" msg;
  raise (Not_equal msg)

let rec is_equal_var var1 var2 = match var1.var_desc, var2.var_desc with
  | ColonVar, ColonVar -> true
  | DollarVar, DollarVar -> true
  | SimpleVar v1, SimpleVar v2 ->
      let str1 = ScilabSymbol.symbol_name v1 and
          str2 = ScilabSymbol.symbol_name v2 in
      Printf.printf
        "['%s'(%i) ? '%s'(%i)] = %b %i \n"
        str1
        (String.length str1)
        str2
        (String.length str2)
        (str1 = str2)
        (String.compare str1 str2);
      if str1 = str2
      then true
      else raise_err str1 str2
  | ArrayListVar var_array1, ArrayListVar var_array2 ->
      let length1 = Array.length var_array1 and
          length2 = Array.length var_array2 in
      if length1 = length2
      then
        begin
          Array.iteri (fun i v ->
            ignore (is_equal_var v (Array.get var_array2 i))) var_array1;
          true
        end
      else raise_err "ArrayListVar1 length" "ArrayListVar2 length"
  | _, _ -> raise_err (to_string_err_var var1) (to_string_err_var var2)

let is_equal_constexp const1 const2 = match const1, const2 with
  | BoolExp boolexp1, BoolExp boolexp2 ->
      if boolexp1.boolExp_value = boolexp2.boolExp_value
      then true
      else raise_err "BoolExp1" "BoolExp2"
  | CommentExp commentexp1, CommentExp commentexp2 -> true
  | DoubleExp doubleexp1, DoubleExp doubleexp2 ->
      (* Test in form of strings because once we pretty-print the float we loose precision *)
      let d1 = string_of_float doubleexp1.doubleExp_value and
          d2 = string_of_float doubleexp2.doubleExp_value in
      if d1 = d2 then true
      else
        raise_err d1 d2
  | FloatExp floatexp1, FloatExp floatexp2 ->
      if floatexp1.floatExp_value = floatexp2.floatExp_value
      then true
      else
        raise_err
          (string_of_float floatexp1.floatExp_value)
          (string_of_float floatexp2.floatExp_value)
  | IntExp intexp1, IntExp intexp2 ->
      if intexp1.intExp_value = intexp2.intExp_value
      then true
      else
        raise_err
          (Int32.to_string intexp1.intExp_value)
          (Int32.to_string intexp2.intExp_value)
  | NilExp, NilExp -> true
  | StringExp stringexp1, StringExp stringexp2 ->
      let test str =
        String.iteri (fun i x -> Printf.printf "%i : %i\n" i (Char.code x)) str in
      test stringexp1.stringExp_value;
      test stringexp2.stringExp_value;
      Printf.printf
        "['%s'(%i) ? '%s'(%i)] = %b %i\n"
        stringexp1.stringExp_value
        (String.length stringexp1.stringExp_value)
        stringexp2.stringExp_value
        (String.length stringexp2.stringExp_value)
        (stringexp1.stringExp_value = stringexp2.stringExp_value)
        (String.compare stringexp1.stringExp_value stringexp2.stringExp_value );
      if stringexp1.stringExp_value = stringexp2.stringExp_value
      then true
      else
        raise_err stringexp1.stringExp_value stringexp2.stringExp_value
  | _, _ -> raise_err (to_string_err_const const1) (to_string_err_const const2)

let rec is_equal_exp exp1 exp2 = match exp1.exp_desc, exp2.exp_desc with
    | AssignExp assignexp1, AssignExp assignexp2 ->
        ignore (is_equal_exp assignexp1.assignExp_left_exp assignexp2.assignExp_left_exp);
        is_equal_exp assignexp1.assignExp_right_exp assignexp2.assignExp_right_exp
    | CallExp callexp1, CallExp callexp2 ->
        ignore (is_equal_exp callexp1.callExp_name callexp2.callExp_name);
        let length1 = Array.length callexp1.callExp_args and
            length2 = Array.length callexp2.callExp_args in
        if length1 = length2
        then
          begin
            Array.iteri (fun i e ->
              ignore (
                is_equal_exp e (Array.get callexp2.callExp_args i))
            ) callexp1.callExp_args;
            true
          end
        else raise_err "CallExpArgs1 length" "CallExpArgs length"
    | CellCallExp cellcallexp1, CellCallExp cellcallexp2 ->
        ignore (is_equal_exp cellcallexp1.callExp_name cellcallexp2.callExp_name);
        let length1 = Array.length cellcallexp1.callExp_args and
            length2 = Array.length cellcallexp2.callExp_args in
        if length1 = length2
        then
          begin
            Array.iteri (fun i e ->
              ignore (
                is_equal_exp e (Array.get cellcallexp2.callExp_args i))
            ) cellcallexp1.callExp_args;
            true
          end
        else raise_err "CellCallExpArgs1 length" "CellCallExpArgs length"
    | ConstExp constexp1, ConstExp constexp2 ->
        is_equal_constexp constexp1 constexp2
    | ControlExp controlexp1, ControlExp controlexp2 ->
        is_equal_controlexp controlexp1 controlexp2
    | Dec dec1, Dec dec2 -> is_equal_dec dec1 dec2
    | FieldExp fieldexp1, FieldExp fieldexp2 ->
        ignore (is_equal_exp fieldexp1.fieldExp_head fieldexp2.fieldExp_head);
        is_equal_exp fieldexp1.fieldExp_tail fieldexp2.fieldExp_tail
    | ListExp listexp1, ListExp listexp2 ->
        ignore (is_equal_exp listexp1.listExp_start listexp2.listExp_start);
        ignore (is_equal_exp listexp1.listExp_step listexp2.listExp_step);
        is_equal_exp listexp1.listExp_end listexp2.listExp_end
    | MathExp mathexp1, MathExp mathexp2 -> is_equal_mathexp mathexp1 mathexp2
    | Var var1, Var var2 -> is_equal_var var1 var2
    | SeqExp seqexp1, SeqExp seqexp2 ->
        let length1 = List.length seqexp1 and
            length2 = List.length seqexp2 in
        if length1 = length2
        then
          begin
            List.iteri (fun i e -> ignore (is_equal_exp e (List.nth seqexp2 i))) seqexp1;
            true
          end
        else
          raise_err
            ("SeqExp1 length : " ^ string_of_int length1)
            ("SeqExp2 length : " ^ string_of_int length2)
    | ArrayListExp exp_array1, ArrayListExp exp_array2 ->
        let length1 = Array.length exp_array1 and
            length2 = Array.length exp_array2 in
        if length1 = length2
        then
          begin
            Array.iteri (fun i e ->
              ignore (is_equal_exp e (Array.get exp_array2 i))) exp_array1;
            true
          end
        else raise_err "ArrayListExp1 length" "ArrayListExp2 length"
    | AssignListExp exp_array1, AssignListExp exp_array2 ->
        let length1 = Array.length exp_array1 and
            length2 = Array.length exp_array2 in
        if length1 = length2
        then
          begin
            Array.iteri (fun i e ->
              ignore (is_equal_exp e (Array.get exp_array2 i))) exp_array1;
            true
          end
        else raise_err "AssignListExp1 length" "AssignListExp2 length"
    | _, _ -> raise_err (to_string_err exp1) (to_string_err exp2)

and is_equal_controlexp ctrl1 ctrl2 = match ctrl1, ctrl2 with
  | BreakExp, BreakExp -> true
  | ContinueExp, ContinueExp -> true
  | ForExp forexp1, ForExp forexp2 ->
      ignore (is_equal_vardec forexp1.forExp_vardec forexp2.forExp_vardec);
      is_equal_exp forexp1.forExp_body forexp2.forExp_body
  | IfExp ifexp1, IfExp ifexp2 ->
      begin
        match ifexp1.ifExp_else, ifexp2.ifExp_else with
          | Some e1,Some e2 ->
              ignore (is_equal_exp ifexp1.ifExp_test ifexp2.ifExp_test);
              ignore (is_equal_exp ifexp1.ifExp_then ifexp2.ifExp_then);
              is_equal_exp e1 e2
          | None, None ->
              ignore (is_equal_exp ifexp1.ifExp_test ifexp2.ifExp_test);
              is_equal_exp ifexp1.ifExp_then ifexp2.ifExp_then
          | _, _ -> raise (Not_equal "Differences between else")
      end
  | ReturnExp returnexp1, ReturnExp returnexp2 ->
      begin
        match returnexp1.returnExp_exp, returnexp2.returnExp_exp with
          | Some e1, Some e2 -> is_equal_exp e1 e2
          | None, None -> true
          | _, _ -> raise (Not_equal "Differences between return")
      end
  | SelectExp selectexp1, SelectExp selectexp2 ->
      ignore (
        is_equal_exp
          selectexp1.selectExp_selectme
          selectexp2.selectExp_selectme);
      let length1 = Array.length selectexp1.selectExp_cases and
          length2 = Array.length selectexp2.selectExp_cases in
      if length1 = length2
      then
        begin
          Array.iteri (fun i c1 ->
            let c2 = Array.get selectexp2.selectExp_cases i in
            ignore (is_equal_exp c1.caseExp_test c2.caseExp_test);
            let length1 = List.length c1.caseExp_body and
                length2 = List.length c2.caseExp_body in
            if length1 = length2
            then List.iteri (fun i e ->
              ignore (is_equal_exp e (List.nth c2.caseExp_body i))) c1.caseExp_body
            else raise_err "caseBody1 length" "caseBody2 length";
          ) selectexp1.selectExp_cases
        end
      else raise_err "cases1 length" "cases2 length";
      begin
        match selectexp1.selectExp_default, selectexp2.selectExp_default with
          | Some (_,e1), Some (_,e2) ->
              let length1 = List.length e1 and
                  length2 = List.length e2 in
              if length1 = length2
              then
                begin
                  List.iteri (fun i e ->
                    ignore (is_equal_exp e (List.nth e2 i))) e1;
                  true
                end
              else raise_err "" ""
          | None, None -> true
          | Some _, None -> raise_err "default1 presence" "default2 absence"
          | None, Some _ -> raise_err "default1 absence" "default2 presence"
      end
  | TryCatchExp trycatchexp1, TryCatchExp trycatchexp2 ->
      let listexp1, listexp2 =
        trycatchexp1.tryCatchExp_tryme, trycatchexp2.tryCatchExp_tryme in
      let length1 = List.length listexp1 and
          length2 = List.length listexp2 in
      if length1 = length2
      then
        List.iteri (fun i e -> ignore (is_equal_exp e (List.nth listexp2 i))) listexp1
      else raise_err "tryBody1 length" "tryBody2 length";
      let listexp1, listexp2 =
        trycatchexp1.tryCatchExp_catchme, trycatchexp2.tryCatchExp_catchme in
      let length1 = List.length listexp1 and
          length2 = List.length listexp2 in
      if length1 = length2
      then
        begin
          List.iteri (fun i e -> ignore (is_equal_exp e (List.nth listexp2 i))) listexp1;
          true
        end
      else raise_err "tryBody1 length" "tryBody2 length"
  | WhileExp whileexp1, WhileExp whileexp2 ->
      ignore (is_equal_exp whileexp1.whileExp_test whileexp2.whileExp_test);
      is_equal_exp whileexp1.whileExp_body whileexp2.whileExp_body
  | _, _ -> raise_err (to_string_err_ctrl ctrl1) (to_string_err_ctrl ctrl2)

and is_equal_vardec vd1 vd2 =
  let str1 = ScilabSymbol.symbol_name vd1.varDec_name and
      str2 = ScilabSymbol.symbol_name vd2.varDec_name in
  if str1 <> str2
  then raise_err ("vardec1 :" ^ str1) ("vardec2" ^ str2)
  else is_equal_exp vd1.varDec_init vd2.varDec_init

and is_equal_dec dec1 dec2 = match dec1, dec2 with
  | FunctionDec dec1, FunctionDec dec2 ->
      let is_equal_arraylv arr1 arr2 =
        let length1 = Array.length arr1 and
            length2 = Array.length arr2 in
        if length1 = length2
        then
          Array.iteri (fun i e ->
            ignore (is_equal_var e (Array.get arr2 i))) arr1
        else raise_err "FunArrayListVar1 length" "FunArrayListVar2 length" in
      let fname1 = ScilabSymbol.symbol_name dec1.functionDec_symbol and
          fname2 = ScilabSymbol.symbol_name dec2.functionDec_symbol in
      if fname1 <> fname2
      then raise_err fname1 fname2
      else
        begin
          is_equal_arraylv
            dec1.functionDec_args.arrayListVar_vars
            dec2.functionDec_args.arrayListVar_vars;
          is_equal_arraylv
            dec1.functionDec_returns.arrayListVar_vars
            dec2.functionDec_returns.arrayListVar_vars;
          is_equal_exp dec1.functionDec_body dec2.functionDec_body
        end
  | VarDec d1, VarDec d2 -> is_equal_vardec d1 d2
  | _, _ -> raise_err (to_string_err_dec dec1) (to_string_err_dec dec2)

and is_equal_mathexp mathexp1 mathexp2 = match mathexp1, mathexp2 with
  | MatrixExp matrixexp1, MatrixExp matrixexp2
  | CellExp matrixexp1, CellExp matrixexp2 ->
      let length1 = Array.length matrixexp1.matrixExp_lines and
          length2 = Array.length matrixexp2.matrixExp_lines in
      if length1 = length2
      then
        begin
          Array.iteri (fun i l ->
            ignore (is_equal_lines l (Array.get matrixexp2.matrixExp_lines i))
          ) matrixexp1.matrixExp_lines;
          true
        end
      else raise_err "MatrixExpLines1 length" "MatrixExpLines2 length"
  | NotExp notexp1, NotExp notexp2 ->
      is_equal_exp notexp1.notExp_exp notexp2.notExp_exp
  | OpExp (oper1, args1), OpExp (oper2, args2) ->
      if oper1 = oper2
      then
        begin
          ignore (is_equal_exp args1.opExp_left args2.opExp_left);
          is_equal_exp args1.opExp_right args2.opExp_right
        end
      else raise_err (to_string_err_oper oper1) (to_string_err_oper oper2)
  | LogicalOpExp (oper1,args1), LogicalOpExp (oper2, args2) ->
      if oper1 = oper2
      then
        begin
          ignore (is_equal_exp args1.opExp_left args2.opExp_left);
          is_equal_exp args1.opExp_right args2.opExp_right
        end
      else raise_err (to_string_err_logoper oper1) (to_string_err_logoper oper2)
  | TransposeExp transposeexp1, TransposeExp transposeexp2 ->
      is_equal_exp transposeexp1.transposeExp_exp transposeexp2.transposeExp_exp
  | _, _ ->
      raise_err
        (to_string_err_mathexp mathexp1)
        (to_string_err_mathexp mathexp2)

and is_equal_lines l1 l2 =
  let length1 = Array.length l1.matrixLineExp_columns and
      length2 = Array.length l2.matrixLineExp_columns in
  if length1 = length2
  then
    begin
      Array.iteri (fun i e ->
        ignore (
          is_equal_exp e (Array.get l2.matrixLineExp_columns i))
      ) l1.matrixLineExp_columns;
      true
    end
  else raise_err "MatrixLineExp_col1 length" "MatrixLineExp_col2 length"

let is_equal ast1 ast2 =
  match ast1,ast2 with
    | Exp exp1, Exp exp2 -> is_equal_exp exp1 exp2
    | Decs dec1, Decs dec2 -> (* is_equal_decs dec1 dec2 *) false
    | _ ,_ -> raise (Not_equal "Ast1 <> Ast2")














