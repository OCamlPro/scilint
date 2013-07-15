open ScilabAst
open Format

let is_block_comment str =
  String.contains str '\n'

let pprint_constexp buf indent constexp = match constexp with
  | BoolExp boolexp ->
      if boolexp.boolExp_value
      then Printf.bprintf buf "%s%%t" indent
      else Printf.bprintf buf "%s%%f" indent
  | CommentExp commentexp ->
      let cmt = commentexp.commentExp_comment in
      if is_block_comment cmt
      then Printf.bprintf buf "%s/* %s */" indent cmt
      else Printf.bprintf buf "%s// %s \n" indent cmt
  | DoubleExp doubleexp ->
      if doubleexp.doubleExp_value = infinity
      then
        (* Hack for infinity, put a dumb high enough value *)
        Printf.bprintf buf "%s%s" indent "1E+310"
      else
        Printf.bprintf buf "%s%s" indent (string_of_float doubleexp.doubleExp_value)
  | FloatExp floatexp ->
      Printf.bprintf buf "%s%s" indent (string_of_float floatexp.floatExp_value)
  | IntExp intexp ->
      Printf.bprintf buf "%s%s" indent (Int32.to_string intexp.intExp_value)
  | NilExp ->
      Printf.bprintf buf "%sNil" indent
  | StringExp stringexp ->
      Printf.bprintf buf "%s\"%s\"" indent stringexp.stringExp_value


let pprint_logicalOpExpOp buf oper = match oper with
  | OpLogicalExp_logicalAnd -> Printf.bprintf buf " & "
  | OpLogicalExp_logicalOr -> Printf.bprintf buf " | "
  | OpLogicalExp_logicalShortCutAnd -> Printf.bprintf buf " && "
  | OpLogicalExp_logicalShortCutOr -> Printf.bprintf buf " || "


let rec pprint_exp buf indent ast = match ast.exp_desc with
  | AssignExp assignexp ->
      pprint_exp buf indent assignexp.assignExp_left_exp;
      Printf.bprintf buf "=" ;
      pprint_exp buf "" assignexp.assignExp_right_exp
  | CallExp callexp ->
      pprint_exp buf indent callexp.callExp_name;
      Printf.bprintf buf "(";
      pprint_call_args buf callexp.callExp_args;
      Printf.bprintf buf ")"
  | CellCallExp cellcallexp ->
      pprint_exp buf indent cellcallexp.callExp_name;
      Printf.bprintf buf "{";
      pprint_call_args buf cellcallexp.callExp_args;
      Printf.bprintf buf "}"
  | ConstExp constexp -> pprint_constexp buf indent constexp
  | ControlExp controlexp -> pprint_controlexp buf indent controlexp
  | Dec dec -> pprint_dec buf indent dec
  | FieldExp fieldexp ->
      pprint_exp buf indent fieldexp.fieldExp_head;
      Printf.bprintf buf ".";
      pprint_exp buf "" fieldexp.fieldExp_tail
  | ListExp listexp ->
      Printf.bprintf buf "%s(" indent;
      pprint_exp buf "" listexp.listExp_start;
      Printf.bprintf buf ":";
      pprint_exp buf "" listexp.listExp_step;
      Printf.bprintf buf ":";
      pprint_exp buf "" listexp.listExp_end;
      Printf.bprintf buf ")";
  | MathExp mathexp ->  pprint_mathexp buf indent mathexp
  | Var var -> pprint_var buf indent var
  | SeqExp seqexp -> pprint_list buf indent seqexp
  | ArrayListExp exp_array -> pprint_array buf indent exp_array
  | AssignListExp exp_array ->
      let length = Array.length exp_array in
      Printf.bprintf buf "%s[" indent;
      Array.iteri (fun index a ->
        if index = length - 1
        then
          pprint_exp buf "" a
        else
          begin
            pprint_exp buf "" a;
            Printf.bprintf buf ", "
          end ) exp_array;
      Printf.bprintf buf "]"

and pprint_controlexp buf indent controlexp = match controlexp with
  | BreakExp -> Printf.bprintf buf "%sbreak" indent
  | ContinueExp -> Printf.bprintf buf "%scontinue" indent
  | ForExp forexp ->
      Printf.bprintf buf "%sfor " indent;
      pprint_var_dec buf "" forexp.forExp_vardec;
      Printf.bprintf buf ";\n";
      pprint_exp buf (indent ^ "  ") forexp.forExp_body;
      Printf.bprintf buf "\n%send" indent
  | IfExp ifexp ->
      begin
        match ifexp.ifExp_else with
          | Some e ->
              Printf.bprintf buf "%sif " indent;
              pprint_exp buf "" ifexp.ifExp_test;
              Printf.bprintf buf " then\n";
              pprint_exp buf (indent ^ "  ") ifexp.ifExp_then;
              Printf.bprintf buf "\n%selse\n" indent;
              pprint_exp buf (indent ^ "  ") e;
              Printf.bprintf buf "\n%send" indent
          | None ->
              Printf.bprintf buf "%sif " indent;
              pprint_exp buf "" ifexp.ifExp_test;
              Printf.bprintf buf " then\n";
              pprint_exp buf (indent ^ "  ") ifexp.ifExp_then;
              Printf.bprintf buf "\n%send" indent
      end
  | ReturnExp returnexp ->
      begin
        match returnexp.returnExp_exp with
          | Some e ->
              Printf.bprintf buf "%sreturn (" indent;
              pprint_exp buf indent e;
              Printf.bprintf buf ")"
          | None -> Printf.bprintf buf "%sreturn" indent;
      end
  | SelectExp selectexp ->
      let pprint_cases buf indent arr =
        Array.iter (fun c ->
          Printf.bprintf buf "%scase " indent;
          pprint_exp buf "" c.caseExp_test;
          Printf.bprintf buf ";\n";
          pprint_list buf (indent ^ "  ") c.caseExp_body;
          Printf.bprintf buf "\n") arr in
      let pprint_default buf indent def =
        Printf.bprintf buf "%sotherwise;\n" indent;
        pprint_list buf (indent ^ "  ") def;
        Printf.bprintf buf "\n" in
      begin
        match selectexp.selectExp_default with
          | Some (_,e) ->
              Printf.bprintf buf "%sselect " indent;
              pprint_exp buf "" selectexp.selectExp_selectme;
              Printf.bprintf buf "\n";
              pprint_cases buf (indent ^ "  ") selectexp.selectExp_cases;
              pprint_default buf (indent ^ "  ") e;
              Printf.bprintf buf "%send" indent
          | None ->
              Printf.bprintf buf "%sselect " indent;
              pprint_exp buf "" selectexp.selectExp_selectme;
              Printf.bprintf buf "\n";
              pprint_cases buf (indent ^ "  ") selectexp.selectExp_cases;
              Printf.bprintf buf "%send" indent
      end
  | TryCatchExp trycatchexp ->
      Printf.bprintf buf "%stry\n" indent;
      pprint_list buf (indent ^ "  ") trycatchexp.tryCatchExp_tryme;
      Printf.bprintf buf "\n%scatch\n" indent;
      pprint_list buf (indent ^ "  ") trycatchexp.tryCatchExp_catchme;
      Printf.bprintf buf "\n%send" indent;
  | WhileExp whileexp ->
      Printf.bprintf buf "%swhile " indent;
      pprint_exp buf "" whileexp.whileExp_test;
      Printf.bprintf buf " do\n";
      pprint_exp buf (indent ^ "  ") whileexp.whileExp_body;
      Printf.bprintf buf "\n%send" indent


and pprint_dec buf indent dec = match dec with
    | FunctionDec fd ->
        let pprint_args buf args =
          let length = Array.length args in
          Printf.bprintf buf "(";
          Array.iteri (fun index a ->
            if index = length - 1
            then
              pprint_var buf "" a
            else
              begin
                pprint_var buf "" a;
                Printf.bprintf buf ", "
              end ) args;
          Printf.bprintf buf ")" in
        let pprint_returns buf args =
          let length = Array.length args in
          Printf.bprintf buf "[";
          Array.iteri (fun index a ->
            if index = length - 1
            then
              pprint_var buf "" a
            else
              begin
                pprint_var buf "" a;
                Printf.bprintf buf ", "
              end ) args;
          Printf.bprintf buf "]" in
        Printf.bprintf buf "%sfunction " indent;
        pprint_returns buf fd.functionDec_returns.arrayListVar_vars;
        Printf.bprintf buf " = %s " (ScilabSymbol.symbol_name fd.functionDec_symbol);
        pprint_args buf fd.functionDec_args.arrayListVar_vars;
        Printf.bprintf buf "\n";
        pprint_exp buf (indent ^ "  ") fd.functionDec_body;
        Printf.bprintf buf "\n%sendfunction" indent
    | VarDec d -> pprint_var_dec buf indent d

and pprint_var_dec buf indent vd =
  Printf.bprintf buf "%s%s = " indent (ScilabSymbol.symbol_name vd.varDec_name);
  pprint_exp buf "" vd.varDec_init

and pprint_call_args buf arr =
  let length = Array.length arr in
  Array.iteri (fun index e ->
    if index = length - 1
    then pprint_exp buf "" e
    else
      begin
        pprint_exp buf "" e;
        Printf.bprintf buf ", "
      end ) arr

and pprint_list buf indent l =
  let length = List.length l in
  List.iteri (fun index e ->
    if index = length - 1
    then pprint_exp buf indent e
    else
      begin
        pprint_exp buf indent e;
        Printf.bprintf buf ";\n"
      end ) l

and pprint_array buf indent arr =
  let length = Array.length arr in
  Array.iteri (fun index e ->
    if index = length - 1
    then pprint_exp buf indent e
    else
      begin
        pprint_exp buf indent e;
        Printf.bprintf buf ";\n"
      end ) arr

and pprint_var buf indent var = match var.var_desc with
  | ColonVar -> Printf.bprintf buf "%s:" indent
  | DollarVar -> Printf.bprintf buf "%s$" indent
  | SimpleVar v -> Printf.bprintf buf "%s%S" indent (ScilabAstPrinter.string_of_unicode (ScilabSymbol.symbol_name v))
  | ArrayListVar var_array ->
      let length = Array.length var_array in
      Printf.bprintf buf "[";
      Array.iteri (fun index a ->
        if index = length - 1
        then
          pprint_var buf "" a
        else
          begin
            pprint_var buf "" a;
            Printf.bprintf buf ", "
          end ) var_array;
      Printf.bprintf buf "]"

and pprint_mathexp buf indent mathexp = match mathexp with
  | MatrixExp matrixexp ->
      let pprint_lines buf indent list =
        let length = Array.length list in
        Array.iteri (fun i l ->
          if i = length - 1
          then pprint_matrixLineExp buf indent l
          else
            begin
              pprint_matrixLineExp buf indent l;
              Printf.bprintf buf "; "
            end
        ) list in
      Printf.bprintf buf "%s[" indent;
      pprint_lines buf (indent ^ " ") matrixexp.matrixExp_lines;
      Printf.bprintf buf "]"
  | CellExp matrixexp ->
      let pprint_lines buf indent list =
        let length = Array.length list in
        Array.iteri (fun i l ->
          if i = length - 1
          then pprint_matrixLineExp buf indent l
          else
            begin
              pprint_matrixLineExp buf indent l;
              Printf.bprintf buf ";\n"
            end
        ) list in
      Printf.bprintf buf "%s{" indent;
      pprint_lines buf (indent ^ " ") matrixexp.matrixExp_lines;
      Printf.bprintf buf "}"
  | NotExp notexp ->
      Printf.bprintf buf "%s~(" indent;
      pprint_exp buf "" notexp.notExp_exp;
      Printf.bprintf buf ")"
  | OpExp (oper, args) ->
      (* *)
      Printf.bprintf buf "%s(" indent;
      pprint_opExpOp buf oper args.opExp_left args.opExp_right;
      Printf.bprintf buf ")"
  | LogicalOpExp (oper, args) ->
      Printf.bprintf buf "%s(" indent;
      pprint_exp buf "" args.opExp_left;
      pprint_logicalOpExpOp buf oper;
      pprint_exp buf "" args.opExp_right;
      Printf.bprintf buf ")"
  | TransposeExp transposeexp ->
      match transposeexp.transposeExp_conjugate with
        | Conjugate ->
            pprint_exp buf indent transposeexp.transposeExp_exp;
            Printf.bprintf buf "'"
        | NonConjugate ->
            pprint_exp buf indent transposeexp.transposeExp_exp;
            Printf.bprintf buf ".'"

and pprint_matrixLineExp buf indent mlexp =
  let length = Array.length mlexp.matrixLineExp_columns in
  Array.iteri (fun i e ->
    (* We need to deal with the comments in martix :
       a comment is not a value of the matrix *)
    pprint_matrix_exp buf indent length i e) mlexp.matrixLineExp_columns

and pprint_matrix_exp buf indent length i exp = match exp.exp_desc with
  | ConstExp (CommentExp commentexp) ->
      Printf.bprintf buf "// %s \n" commentexp.commentExp_comment
  | _ ->
      if i <> 0
      then
        begin
          Printf.bprintf buf ", ";
          pprint_exp buf "" exp
        end
      else
        pprint_exp buf "" exp


and pprint_opExpOp buf oper left right = match oper with
  | OpExp_plus ->
      pprint_exp buf "" left;
      Printf.bprintf buf " + ";
      pprint_exp buf "" right
  | OpExp_minus ->
      pprint_exp buf "" left;
      Printf.bprintf buf " - ";
      pprint_exp buf "" right
  | OpExp_times ->
      pprint_exp buf "" left;
      Printf.bprintf buf " * ";
      pprint_exp buf "" right
  | OpExp_rdivide ->
      pprint_exp buf "" left;
      Printf.bprintf buf " / ";
      pprint_exp buf "" right
  | OpExp_ldivide ->
      pprint_exp buf "" left;
      Printf.bprintf buf " \\ ";
      pprint_exp buf "" right
  | OpExp_power ->
      pprint_exp buf "" left;
      Printf.bprintf buf " ^ ";
      pprint_exp buf "" right
  | OpExp_unaryMinus ->
      Printf.bprintf buf " -";
      pprint_exp buf "" right
  | OpExp_dottimes ->
      pprint_exp buf "" left;
      Printf.bprintf buf " .* ";
      pprint_exp buf "" right
  | OpExp_dotrdivide ->
      pprint_exp buf "" left;
      Printf.bprintf buf " ./ ";
      pprint_exp buf "" right
  | OpExp_dotldivide ->
      pprint_exp buf "" left;
      Printf.bprintf buf " .\\ ";
      pprint_exp buf "" right
  | OpExp_dotpower ->
      pprint_exp buf "" left;
      Printf.bprintf buf " .^ ";
      pprint_exp buf "" right
  | OpExp_krontimes ->
      pprint_exp buf "" left;
      Printf.bprintf buf " .*. ";
      pprint_exp buf "" right
  | OpExp_kronrdivide ->
      pprint_exp buf "" left;
      Printf.bprintf buf " ./. ";
      pprint_exp buf "" right
  | OpExp_kronldivide ->
      pprint_exp buf "" left;
      Printf.bprintf buf " .\\. ";
      pprint_exp buf "" right
  | OpExp_controltimes ->
      pprint_exp buf "" left;
      Printf.bprintf buf " *. ";
      pprint_exp buf "" right
  | OpExp_controlrdivide ->
      pprint_exp buf "" left;
      Printf.bprintf buf " /. ";
      pprint_exp buf "" right
  | OpExp_controlldivide ->
      pprint_exp buf "" left;
      Printf.bprintf buf " \\. ";
      pprint_exp buf "" right
  | OpExp_eq ->
      pprint_exp buf "" left;
      Printf.bprintf buf " == ";
      pprint_exp buf "" right
  | OpExp_ne ->
      pprint_exp buf "" left;
      Printf.bprintf buf " <> ";
      pprint_exp buf "" right
  | OpExp_lt ->
      pprint_exp buf "" left;
      Printf.bprintf buf " < ";
      pprint_exp buf "" right
  | OpExp_le ->
      pprint_exp buf "" left;
      Printf.bprintf buf " <= ";
      pprint_exp buf "" right
  | OpExp_gt ->
      pprint_exp buf "" left;
      Printf.bprintf buf " > ";
      pprint_exp buf "" right
  | OpExp_ge ->
      pprint_exp buf "" left;
      Printf.bprintf buf " >= ";
      pprint_exp buf "" right


let pprint_ast ast =
  let b = Buffer.create 1024 in
  pprint_exp b "" ast;
  Buffer.contents b













