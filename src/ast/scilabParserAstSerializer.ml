(*  OCamlPro Scilab Toolbox - AST serialisation for interop with C++
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabParserAst

(** Builds a linear, binary representation of the ast. The format is a
    prefix traversal of the tree in which every node is output as
    the sequence [ LOC ID ... ]

    - ID is an UINT8 corresponding to the C++ class of the node
    - LOC is a sequence of 4 UINT32 (top, left, bottom, right)
    - ... depends on the node type

    See the code for more information. *)
let serialize_ast =
  (* we reuse the same buffer for minimizing growths *)
  let b = Buffer.create 10_000 in
  (* binary buffer output primitives *)
  let output_uint8 n =
    if n < 0 || n > 255 then
      invalid_arg ("ScilabAstSerializer.output_uint8 " ^ string_of_int n) ;
    Buffer.add_char b (char_of_int n)
  in
  let output_bool bool =
    output_uint8 (if bool then 1 else 0)
  and output_uint32 n =
    output_uint8 (n land 0xff) ;
    output_uint8 ((n lsr 8) land 0xff) ;
    output_uint8 ((n lsr 16) land 0xff) ;
    output_uint8 ((n lsr 24) land 0xff)
  and output_double f =
    let bits = Int64.bits_of_float f in
    for i = 0 to 7 do
      let ofs = i * 8 in
      output_uint8 (Int64.(to_int (shift_right bits ofs)) land 0xFF)
    done
  in
  let output_wstring s =
    output_uint32 (String.length s) ;
    Buffer.add_string b s
  in
  (* ast types output functions *)
  let output_symbol s =
    output_wstring s   
  and output_location (_, ((fl, fc), (ll, lc))) =
    output_uint32 fl; output_uint32 fc;
    output_uint32 ll; output_uint32 lc
  in
  let rec output_list output items =
    output_uint32 (List.length items) ;
    List.iter output items
  in
  let output_header loc code =
    output_uint8 code ;
    output_location loc
  in
  let seq ({ cstr ; loc } as stmt) =
    match cstr with
    | Seq l -> l, loc
    | _ -> [ stmt ], loc
  in
  let rec output_stmt { cstr ; loc ; meta ; comment } =
    match cstr with
      | Exp exp ->
        output_exp exp
      | Assign ([left], right) ->
        output_header loc 31 (* AssignExp *) ;
        output_exp left ;
        output_exp right
      | Assign (lefts, right) ->
        output_header loc 25 (* AssignListExp *) ;
        output_list output_exp lefts ;
        output_exp right
      | Seq stmts ->
        output_header loc 1 (* SeqExp *) ;
        output_list output_stmt stmts
      | Defun { name ; args ; rets ; body } ->
        output_header loc 29 (* FunctionDec  *) ;
        output_symbol name.cstr ;
        output_location loc (* FIXME: forge better locations (args) *) ;
        output_location loc (* FIXME: forge better locations (rets) *) ;
        output_stmt body ;
        output_list output_var args ;
        output_list output_var rets
      | Comment text ->
        output_header loc 3 (* CommentExp *) ;
        output_wstring text
      | For ({ cstr = it ; loc = it_loc }, range, body) ->
        output_header loc 17 (* ForExp *) ;
        output_location it_loc ;
        output_symbol it ;
        output_exp range ;
        output_stmt body
      | If (cond, tbody, Some fbody)  ->
        output_header loc 14 (* IfExp *) ;
        output_bool true (* has else *) ;
        output_exp cond ;
        output_stmt tbody ;
        output_stmt fbody
      | If (cond, tbody, None)  ->
        output_header loc 14 (* IfExp *) ;
        output_bool false (* has else *) ;
        output_exp cond ;
        output_stmt tbody
      | Select { cond ; cases ; default }  ->
        output_header loc 21 (* SelectExp *) ;
        (match default with
         | None -> output_bool false
         | Some d ->
           output_bool true ;
           let dbody, dloc = seq d in
           output_location dloc ;
           output_stmts  dbody) ;
        output_exp cond ;
        output_list
          (fun (v, branch) ->
             let bbody, bloc = seq branch in
             output_location v.loc (* FIXME: forge whole location *) ;
             output_location bloc ;
             output_exp v ;
             output_stmts bbody)
          cases
      | Try (tbody, cbody)  ->
        output_header loc 15 (* TryCatchExp *) ;
        (* the C++ code has SeqExp fields so we forge seqs *)
        let (tbody, tloc), (cbody, cloc) =  seq tbody, seq cbody in
        output_location tloc ;
        output_location cloc ;
        output_stmts tbody ;
        output_stmts cbody
      | While (cond, body, None)  ->
        output_header loc 16 (* WhileExp *) ;
        output_exp cond ;
        output_stmt body
      | While (cond, body, Some _)  ->
        (* FIXME: macro generate ?? *)
        failwith "Unsupported while with else"
      | Return ->
        output_header loc 20 (* ReturnExp *) ;
        output_bool true (* no parameters *)
      | Break ->
        output_header loc 18 (* BreakExp *)
      | Continue ->
        output_header loc 19 (* ContinueExp *)
  and output_var { cstr ; loc } =
    match cstr with
    | "$" ->
      output_header loc 11 (* DollarVar *) ;
    | sym ->
      output_header loc 9 (* SimpleVar *) ;
      output_wstring sym
  and output_exp { cstr ; loc ; meta ; comment } =
    output_location loc ;
    let op_codes op =
      match op with
      (* OpExp *)
      | Plus -> 32, 1
      | Minus -> 32, 2
      | Times -> 32, 3
      | Rdivide -> 32, 4
      | Ldivide -> 32, 5
      | Power -> 32, 6
      | Dot_times -> 32, 7
      | Dot_rdivide -> 32, 8
      | Dot_ldivide -> 32, 9
      | Dot_power -> 32, 10
      | Kron_times -> 32, 11
      | Kron_rdivide -> 32, 12
      | Kron_ldivide -> 32, 13
      | Control_times -> 32, 14
      | Control_rdivide -> 32, 15
      | Control_ldivide -> 32, 16
      | Eq -> 32, 17
      | Ne -> 32, 18
      | Lt -> 32, 19
      | Le -> 32, 20
      | Gt -> 32, 21
      | Ge -> 32, 22
      (* LogicalOpExp *)
      | And -> 33, 24
      | Or -> 33, 25
      | Seq_and -> 33, 26
      | Seq_or -> 33, 27
    in
    match cstr with
      | Call (name, args, (Tuplified | Shell)) ->
        output_header loc 35 (* CalExp *) ;
        output_exp name ;
        output_list output_arg args
      | Call (name, args, Cell) ->
        output_header loc 37 (* CellCalExp *) ;
        output_exp name ;
        output_list output_arg args
      | Call (lexp, [ None, rexp], Field) ->
        output_header loc 13 (* FieldExp *) ;
        output_exp lexp ;
        output_exp rexp
      | Call (lexp, _, Field) ->
        assert false
      | Identity args ->
        output_header loc 12 ; (* ArrayListVar *)
        output_exps args
      | Range (sexp, None, eexp) ->
        output_header loc 30 (* ListExp *) ;
        output_exp sexp ;
        (* we forge the 1 in start:1:end *)
        output_header loc 6 (* DoubleExp *) ;
        output_double 1.0 ;
        output_exp eexp
      | Range (sexp, Some stepexp, eexp) ->
        output_header loc 30 (* ListExp *) ;
        output_exp sexp ;
        output_exp stepexp ;
        output_exp eexp
      | Var name ->
        output_var name
      | Matrix rows ->
        output_header loc 34 (* MatrixExp *) ;
        output_matrix_contents rows
      | Cell_array rows ->
        output_header loc 23 (* CellExp *) ;
        output_matrix_contents rows
      | Unop (Not, exp) ->
        output_header loc 26 (* NotExp *) ;
        output_exp exp
      | Unop (Unary_minus, exp) ->
        (* let's forge (0.0 - exp) *)
        output_header loc 32 (* OpExp *) ;
        output_header loc 23 (* unaryMinus *) ;
        output_header loc 6 (* DoubleExp *) ;
        output_double 0.0 ;
        output_exp exp
      | Unop (Unary_plus, exp) ->
        output_exp exp
      | Op (op, lexp, rexp) ->
        let code, op_code = op_codes op in
        output_header loc code ;
        output_header loc op_code ;
        output_exp lexp ;
        output_exp rexp
      | Unop (Transpose_conjugate, exp) ->
        output_header loc 27 (* TransposeExp *) ;
        output_uint8 1 ;
        output_exp exp ;
      | Unop (Transpose_non_conjugate, exp) ->
        output_header loc 27 (* TransposeExp *) ;
        output_uint8 2 ;
        output_exp exp ;
      | Bool v ->
        output_header loc 7 (* BoolExp *) ;
        output_bool v
      | Num f ->
        output_header loc 6 (* DoubleExp *) ;
        output_double f
      | String str ->
        output_header loc 2 (* StringExp *) ;
        output_wstring str
      | Colon ->
        output_header loc 10 (* ColonVar *)
      | Error ->
        failwith "Trailing error in the AST"
  and output_arg (n, exp) =
    match n with 
    | Some var ->
      (* let's forge an assignment *)
      output_header exp.loc 31 (* AssignExp *) ;
      output_exp { var with cstr = Var var } ;
      output_exp exp
    | None -> output_exp exp
  and output_exps exps = output_list output_exp exps
  and output_stmts stmts = output_list output_stmt stmts
  and output_matrix_contents ctns =
    output_uint32 (List.length ctns) ;
    List.iter
      (fun { cstr = line ; loc } ->
         output_location loc ;
         output_exps line)
      ctns
  in  
let set_uint32 s pos n =
  s.[pos] <- char_of_int (n land 0xff) ;
  s.[pos+1] <- char_of_int ((n lsr 8) land 0xff) ;
  s.[pos+2] <- char_of_int ((n lsr 16) land 0xff) ;
  s.[pos+3] <- char_of_int ((n lsr 24) land 0xff)
in
  (* main output function *)
  fun ast ->
    Buffer.clear b ;
    Buffer.add_string b "SIZE";
    output_stmts ast;
    let str = Buffer.contents b in
    let size = String.length str in
    set_uint32 str 0 size ;
    (* let's not waste too much memory  *)
    if size >= 1_000_000 then Buffer.reset b ;
    str
