(*  OCamlPro Scilab Toolbox - AST pretty printing
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabAst
open PPrint

(** Parameters for the printer: AST type parameters and their
    projections to PPrint documents *)
module type PrinterParameters = sig
  module Parameters : Parameters
  val document_of_meta : Parameters.meta -> document -> document
  val document_of_symbol : Parameters.symbol -> document
end

(** Instanciate this functor with the same {!Parameters} as the AST to
    obtain a pretty printer ([output_ast file ast]). *)
module Make
    (Parameters : Parameters)
    (PrinterParameters : PrinterParameters with module Parameters := Parameters)
    (Ast : module type of ScilabAst.Make (Parameters)) = struct
  open Ast

  let instr_end =
    ifflat (string ", ") hardline
  let arg_sep in_matrix =
    if in_matrix then string ","
    else ifflat (string ", ") (string ", .." ^^ hardline)
  let col_sep =
    ifflat (string ", ") (string ", .." ^^ hardline)
  let shell_arg_sep =
    ifflat (string " ") (string " .." ^^ hardline)
  let bsp =
    ifflat (string " ") (string " .." ^^ hardline)
  let nbsp =
    string " "
  let mbsp in_matrix =
    if in_matrix then empty
    else ifflat (string " ") (string " .." ^^ hardline)
  let mnbsp in_matrix =
    if in_matrix then empty
    else string " "

  let format_string ?(delim ="\"") str =
    let len = String.length str in
    let rec spaces buf s i =
      if i = len then
        List.rev (cut s (if s = 0 then i else i - 1) :: buf)
      else match str.[i], i - s with
        | ' ', _ -> spaces buf s (succ i)
        | _, 0 -> chars buf i i
        | _ -> chars (cut s (i - 1) :: buf) i i
    and chars buf s i =
      if i = len then
        List.rev (cut s i :: buf)
      else match str.[i] with
        | ' ' -> spaces (cut s i :: buf) i i
        | c -> chars buf s (succ i)
    and cut s i =
      string (String.sub str s (max 0 (i - s)))
    in
    let subs = spaces [] 0 0 in
    align (string delim ^^ flow bsp subs ^^ string delim)

  (** Convert an AST to its PPrint intermediate representation *)
  let rec document_of_ast (ast : Ast.ast) =
    separate_map hardline document_of_stmt ast

  and document_of_exp ?in_matrix ?sp (w : Ast.exp) =
    group (document_of_descr w
             (document_of_exp_cstr ?in_matrix ?sp w.cstr))

  and group_sp sp doc =
    if sp then group (bsp ^^ doc) else doc

  and document_of_exp_cstr ?(in_matrix = false) ?(sp = false) (e : Ast.exp_cstr) =
    match e with
    | Call (name, args, Tuplified) ->
      group_sp sp
        (document_of_exp name ^^ mnbsp in_matrix ^^ string "("
         ^^ align (flow (arg_sep in_matrix)
                     (List.map (document_of_arg ~in_matrix) args))
         ^^ string ")")
    | Call (name, args, Cell) ->
      group_sp sp
        (document_of_exp name ^^ mnbsp in_matrix ^^ string "{"
         ^^ align (flow (arg_sep in_matrix)
                     (List.map (document_of_arg ~in_matrix) args))
         ^^ string "}")
    | Call (name, args, Shell) ->
      let shell_arg (_, exp) = document_of_exp exp in
      group_sp sp
        (document_of_exp name
         ^^ nest 2 (bsp ^^ flow shell_arg_sep (List.map shell_arg args)))
    | Call (lexp, [ None, { cstr = String name} ], Field) ->
      document_of_exp ~sp ~in_matrix lexp ^^ string "." ^^ string name
    | Call (lexp, _, Field) ->
      assert false
    | Identity args ->
      group_sp sp (string "(" 
                   ^^ (separate_map
                         (arg_sep in_matrix)
                         (document_of_exp ~in_matrix) args)
                   ^^ string ")")
    | Range (sexp, None, eexp) ->
      group (document_of_exp ~sp sexp
             ^^ mbsp in_matrix ^^ string ":" ^^ mnbsp in_matrix
             ^^ document_of_exp ~in_matrix eexp)
    | Range (sexp, Some stepexp, eexp) ->
      group (document_of_exp ~in_matrix ~sp sexp
             ^^ mbsp in_matrix ^^ string ":" ^^ mnbsp in_matrix
             ^^ document_of_exp ~in_matrix stepexp
             ^^ mbsp in_matrix ^^ string ":" ^^ mnbsp in_matrix
             ^^ document_of_exp ~in_matrix eexp)
    | Bool true ->
      group_sp sp (string "%T")
    | Bool false ->
      group_sp sp (string "%F")
    | Num f ->
      group_sp sp (string (Printf.sprintf "%g" f))
    | String str ->
      group_sp sp (format_string str)
    | Var sym ->
      group_sp sp (document_of_var sym)
    | Colon ->
      group_sp sp (string ":")
    | Matrix rows ->
      let st = (if sp then nbsp else empty) ^^ string "[" in
      let ed = string "]" in
      document_of_matrix_ctns st ed rows
    | Cell_array rows ->
      let st = (if sp then nbsp else empty) ^^ string "{" in
      let ed = string "}" in
      document_of_matrix_ctns st ed rows
    | Unop (Unary_plus, exp) ->
      group_sp sp (string "+" ^^ document_of_exp ~in_matrix exp)
    | Unop (Unary_minus, exp) ->
      group_sp sp (string "-" ^^ document_of_exp ~in_matrix exp)
    | Unop (Not, exp) ->
      group_sp sp (string "~" ^^ document_of_exp ~in_matrix exp)
    | Unop (Transpose_conjugate, exp) ->
      group_sp sp (document_of_exp ~in_matrix exp ^^ string "'")
    | Unop (Transpose_non_conjugate, exp) ->
      group_sp sp (document_of_exp ~in_matrix exp ^^ string ".'")
    | Op (op, lexp, rexp) ->
      if in_matrix then
        group_sp sp
          (align (document_of_exp ~in_matrix lexp
                  ^^ document_of_op op
                  ^^ document_of_exp ~in_matrix rexp))
      else
        group_sp sp
          (align (document_of_exp lexp
                  ^^ shell_arg_sep ^^ document_of_op op
                  ^^ nbsp ^^ document_of_exp rexp))
    | Error ->
      group_sp sp (string "error")
        
  and document_of_op op =
    match op with
    | Plus -> string "+"
    | Minus -> string "-"
    | Times -> string "*"
    | Rdivide -> string "/"
    | Ldivide -> string "\\"
    | Power -> string "**"
    | Dot_times-> string ".*"
    | Dot_rdivide -> string "./"
    | Dot_ldivide -> string ".\\"
    | Dot_power -> string ".^"
    | Kron_times -> string ".*."
    | Kron_rdivide -> string "./."
    | Kron_ldivide -> string ".\\."
    | Control_times -> string "*."
    | Control_rdivide -> string "/."
    | Control_ldivide -> string "\\."
    | Eq -> string "=="
    | Ne -> string "<>"
    | Lt -> string "<"
    | Le -> string "<="
    | Gt -> string "<"
    | Ge -> string ">="
    | And -> string "&"
    | Or -> string "|"
    | Seq_and -> string "&&"
    | Seq_or -> string "||"

  and document_of_matrix_ctns st ed rows =
    let render = document_of_exp ~in_matrix:true in
    let docs = List.map (fun { cstr } -> List.map render cstr) rows in
    let buf = Buffer.create 200 in
    let str doc =
      Buffer.reset buf ;
      ToBuffer.compact buf doc ;
      Buffer.contents buf
    in
    let strs = List.map (List.map str) docs in
    let rec widths lines =
      let heads, tails, continue = cut lines in
      if not continue then [] else
        List.fold_left max 0 heads :: widths tails
    and cut lines =
      match lines with
      | (h :: t) :: lines ->
        let heads, tails, _ = cut lines in
        String.length h :: heads, t :: tails, true
      | ([]) :: lines -> cut lines
      | [] -> [], [], false
    in
    let pad s w =
      let r = String.make (w + 1) ' ' in
      String.blit s 0 r 0 (String.length s) ;
      r.[String.length s] <- ',' ; r
    in
    let format_row w s =
      let rec loop acc w s =
        match w, s with
        | [], _ -> assert false
        | _, [] -> acc
        | w :: _ , [ s ] -> group (acc ^^ string s)
        | w :: ws, s :: ss ->
          loop
            (group (acc ^^ ifflat (string (pad s w)) (string s) ^^ bsp))
            ws ss
      in loop empty w s
    in
    let row_sep =
      ifflat (string " ; ") (string " ;" ^^ hardline)
    and mbsp =
      ifflat (string " ") (hardline)
    in
    let widths = widths strs in
    let items = List.map (format_row widths) strs in
    group
      (ifflat
         (st ^^
          separate_map (string " ; ")
            (separate_map (string ", ") string)
            strs
         ^^ ed)
         (nest 2 (st ^^ mbsp ^^ separate row_sep items) ^^ mbsp ^^ ed))

  and document_of_arg ?(in_matrix = false) (name, exp) =
    match name with
    | None -> document_of_exp ~in_matrix exp
    | Some n ->
      document_of_var n
      ^^ mnbsp in_matrix ^^ string "=" ^^
      group (nest 2 (mbsp in_matrix
                     ^^ document_of_exp ~in_matrix exp))

  and document_of_stmt (w : Ast.stmt) =
    document_of_descr w (document_of_stmt_cstr w.cstr)

  and document_of_stmt_cstr (st : Ast.stmt_cstr) =
    match st with
    | Assign ([left], right) ->
      document_of_exp left ^^ nbsp ^^ string "="
      ^^ nest 2 (document_of_exp ~sp:true right)
    | Assign (lefts, right) ->
      group (string "["
             ^^ nest 2 (separate_map (arg_sep false)
                          (document_of_exp ~in_matrix:true) lefts)
             ^^ string "]")
      ^^ nbsp ^^ string "="
      ^^ nest 2 (document_of_exp ~sp:true right)
    | Seq stmts ->
      separate_map instr_end document_of_stmt stmts
    | Defun { name ; args ; rets = [] ; body } ->
      string "function" ^^ nbsp ^^ document_of_var name ^^ nbsp
      ^^ group (string "("
                ^^ align (flow (arg_sep false)
                            (List.map document_of_var args)
                          ^^ string ")"))
      ^^ nest 2 (instr_end ^^ document_of_stmt body)
      ^^ instr_end ^^ string "endfunction"
    | Defun { name ; args ; rets = [ ret ] ; body } ->
      string "function" ^^ nbsp
      ^^ document_of_var ret ^^ nbsp ^^ string "="
      ^^ bsp ^^ document_of_var name ^^ nbsp
      ^^ group (string "("
                ^^ align (flow (arg_sep false)
                            (List.map document_of_var args)
                          ^^ string ")"))
      ^^ nest 2 (instr_end ^^ document_of_stmt body)
      ^^ instr_end ^^ string "endfunction"
    | Defun { name ; args ; rets ; body } ->
      string "function" ^^ nbsp
      ^^ group (string "["
                ^^ separate_map (arg_sep false) document_of_var rets
                ^^ string "] =")
      ^^ group (bsp ^^ document_of_var name) ^^ nbsp
      ^^ group (string "("
                ^^ align (flow (arg_sep false)
                            (List.map document_of_var args)
                          ^^ string ")"))
      ^^ nest 2 (instr_end ^^ document_of_stmt body)
      ^^ instr_end ^^ string "endfunction"
    | Exp exp ->
      document_of_exp exp
    | Break ->
      string "break"
    | Continue ->
      string "continue"
    | Comment text ->
      string text
    | For (it, range, body) ->
      string "for" ^^ nbsp ^^ document_of_var it
      ^^ nbsp ^^ string "=" ^^ nbsp
      ^^ document_of_exp range ^^ nbsp ^^ string "do"
      ^^ nest 2 (instr_end ^^ document_of_stmt body) ^^ instr_end
      ^^ string "end"
    | If (cond, tbody, Some fbody)  ->
      string "if" ^^ nbsp ^^ document_of_exp cond
      ^^ nbsp ^^ string "then"
      ^^ nest 2 (instr_end ^^ document_of_stmt tbody) ^^ instr_end
      ^^ string "else"
      ^^ nest 2 (instr_end ^^ document_of_stmt fbody) ^^ instr_end
      ^^ string "end"
    | If (cond, tbody, None)  ->
      string "if" ^^ nbsp ^^ document_of_exp cond
      ^^ nbsp ^^ string "then"
      ^^ nest 2 (instr_end ^^ document_of_stmt tbody) ^^ instr_end
      ^^ string "end"
    | Return  ->
      string "return"
    | Select { cond ; cases ; default = None }  ->
      string "select" ^^ nbsp ^^ document_of_exp cond
      ^^ nest 2 (instr_end ^^ separate_map instr_end document_of_case cases)
      ^^ instr_end
      ^^ string "end"
    | Select { cond ; cases ; default = Some d }  ->
      string "select" ^^ nbsp ^^ document_of_exp cond
      ^^ nest 2 (instr_end ^^ separate_map instr_end document_of_case cases)
      ^^ instr_end
      ^^ string "else"
      ^^ nest 2 (instr_end ^^ document_of_stmt d) ^^ instr_end
      ^^ string "end"
    | Try (tbody, cbody)  ->
      string "try"
      ^^ nest 2 (instr_end ^^ document_of_stmt tbody) ^^ instr_end
      ^^ string "catch" ^^ instr_end
      ^^ nest 2 (instr_end ^^ document_of_stmt cbody) ^^ instr_end
      ^^ string "end"
    | While (cond, tbody, Some fbody)  ->
      string "while" ^^ nbsp ^^ document_of_exp cond
      ^^ nbsp ^^ string "then"
      ^^ nest 2 (instr_end ^^ document_of_stmt tbody) ^^ instr_end
      ^^ string "else"
      ^^ nest 2 (instr_end ^^ document_of_stmt fbody) ^^ instr_end
      ^^ string "end"
    | While (cond, tbody, None)  ->
      string "while" ^^ nbsp ^^ document_of_exp cond
      ^^ nbsp ^^ string "then"
      ^^ nest 2 (instr_end ^^ document_of_stmt tbody) ^^ instr_end
      ^^ string "end"

  and document_of_case (exp, stmt) =
      string "case" ^^ nbsp ^^ document_of_exp exp
      ^^ nest 2 (instr_end ^^ document_of_stmt stmt)

  and document_of_var (s : Ast.var) =
    PrinterParameters.document_of_symbol s.cstr

  and document_of_descr : 'a. 'a descr -> document -> document
    = fun { comment ; meta } document ->
    List.fold_left
      (fun r { cstr } -> group (document ^^ break 1 ^^ string cstr) ^^ hardline)
      document comment
    |> PrinterParameters.document_of_meta meta

  (** Output an S-expr to a channel using PPrint for great beauty. *)
  let pretty_output ?(width = 80) (fp : out_channel) ast =
    ToChannel.pretty 0.9 width fp (document_of_ast ast)

  (** Output an S-expr to a channel using PPrint for great beauty. *)
  let compact_output (fp : out_channel) ast =
    ToChannel.compact fp (document_of_ast ast)
end
