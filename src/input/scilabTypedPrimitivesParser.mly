(*  OCamlPro Scilab Toolbox - Typed primitives importer from SILKAN's format
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

%token EOF
%token COLON SEMI COMA ARROW OPAR CPAR SPARSE OBRA CBRA DOTDOTDOT
%token INT REAL BOOL VOID STRING COMPLEX
%token PLUS MINUS STAR DIV
%token <int> DIMVAR
%token <string> R NR OP
%token <string> CONST_STRING
%token <int> CONST_INT
%token <string * string> MARKUP

%left PLUS MINUS
%left STAR DIV

%start<ScilabTypedPrimitives.typed_primitive list> main

%{

open ScilabTypedPrimitives

type ir =
  | Markup of string * string
  | Def of bool * string * ty
  | Alias of bool * string * string list

let fresh =
  let cpt = ref 1000 in
  fun () -> incr cpt ; !cpt

let process ir =
  let module SM = Map.Make (String) in 
  let map = ref SM.empty in
  let find name =
    try snd (SM.find name !map) with Not_found -> []
  in
  List.iter
    (function
      | Markup _ -> ()
      | Def (e, name, ty) ->
        map := SM.add name (e, narrow ty :: find name) !map
      | Alias (e, name, names) ->
        let tys = List.flatten (List.map find names) in
        map := SM.add name (e, tys) !map)
    ir ;
  SM.fold
    (fun name (e, tys) r -> if e then (name, tys) :: r else r)
    !map []

%}

%%

main:
  | list (SEMI) res = mainrec { process res }
;

mainrec:
  | s = stmt SEMI list (SEMI) res = mainrec { s :: res }
  | s = stmt EOF { [ s ] }
  | EOF { [] }
;

stmt:
  | v = MARKUP
      { let name, text = v in
        Markup (name, text) }
  | op = OP COLON def = def
      { match def with
        | `Alias names -> Alias (true, op, names)
        | `Def ty -> Def (true, op, ty) }
  | n = R COLON def = def
  | n = NR COLON def = def
      { match def with
        | `Alias names -> Alias (false, n, names)
        | `Def ty -> Def (false, n, ty) }
;

def:
  | names = nonempty_list (terminated (name, option(COMA)))
      { `Alias names }
  | tys = delimited (OPAR, separated_list (COMA, arg), CPAR) ARROW rets = ret
      { `Def (Fptr (tys, rets)) }
  | OPAR VOID CPAR ARROW rets = ret
      { `Def (Fptr ([], rets)) }
;

arg:
  | DOTDOTDOT
    { Rest }
  | ty = ty
    { Arg ty }
;

ret:
  | VOID
    { [] }
  | tys = delimited (OPAR, separated_list (COMA, ty), CPAR)
      { tys }
  | ty = ty
      { [ ty ] }
;

ty:
  | INT { Int }
  | BOOL { Bool }
  | REAL { Num }
  | STRING dim = option (delimited (OPAR, dim, CPAR)) { String dim }
  | COMPLEX { Complex }
  | s = CONST_STRING { Const_string s }
  | STAR { Ty_var (fresh ()) }
  | SPARSE mat = mat
      { let ty, dims = mat in
        Mat (true, ty, dims) }
  | mat = mat
      { let ty, dims = mat in
        Mat (false, ty, dims) }
  | i = CONST_INT { Const_int i }
  | i = DIMVAR PLUS e2 = dim { Dim (Add (Dim_var i, e2)) }
  | i = DIMVAR MINUS e2 = dim { Dim (Sub (Dim_var i, e2)) }
  | i = DIMVAR STAR e2 = dim { Dim (Mult (Dim_var i, e2)) }
  | i = DIMVAR DIV e2 = dim { Dim (Div (Dim_var i, e2)) }
  | i = DIMVAR { Dim (Dim_var i) }
;

%inline mat:
  OBRA dims = separated_nonempty_list (COMA, dim) CBRA ty = ty
    { (ty, dims) }
;

dim:
  | i = DIMVAR { Dim_var i }
  | STAR { Dim_var (fresh ()) }
  | i = CONST_INT { Dim_const i }
  | OPAR e = dim CPAR { e }
  | e1 = dim PLUS e2 = dim { Add (e1, e2) }
  | e1 = dim MINUS e2 = dim { Sub (e1, e2) }
  | e1 = dim STAR e2 = dim { Mult (e1, e2) }
  | e1 = dim DIV e2 = dim { Div (e1, e2) }
;

name:
  | n = R | n = NR | n = OP { n }
;

%%
