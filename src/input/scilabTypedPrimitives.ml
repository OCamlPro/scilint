(*  OCamlPro Scilab Toolbox - Typed primitives
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

type typed_primitive =
  string * ty list

and ty =
  | TyVar of int
  | Mat of bool * ty * dim list
  | Dim of dim
  | Const_string of string
  | Const_bool of bool
  | Fptr of arg list * ty list
  | String of dim option
  | Int | Bool | Num | Complex

and arg =
  | NamedArg of string * ty
  | Arg of ty
  | Rest

and dim =
  | Const_int of int
  | Add of dim * dim
  | Mult of dim * dim
  | Sub of dim * dim
  | Div of dim * dim
  | DimVar of int
