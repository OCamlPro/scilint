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
  | Ty_var of int
  | Mat of bool * ty * dim list
  | Dim of dim
  | Const_string of string
  | Const_bool of bool
  | Const_int of int
  | Fptr of arg list * ty list
  | String of dim option
  | Int | Bool | Num | Complex

and arg =
  | Named_arg of string * ty
  | Arg of ty
  | Rest

and dim =
  | Add of dim * dim
  | Mult of dim * dim
  | Sub of dim * dim
  | Div of dim * dim
  | Dim_var of int
  | Dim_const of int

(** Rename type and dimension variables starting from zeroes *)
let narrow ty =
  let var_store () =
    let vars = Hashtbl.create 10 and var = ref 0 in
    (fun i ->
       try Hashtbl.find vars i
       with Not_found ->
         let r = !var in
         Hashtbl.replace vars i r ;
         incr var ; r)
  in
  let dim_var = var_store () in
  let ty_var = var_store () in
  let rec rw_ty = function
    | Ty_var i -> Ty_var (ty_var  i)
    | Mat (s, ty, dims) -> Mat (s, rw_ty ty, List.map rw_dim dims) 
    | Dim dim -> Dim (rw_dim dim)
    | Fptr (args, tys) -> Fptr (List.map rw_arg args, List.map rw_ty tys)
    | String (Some dim) -> String (Some (rw_dim dim))
    | String None
    | Const_string _ | Const_bool _ | Const_int _
    | Int | Bool | Num | Complex as ty -> ty
  and rw_arg = function
    | Named_arg (n, ty) -> Named_arg (n, rw_ty ty)
    | Arg ty -> Arg (rw_ty ty)
    | Rest -> Rest
  and rw_dim = function
    | Add (ldim, rdim) -> Add (rw_dim ldim, rw_dim rdim)
    | Mult (ldim, rdim) -> Mult (rw_dim ldim, rw_dim rdim)
    | Sub (ldim, rdim) -> Sub (rw_dim ldim, rw_dim rdim)
    | Div (ldim, rdim) -> Div (rw_dim ldim, rw_dim rdim)
    | Dim_var i -> Dim_var (dim_var  i)
    | Dim_const _ as c -> c
  in
  rw_ty ty
