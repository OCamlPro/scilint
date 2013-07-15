(*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2012-2013 - OCAMLPRO INRIA - Fabrice LE FESSANT
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 *)

type t

type box = {
  mutable box_value : t;
  mutable box_refcount : int;
 }


type symbol = {
  symbol_name : string;
  mutable symbol_binding : binding option;
}

and binding = {
             binding_name : string;
     mutable binding_locals : local_binding list;
     mutable binding_global : global_binding option;
}

and local_binding = {
    mutable local_box : box;
    local_scope : scope;
}

and global_binding = {
    mutable global_box : box;
    mutable global_scopes : scope list;
}

and scope = {
  scope_level : int;
  mutable scope_locals : binding list;
  mutable scope_globals : binding list;
}

and context = {
  mutable context_create_empty_value : (unit -> t);
  mutable context_string_of_value : (t -> string);
  mutable context_increase_refcount : (t -> t);
  mutable context_decrease_refcount : (t -> unit);

  mutable context_scopes : scope list; (* never [] *)
}

let new_symbol name = {
  symbol_name = name;
  symbol_binding = None;
}


let symbol_name sy = sy.symbol_name
