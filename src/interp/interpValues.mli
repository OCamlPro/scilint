(*  OCamlPro Scilab Toolbox - OcSciLab, main interpreter state
 *  Copyright (C) 2014 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

module type S = sig

  (** Bad matrix / list access *)
  exception Bad_index

  (** Type error *)
  exception Bad_type

  (** Type cast error *)
  exception Bad_cast

  (** Abstract internal representation of values *)
  type value

  (** Abstract internal representation of polynomials *)
  type 'a poly_repr

  (** Abstract internal representation of matrices *)
  type 'a matrix_repr

  (** Abstract internal representation of sparce matrices *)
  type 'a sparse_repr

  (** Types for immediates tagged by their OCaml equivalents *)
  type _ itag =
    | Number : 'a poly ntag -> 'a itag
    | Poly : 'a poly ntag -> 'a poly itag
    | String : string itag
    | Bool : bool itag
    | Int8 : int itag
    | Int16 : int itag
    | Int32 : int itag
    | Uint8 : int itag
    | Uint16 : int itag
    | Uint32 : int itag

  (** Type tag for precising if numbers and polynomials are in R or C *)
  and _ ntag =
    | Real : float poly ntag
    | Complex : (float * float) poly ntag

  (** Representation of polynomials *)
  and 'a poly = P of 'a poly_repr (* Guarded for injectivity *)

  val poly_create : 'a poly ntag -> string -> 'a poly
  val poly_get : 'a poly -> int -> 'a
  val poly_set : 'a poly -> int -> 'a -> unit
  val poly_length : 'a poly -> int
  val poly_variable : 'a poly -> string

  (** Representation of matrices *)
  type 'a matrix = M of 'a matrix_repr (* Guarded for injectivity *)

  val matrix_create : 'a itag -> int -> int -> 'a matrix
  val matrix_cast : 'a itag -> 'b itag -> 'a matrix -> 'b matrix
  val matrix_get : 'a matrix -> int -> int -> 'a
  val matrix_get_linear : 'a matrix -> int -> 'a
  val matrix_set : 'a matrix -> int -> int -> 'a -> unit
  val matrix_set_linear : 'a matrix -> int -> 'a -> unit
  val matrix_size : 'a matrix -> int * int
  val matrix_collate_horizontally : 'a matrix -> 'a matrix -> 'a matrix
  val matrix_collate_vertically : 'a matrix -> 'a matrix -> 'a matrix

  (** Representation of sparse matrices *)
  type 'a sparse = S of 'a sparse_repr (* guarded for injectivity *)

  val sparse_create : 'a itag -> int -> int -> 'a sparse
  val sparse_cast : 'a itag -> 'b itag -> 'a sparse -> 'b sparse
  val sparse_get : 'a sparse -> int -> int -> 'a
  val sparse_set : 'a sparse -> int -> int -> 'a -> unit
  val sparse_size : 'a sparse -> int * int

  (** Representation of lists *)
  type vlist

  val vlist_create : value list -> vlist
  val vlist_length : vlist -> int
  val vlist_get : vlist -> int -> value
  val vlist_set : vlist -> int -> value -> unit

  (** Representation of 'matrix oriented' lists *)
  type mlist

  val mlist_create : string -> string list -> value list -> mlist
  val mlist_length : mlist -> int
  val mlist_label : mlist -> string
  val mlist_fields : mlist -> string list
  val mlist_get : mlist -> string -> value
  val mlist_set : mlist -> string -> value -> unit
  val mlist_get_by_index : mlist -> int -> value
  val mlist_set_by_index : mlist -> int -> value -> unit

  (** Representation of 'typed' lists *)
  type tlist

  val tlist_create : string -> string list -> value list -> tlist
  val tlist_length : tlist -> int
  val tlist_label : tlist -> string
  val tlist_fields : tlist -> string list
  val tlist_get : tlist -> string -> value
  val tlist_set : tlist -> string -> value -> unit
  val tlist_get_by_index : tlist -> int -> value
  val tlist_set_by_index : tlist -> int -> value -> unit

  (** Representation of user-defined functions *)
  type macro

  (** Representation of 'graphic handles' *)
  type handle

  (** Value types tagged by their OCaml equivalents *)
  type _ tag =
    | Single : 't itag -> 't tag
    | Eye : 't itag -> 't tag
    | Matrix : 't itag -> 't matrix tag
    | Sparse : 't itag -> 't sparse tag
    | Vlist : vlist tag
    | Tlist : string -> tlist tag
    | Mlist : string -> mlist tag
    | Macro : macro tag
    | Primitive : string tag
    | Handle : handle tag
    | Atom : unit tag
    | Null : unit tag

  (** Detagged value types *)
  type rtt = T : 'a tag -> rtt

  (** Obtains the run-time type of a value *)
  val typeof : value -> rtt

  (** Checks the type of a value *)
  val instanceof : value -> rtt -> bool

  (** Tries to cast a value to a given type *)
  val cast : value -> rtt -> value

  (** Builds a value from an unboxed OCaml value and its tag *)
  val inject : 'a tag -> 'a -> value

  (** Extracts an unboxed OCaml value for a given tag. Raises
      [Bad_type] if impossible. *)
  val extract : 'a tag -> value -> 'a

  (** Generic OCaml view of values *)
  type view = V : 'a tag * 'a -> view
          
  (** Obtains an OCaml view of a value *)
  val view : value -> view

  (** Builds a value from a boxed OCaml view *)
  val repr : view -> value

  (** Conversion between immediate types *)
  val icast : 'a itag -> 'b itag -> 'a -> 'b
    
  (** Performs a deep copy *)
  val copy : value -> value

  (** Primitive for the copy / refcounted semantics to claim ownership *)
  val grab : value -> value

  (** Primitive for the copy / refcounted semantics to abandon ownership  *)
  val release : value -> unit

end
