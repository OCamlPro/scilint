(*  OCamlPro Scilab Toolbox - OcSciLab, pure OCaml implementation
 *  Copyright (C) 2014 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

(** Quick & Dirty implementation of values in pure OCaml using OCaml
    ints, floats and arrays. *)

module type Parameters = sig
  type macro
end

module Make (Parameters : Parameters) = struct

  exception Bad_index
  exception Bad_type
  exception Bad_cast

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
  and _ ntag =
    | Real : float poly ntag
    | Complex : (float * float) poly ntag
  and 'a poly = P of 'a poly_repr
  and 'a matrix = M of 'a matrix_repr
  and 'a sparse = S of 'a sparse_repr
  and 'a poly_repr = 'a * string * 'a array ref
  and 'a matrix_repr = 'a * (int * int * 'a array) ref
  and 'a sparse_repr = 'a * (int * int * 'a array) ref (* FIXME: not so sparse *)
  and vlist = value array ref
  and mlist = string * string list * value array ref
  and tlist = string * string list * value array ref
  and macro = Parameters.macro
  and handle
  and value = view
  and view =
    | V : 'a tag * 'a -> view
  and _ tag =
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
  type rtt = T : 'a tag -> rtt

  let rec icast : type a b. a itag -> b itag -> a -> b = fun k1 k2 v ->
    let s m v =
      let r = v land m in 
      if r land ((m + 1) lsr 1) = 0 then r else r lor (lnot m)
    in
    let u m v =
      v land m
    in
    let s8 = s 0xFF and s16 = s 0xFFFF and s32 = s 0xFFFFFFFF in
    let u8 = u 0xFF and u16 = u 0xFFFF and u32 = u 0xFFFFFFFF in
    match k1, k2 with
    (* cast to Uint8 *)
    | Number Real, Uint8 -> u8 (int_of_float v)
    | Number Complex, Uint8 -> raise Bad_cast
    | Poly Real, Uint8 -> raise Bad_cast
    | Poly Complex, Uint8 -> raise Bad_cast
    | Bool, Uint8 -> if v then 1 else 0
    | String, Uint8 -> raise Bad_cast
    | Int8, Uint8 -> u8 v
    | Int16, Uint8 -> u8 v
    | Int32, Uint8 -> u8 v
    | Uint8, Uint8 -> v
    | Uint16, Uint8 -> u8 v
    | Uint32, Uint8 -> u8 v
    (* cast to Int8 *)
    | Number Real, Int8 -> s8 (int_of_float v)
    | Number Complex, Int8 -> raise Bad_cast
    | Poly Real, Int8 -> raise Bad_cast
    | Poly Complex, Int8 -> raise Bad_cast
    | Bool, Int8 -> if v then 1 else 0
    | String, Int8 -> raise Bad_cast
    | Int8, Int8 -> v
    | Int16, Int8 -> s8 v
    | Int32, Int8 -> s8 v
    | Uint8, Int8 -> s8 v
    | Uint16, Int8 -> s8 v
    | Uint32, Int8 -> s8 v
    (* cast to Uint16 *)
    | Number Real, Uint16 -> u16 (int_of_float v)
    | Number Complex, Uint16 -> raise Bad_cast
    | Poly Real, Uint16 -> raise Bad_cast
    | Poly Complex, Uint16 -> raise Bad_cast
    | Bool, Uint16 -> if v then 1 else 0
    | String, Uint16 -> raise Bad_cast
    | Int8, Uint16 -> u16 v
    | Int16, Uint16 -> u16 v
    | Int32, Uint16 -> u16 v
    | Uint8, Uint16 -> v
    | Uint16, Uint16 -> v
    | Uint32, Uint16 -> u16 v
    (* cast to Int16 *)
    | Number Real, Int16 -> s16 (int_of_float v)
    | Number Complex, Int16 -> raise Bad_cast
    | Poly Real, Int16 -> raise Bad_cast
    | Poly Complex, Int16 -> raise Bad_cast
    | Bool, Int16 -> if v then 1 else 0
    | String, Int16 -> raise Bad_cast
    | Int8, Int16 -> v
    | Int16, Int16 -> v
    | Int32, Int16 -> s16 v
    | Uint8, Int16 -> s16 v
    | Uint16, Int16 -> s16 v
    | Uint32, Int16 -> s16 v
    (* cast to Uint32 *)
    | Number Real, Uint32 -> u32 (int_of_float v)
    | Number Complex, Uint32 -> raise Bad_cast
    | Poly Real, Uint32 -> raise Bad_cast
    | Poly Complex, Uint32 -> raise Bad_cast
    | Bool, Uint32 -> if v then 1 else 0
    | String, Uint32 -> raise Bad_cast
    | Int8, Uint32 -> u32 v
    | Int16, Uint32 -> u32 v
    | Int32, Uint32 -> u32 v
    | Uint8, Uint32 -> v
    | Uint16, Uint32 -> v
    | Uint32, Uint32 -> v
    (* cast to Int32 *)
    | Number Real, Int32 -> s32 (int_of_float v)
    | Number Complex, Int32 -> raise Bad_cast
    | Poly Real, Int32 -> raise Bad_cast
    | Poly Complex, Int32 -> raise Bad_cast
    | Bool, Int32 -> if v then 1 else 0
    | String, Int32 -> raise Bad_cast
    | Int8, Int32 -> v
    | Int16, Int32 -> v
    | Int32, Int32 -> v
    | Uint8, Int32 -> s32 v
    | Uint16, Int32 -> s32 v
    | Uint32, Int32 -> s32 v
    (* cast to Number Real *)
    | Number Real, Number Real -> v
    | Number Complex, Number Real ->
      if snd v = 0. then fst v
      else raise Bad_cast
    | Poly Real, Number Real -> raise Bad_cast
    | Poly Complex, Number Real -> raise Bad_cast
    | Bool, Number Real -> if v then 1. else 0.
    | String, Number Real -> raise Bad_cast
    | Int8, Number Real -> float v
    | Int16, Number Real -> float v
    | Int32, Number Real -> float v
    | Uint8, Number Real -> float v
    | Uint16, Number Real -> float v
    | Uint32, Number Real -> float v
    (* cast to Number Complex *)
    | Number Real, Number Complex -> v, 0.
    | Number Complex, Number Complex -> v
    | Poly Real, Number Complex -> raise Bad_cast
    | Poly Complex, Number Complex -> raise Bad_cast
    | Bool, Number Complex -> if v then 1., 0. else 0., 0.
    | String, Number Complex -> raise Bad_cast
    | Int8, Number Complex -> float v, 0.
    | Int16, Number Complex -> float v, 0.
    | Int32, Number Complex -> float v, 0.
    | Uint8, Number Complex -> float v, 0.
    | Uint16, Number Complex -> float v, 0.
    | Uint32, Number Complex -> float v, 0.
    (* cast to Poly Real *)
    | Number Real, Poly Real ->
      P (0., "$", ref [| v |])
    | Number Complex, Poly Real -> 
      P (0., "$", ref [| icast (Number Complex) (Number Real) v |])
    | Poly Real, Poly Real -> v
    | Poly Complex, Poly Real ->
      let P (_, variable, arr) = v in
      P (0., variable, ref (Array.map (icast (Number Complex) (Number Real)) !arr))
    | Bool, Poly Real -> P (0., "$", (if v then ref [| 1. |] else ref [| 0. |]))
    | String, Poly Real -> raise Bad_cast
    | Int8, Poly Real -> P (0., "$", ref [| float v |])
    | Int16, Poly Real -> P (0., "$", ref [| float v |])
    | Int32, Poly Real -> P (0., "$", ref [| float v |])
    | Uint8, Poly Real -> P (0., "$", ref [| float v |])
    | Uint16, Poly Real -> P (0., "$", ref [| float v |])
    | Uint32, Poly Real -> P (0., "$", ref [| float v |])
    (* cast to Poly Complex *)
    | Number Real, Poly Complex -> P ((0., 0.), "$", ref [| v, 0. |])
    | Number Complex, Poly Complex -> P ((0., 0.), "$", ref [| v |])
    | Poly Real, Poly Complex ->
      let P (_, variable, arr) = v in
      P ((0., 0.), variable, ref (Array.map (fun v -> (v, 0.)) !arr))
    | Poly Complex, Poly Complex -> v
    | Bool, Poly Complex ->
      P ((0., 0.), "$", (if v then ref [| 1., 0. |] else ref [| 0., 0. |]))
    | String, Poly Complex -> raise Bad_cast
    | Int8, Poly Complex -> P ((0., 0.), "$", ref [| float v, 0. |])
    | Int16, Poly Complex -> P ((0., 0.), "$", ref [| float v, 0. |])
    | Int32, Poly Complex -> P ((0., 0.), "$", ref [| float v, 0. |])
    | Uint8, Poly Complex -> P ((0., 0.), "$", ref [| float v, 0. |])
    | Uint16, Poly Complex -> P ((0., 0.), "$", ref [| float v, 0. |])
    | Uint32, Poly Complex -> P ((0., 0.), "$", ref [| float v, 0. |])
    (* cast to String *)
    | Number Real, String -> string_of_float v
    | Number Complex, String ->
      let re, im = v in
      let s = string_of_float re
              ^ if im = 0. then "" else " + " ^ string_of_float im ^ "i" in
      s
    | Poly Real, String -> raise Bad_cast
    | Poly Complex, String -> raise Bad_cast
    | Bool, String -> if v then "T" else "F"
    | String, String -> v
    | Int8, String -> string_of_int v
    | Int16, String -> string_of_int v
    | Int32, String -> string_of_int v
    | Uint8, String -> string_of_int v
    | Uint16, String -> string_of_int v
    | Uint32, String -> string_of_int v
    (* cast to Bool *)
    | Number Real, Bool -> v <> 0.
    | Number Complex, Bool -> v <> (0., 0.)
    | Poly Real, Bool -> raise Bad_cast
    | Poly Complex, Bool -> raise Bad_cast
    | Bool, Bool -> v
    | String, Bool -> raise Bad_cast
    | Int8, Bool -> v <> 0
    | Int16, Bool -> v <> 0
    | Int32, Bool -> v <> 0
    | Uint8, Bool -> v <> 0
    | Uint16, Bool -> v <> 0
    | Uint32, Bool -> v <> 0

  let poly_create
    : type a. a poly ntag -> string -> a poly
    = fun kind variable -> match kind with
    | Real -> P (0., variable, ref [||])
    | Complex -> P ((0., 0.), variable, ref [||])
  let poly_get (P (_, _, ar)) idx =
    let idx = if idx >= 1 then idx - 1
      else raise Bad_index in
    !ar.(idx)
  let poly_set (P (def, _, ar)) idx v =
    let idx = if idx >= 1 then idx - 1
      else raise Bad_index in
    let diff = idx - Array.length !ar + 1 in
    if diff >= 0 then
      ar := Array.concat [ !ar ; Array.make diff def ] ;
    (!ar).(idx) <- v
  let poly_length (P (_, _, ar)) =
    Array.length !ar
  let poly_variable (P (_, variable, _)) =
    variable

  let matrix_create : type a. a itag -> int -> int -> a matrix
    = fun kind wd ht ->
      let def : a = match kind with
        | Number Real -> 0.
        | Number Complex -> (0., 0.)
        | Poly kind -> poly_create kind "$"
        | String -> ""
        | Bool -> false
        | Int8 -> 0
        | Int16 -> 0
        | Int32 -> 0
        | Uint8 -> 0
        | Uint16 -> 0
        | Uint32 -> 0
      in
      M (def, ref (wd, ht, Array.make (wd * ht) def))

  let sparse_create : type a. a itag -> int -> int -> a sparse
    = fun kind wd ht ->
      let def : a = match kind with
        | Number Real -> 0.
        | Number Complex -> (0., 0.)
        | Poly kind -> poly_create kind "$"
        | String -> ""
        | Bool -> false
        | Int8 -> 0
        | Int16 -> 0
        | Int32 -> 0
        | Uint8 -> 0
        | Uint16 -> 0
        | Uint32 -> 0
      in
      S (def, ref (wd, ht, Array.make (wd * ht) def))

  let matrix_cast
    : type a b. a itag -> b itag -> a matrix -> b matrix
    = fun k1 k2 (M (def, { contents = (wd, ht, a ) })) ->
      M (icast k1 k2 def, ref (wd, ht, Array.map (icast k1 k2) a))

  let matrix_get (M (_, { contents = (wd, ht, a) })) idxi idxj =
    let idxi = if idxi >= 1 && idxi <= wd then idxi - 1
      else raise Bad_index in
    let idxj = if idxj >= 1 && idxj <= ht then idxj - 1
      else raise Bad_index in
    a.(idxi * ht + idxj)

  let matrix_get_linear (M (_, { contents = (_, ht, a) })) idx =
    let idx = if idx >= 1 && idx <= Array.length a then idx - 1
      else raise Bad_index in
    a.(idx)

  let matrix_set (M (def, ({ contents = (wd, ht, a) } as ar))) idxi idxj v =
    let idxi = if idxi >= 1 then idxi - 1
      else raise Bad_index in
    let idxj = if idxj >= 1 then idxj - 1
      else raise Bad_index in
    let a, ht =
      if idxi >= wd || idxj >= ht then
        let nwd = max (idxi + 1) wd and nht = max (idxj + 1) ht in
        let na = Array.make (nwd * nht) def in
        for i = 0 to wd - 1 do
          Array.blit a (i * ht) na (i * nht) ht
        done ;
        ar := (nwd, nht, na) ;
        (na, nht)
      else
        (a, ht)
    in
    a.(idxi * ht + idxj) <- v

  let matrix_set_linear (M (_, { contents = (_, ht, a) })) idx v =
    let idx = if idx >= 1 && idx <= Array.length a then idx - 1
      else raise Bad_index in
    a.(idx) <- v

  let matrix_size (M (_, { contents = (wd, ht, a) })) =
    (wd, ht)

  let sparse_cast
    : type a b. a itag -> b itag -> a sparse -> b sparse
    = fun k1 k2 (S (def, { contents = (wd, ht, a ) })) ->
      S (icast k1 k2 def, ref (wd, ht, Array.map (icast k1 k2) a))

  let sparse_get (S (_, { contents = (wd, ht, a) })) idxi idxj =
    let idxi = if idxi >= 1 && idxi <= wd then idxi - 1
      else raise Bad_index in
    let idxj = if idxj >= 1 && idxj <= ht then idxj - 1
      else raise Bad_index in
    a.(idxi * ht + idxj)

  let sparse_get_linear (S (_, { contents = (_, ht, a) })) idx =
    let idx = if idx >= 1 && idx <= Array.length a then idx - 1
      else raise Bad_index in
    a.(idx)

  let sparse_set (S (def, ({ contents = (wd, ht, a) } as ar))) idxi idxj v =
    let idxi = if idxi >= 1 then idxi - 1
      else raise Bad_index in
    let idxj = if idxj >= 1 then idxj - 1
      else raise Bad_index in
    let a, ht =
      if idxi >= wd || idxj >= ht then
        let nwd = max idxi wd and nht = max idxj ht in
        let na = Array.make (nwd * nht) def in
        for i = 0 to wd - 1 do
          Array.blit a (i * ht) na (i * nht) ht
        done ;
        ar := (nwd, nht, na) ;
        (na, nht)
      else
        (a, ht)
    in
    a.(idxi * ht + idxj) <- v

  let sparse_set_linear (S (_, { contents = (_, ht, a) })) idx v =
    let idx = if idx >= 1 && idx <= Array.length a then idx - 1
      else raise Bad_index in
    a.(idx) <- v

  let sparse_size (S (_, { contents = (wd, ht, a) })) =
    (wd, ht)


  let vlist_create values =
    ref (Array.of_list values)
  let vlist_length ar =
    Array.length !ar
  let vlist_get ar idx =
    let idx = if idx >= 1 && idx <= Array.length !ar then idx - 1
      else raise Bad_index in
    (!ar).(idx)
  let vlist_set ar idx v =
    let idx = if idx >= 1 then idx - 1
      else raise Bad_index in
    let diff = idx - Array.length !ar + 1 in
    if diff >= 0 then
      ar := Array.concat [ !ar ; Array.make diff (V (Null, ()))  ] ;
    (!ar).(idx) <- v

  let tlist_create label fields values =
    label, fields, vlist_create values
  let tlist_length (_, _, ar) =
    vlist_length ar
  let tlist_label (label, _, _) =
    label
  let tlist_fields (_, fields, _) =
    fields
  let rec tlist_idx field n = function
    | [] -> raise Bad_index
    | h :: t -> if h = field then n else tlist_idx field (n + 1) t
  let tlist_get (_, fields, ar) field =
    vlist_get ar (tlist_idx field 1 fields)
  let tlist_set (_, fields, ar) field v =
    vlist_set ar (tlist_idx field 1 fields) v
  let tlist_get_by_index (_, fields, ar) idx =
    vlist_get ar idx
  let tlist_set_by_index (_, fields, ar) idx v=
    vlist_set ar idx v

  let typeof (V (tag, _)) =
    T tag

  let instanceof (V (tag, _)) rtt =
    rtt = T tag

  let inject : type a. a tag -> a -> value = fun tag v ->
    match tag, v with
    | Matrix _, M (_, { contents = (0, _, _) }) -> V (Atom, ())
    | Matrix _, M (_, { contents = (_, 0, _) }) -> V (Atom, ())
    | Matrix k, M (_, { contents = (1, 1, [| v |]) }) -> V (Single k, v)
    | _ -> V (tag, v)

  let rec extract : type a. a tag -> value -> a = fun tag v ->
    match tag, v with
    | Single tag, V (Single tag', v) -> extract_single tag tag' v
    | Eye tag, V (Eye tag', v) -> extract_single tag tag' v
    | Matrix tag, V (Matrix tag', v) -> extract_matrix tag tag' v
    | Vlist, V (Vlist, v) -> v
    | Tlist name, V (Tlist name', v) when name = name' -> v
    | Mlist name, V (Mlist name', v) when name = name' -> v
    | Macro, V (Macro, v) -> v
    | Primitive, V (Primitive, v) -> v
    | Handle, V (Handle, v) -> v
    | Atom, V (Atom, v) -> v
    | Null, V (Null, v) -> v
    | _ -> raise Bad_type    

  and extract_matrix : type a b. a itag -> b itag -> b matrix -> a matrix = fun ta tb v ->
    match ta, tb with
    | Number Real, Number Real -> v
    | Number Complex, Number Complex -> v
    | Poly Real, Poly Real -> v
    | Poly Complex, Poly Complex -> v
    | String, String -> v
    | Bool, Bool -> v
    | Int8, Int8 -> v
    | Int16, Int16 -> v
    | Int32, Int32 -> v
    | Uint8, Uint8 -> v
    | Uint16, Uint16 -> v
    | Uint32, Uint32 -> v
    | _ -> raise Bad_type

  and extract_single : type a b. a itag -> b itag -> b -> a = fun ta tb v ->
    match ta, tb with
    | Number Real, Number Real -> v
    | Number Complex, Number Complex -> v
    | Poly Real, Poly Real -> v
    | Poly Complex, Poly Complex -> v
    | String, String -> v
    | Bool, Bool -> v
    | Int8, Int8 -> v
    | Int16, Int16 -> v
    | Int32, Int32 -> v
    | Uint8, Uint8 -> v
    | Uint16, Uint16 -> v
    | Uint32, Uint32 -> v
    | _ -> raise Bad_type

  let view v = v
  let repr v = v

  let cast v rtt = match v, rtt with
    | V (Single tv, v), T (Single tr) ->
      V (Single tr, icast tv tr v)
    | V (Eye tv, v), T (Single tr) ->
      V (Single tr, icast tv tr v)
    | V (Eye tv, v), T (Eye tr) ->
      V (Eye tr, icast tv tr v)
    | V (Matrix tv, v), T (Matrix tr) ->
      V (Matrix tr, matrix_cast tv tr v)
    | V (Sparse tv, v), T (Sparse tr) ->
      V (Sparse tr, sparse_cast tv tr v)
    | V (Matrix tv, v), T (Single tr) when matrix_size v = (1, 1) ->
      V (Single tr, icast tv tr (matrix_get v 1 1))
    | V (Single tv, v), T (Matrix tr) ->
      let m = matrix_create tr 1 1 in
      matrix_set m 1 1 (icast tv tr v) ;
      V (Matrix tr, m)
    | _ -> raise Bad_cast

  let rec copy (value : value) : value =
    match value with
    | V (Single tag, v) -> V (Single tag, copy_single tag v)
    | V (Eye tag, v) -> V (Eye tag, copy_single tag v)
    | V (Matrix tag, v) -> V (Matrix tag, copy_matrix tag v)
    | V (Sparse tag, v) -> V (Sparse tag, copy_sparse tag v)
    | V (Vlist, { contents = values }) ->
      V (Vlist, (ref (Array.map copy values)))
    | V (Tlist _, (name, fields, { contents = values})) ->
      V (Tlist name, (name, fields, ref (Array.map copy values)))
    | V (Mlist _, (name, fields, { contents = values})) ->
      V (Mlist name, (name, fields, ref (Array.map copy values)))
    | V (Macro, m) -> V (Macro, m)
    | V (Primitive, p) -> V (Primitive, p)
    | V (Handle, h) -> V (Handle, h)
    | V (Atom, ()) -> V (Atom, ())
    | V (Null, ()) -> V (Null, ())
  and copy_single : type a. a itag -> a -> a = fun tag v ->
    match tag, v with
    | Number Real, v -> v
    | Number Complex, v -> (fst v, snd v)
    | Poly kind, (P (def, variable, { contents = values })) ->
      let copy_one v = copy_single (Number kind) v in
      P (copy_one def, variable, ref (Array.map copy_one values))
    | String, s -> s
    | Bool, b -> b
    | Int8, i -> i
    | Int16, i -> i
    | Int32, i -> i
    | Uint8, i -> i
    | Uint16, i -> i
    | Uint32, i -> i
  and copy_matrix : type a. a itag -> a matrix -> a matrix = fun tag m ->
    let M (def, { contents = (wd, ht, values) }) = m in
    let copy_one = copy_single tag in
    M (copy_one def, ref (wd, ht, Array.map copy_one values))
  and copy_sparse : type a. a itag -> a sparse -> a sparse = fun tag m ->
    let S (def, { contents = (wd, ht, values) }) = m in
    let copy_one = copy_single tag in
    S (copy_one def, ref (wd, ht, Array.map copy_one values))

  let grab v = copy v
  let release v = ()

  let matrix_collate_vertically
      (M (def1, ({ contents = (wd1, ht1, a1) })) as m1)
      (M (def2, ({ contents = (wd2, ht2, a2) })) as m2) =
    if ht1 = 0 && wd1 = 0 then m2
    else if ht2 = 0 && wd2 = 0 then m1
    else if wd1 <> wd2 then raise Bad_index
    else
      let ht = ht1 + ht2 in
      let a = Array.make (wd1 * ht) def1 in
      for i = 0 to wd1 - 1 do
        Array.blit a1 (i * ht1) a (i * ht) ht1 ;
        Array.blit a2 (i * ht2) a (i * ht + ht1) ht2
      done ;
      M (def1, ({ contents = (wd1, ht1 + ht2, a) }))

  let matrix_collate_horizontally
      (M (def1, ({ contents = (wd1, ht1, a1) })) as m1)
      (M (def2, ({ contents = (wd2, ht2, a2) })) as m2) =
    if ht1 = 0 && wd1 = 0 then m2
    else if ht2 = 0 && wd2 = 0 then m1
    else if ht1 <> ht2 then raise Bad_index
    else
      let a = Array.concat [ a1 ; a2 ] in
      M (def1, ({ contents = (wd1 + wd2, ht1, a) }))

end
