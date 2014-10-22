(*  OCamlPro Scilab Toolbox - OcSciLab, scope handling implementation
 *  Copyright (C) 2014 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

(** Quick & Dirty implementation of states in pure OCaml, uses edit
    lists for O(1) access, O(1) scope opening and O(vars used) scope
    closing *)

module Make (Values : InterpValues.S)
  : InterpState.S with type value := Values.value = struct

  open Values

  exception Cannot_resume_from_toplevel

  type variable =
    { name : string ;
      id : int ;
      mutable global : value ref option ;
      mutable binding : binding }
  and binding =
    | Local of level * value ref
    | Global of level * value ref
    | Undefined
  and state =
    { table : (string, variable) Hashtbl.t ;
      mutable level : level ;
      mutable max_id : int ;
      mutable restoration : (variable * binding) list ref list }
  and level =
    int

  let compare_vars v1 v2 =
    compare v1.id v2.id

  let var state name =
    try
      Hashtbl.find state.table name
    with Not_found ->
      state.max_id <- state.max_id + 1 ;
      let id = state.max_id in
      let variable = { name ; global = None ; binding = Undefined ; id } in
      Hashtbl.add state.table name variable ;
      variable

  let name { name } =
    name
    
  let init () =
    { table = Hashtbl.create 1000 ;
      level = 0 ;
      max_id = 0 ;
      restoration = [ ref [] ] }

  let push_restoration state var =
    let rlist = List.hd state.restoration in
    rlist := (var, var.binding) :: !rlist

  let update r v =
    let v = Values.grab v in
    Values.release !r ;
    r := v

  let put ({ level ; restoration } as state) var v =
    match var with
    | { binding = Global (glevel, r) } ->
      if glevel <> level then begin
        push_restoration state var ;
        var.binding <- Local (level, ref (Values.grab v))
      end else update r v
    | { binding = Local (llevel, r) } ->
      if llevel <> level then begin
        push_restoration state var ;
        var.binding <- Local (level, ref (Values.grab v))
      end else update r v
    | { binding = Undefined } ->
      push_restoration state var ;
      var.binding <- Local (level, ref (Values.grab v))

  let clear ({ level ; restoration } as state) var =
    match var with
    | { binding = Global (glevel, r) } ->
      if glevel <> level then
        push_restoration state var ;
      var.binding <- Undefined
    | { binding = Local (llevel, r) } ->
      if llevel <> level then
        push_restoration state var
      else
        Values.release !r ;
      var.binding <- Undefined
    | { binding = Undefined } -> ()

  let clear_global ({ level ; restoration } as state) var =
    failwith "clear_global not implemented"

  let get _ var =
    match var with
    | { binding = (Global (vlevel, r) | Local (vlevel, r)) } -> !r
    | { binding = Undefined ; name } -> raise Not_found

  let grab ({ level } as state) var =
    match var with
    | { binding = (Global (vlevel, r) | Local (vlevel, r)) } ->
      if vlevel <> level then begin
        push_restoration state var ;
        let v = Values.grab !r in
        var.binding <- Local (level, ref v) ;
        v
      end else !r
    | { binding = Undefined ; name } -> raise Not_found

  let global ({ level ; restoration } as state) var =
    match var with
    | { binding = Global (glevel, r) } ->
      if glevel <> level then begin
        push_restoration state var ;
        var.binding <- Global (level, r)
      end
    | { binding = (Local (llevel, r)) ; global = Some gr } ->
      if llevel <> level then begin
        push_restoration state var ;
        var.binding <- Global (level, gr)
      end else begin
        Values.release !r ;
        var.binding <- Global (level, gr)
      end
    | { binding = (Local (llevel, r)) ; global = None } ->
      if llevel <> level then begin
        push_restoration state var ;
        let gr = ref (Values.grab !r) in
        var.global <- Some gr ;
        var.binding <- Global (level, gr)
      end else begin
        var.global <- Some r ;
        var.binding <- Global (level, r)
      end
    | { binding = Undefined ; global = Some gr } ->
      push_restoration state var ;
      var.binding <- Global (level, gr)
    | { binding = Undefined ; global = None } ->
      push_restoration state var ;
      let gr = ref Values.(grab (inject Atom ())) in
      var.binding <- Global (level, gr) ;
      var.global <- Some gr

  let is_global _ var =
    match var with
    | { binding = (Global (_, _)) } -> true
    | _ -> false

  let resume state var v =
    (* somewhat inefficient but who cares about the efficiency of resume ? *)
    if state.level <= 0 then raise Cannot_resume_from_toplevel ;
    let rlist = List.hd state.restoration in
    let prev_binding =
      try
        let rec extract acc = function
          | ((var', b) as r) :: rs ->
            if var == var' then begin
              rlist := List.rev_append acc rs ;
              b
            end else extract (r :: acc) rs
          | [] -> raise Not_found
        in extract [] !rlist
      with Not_found ->
        var.binding
    in
    let prev_state =
      { state with
        level = state.level - 1 ;
        restoration = List.tl state.restoration } in
    let prev_var = { var with binding = prev_binding } in
    put prev_state prev_var v ;
    rlist := (var, prev_var.binding) :: !rlist

  let enter_scope st =
    st.level <- st.level + 1 ;
    st.restoration <- ref [] :: st.restoration

  let exit_scope st =
    assert (st.level > 0) ;
    let diff, next = match st.restoration with
      | [] -> assert false
      | { contents = diff } :: next -> diff, next
    in
    st.restoration <- next ;
    List.iter (fun (var, binding) ->
        begin match var.binding with
          | Local (llevel, r) when llevel = st.level ->
            Values.release !r
          | _ -> ()
        end ;
        var.binding <- binding) diff ;
    st.level <- st.level - 1
end
