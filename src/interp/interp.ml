(*  OCamlPro Scilab Toolbox - OcSciLab, interpretation
 *  Copyright (C) 2014 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilintWarning
open InterpCore
open InterpMessages
open Ast
open AstUtils
open Values
open Dispatcher

exception Exit_function
exception Exit_program
exception Break
exception Continue

(** typed representation of the output context *)
type _ dest =
  | Zero : unit dest
  | One : value dest
  | Min : int -> value list dest
  | Strictly : 'res dest -> 'res dest

(** Main interpreting function *)
let rec interpret (state : state) (lib : lib) ast =
  let lib = ref lib in
  let ans = State.var state "ans" in
  let colon = State.var state ":" in
  let varargin = State.var state "varargin" in
  let varargout = State.var state "varargout" in
  let var_name { cstr } = State.name cstr in
  let rec interpret_stmt_toplevel stmt =
    try interpret_stmt stmt with
    | Interp_error err ->
      message err
    | Exit ->
      Printf.fprintf stderr "Bye.\n%!" ;
      exit 0

  and interpret_stmt ({ cstr ; loc } as stmt) =
    let var_is_function var =
      match typeof (State.get state var) with
      | T Macro -> true
      | T Primitive -> true
      | T _ -> false
      | exception Not_found -> false in
    try match cstr with
      | Exp ({ cstr = Var { cstr = var } } as exp) when var_is_function var ->
        (* hack for the toplevel *)
        let forged_call = { stmt with cstr = Call (exp, [], Shell)}in
        let forged = { stmt with cstr = Exp forged_call } in
        interpret_stmt forged
      | Exp exp ->
        let res = interpret_exp One exp in
        if typeof res <> T Null then begin
          State.put state ans res ;
          message (Result ("ans", res)) ;
        end else begin
          State.clear state ans
	(* messages [ Located (exp.loc, Warning ScilintWarning.(P (Variable_cleared "ans"))) ] *)
        end
      | Assign (lexps, exp) -> (* TODO: argn, resume, etc. *)
        let lhs = List.length lexps in
        let res = interpret_exp (Min lhs) exp in
        List.iter2 interpret_assignment (List.rev lexps) (List.rev res)
      | Defun ({ name } as defun) ->
        let res = inject Macro defun in
        State.put state name.cstr res ;
        let name = State.name name.cstr in
        message (Result (name, res)) ;
        if name.[0] = '%' then overload name (inject Macro defun) loc
      | Seq stmts ->
        List.iter interpret_stmt stmts
      | If (cond, bthen, belse) ->
        let vcond = interpret_exp One cond in
        let vcond = extract (Single Bool) (cast vcond (T (Single Bool))) in
        if vcond then interpret_stmt bthen
        else interpret_else belse
      | Select { cond ; cases ; default } ->
        let vcond = interpret_exp One cond in
        let rec loop = function
          | (case, body) :: rest ->
            let vcase = interpret_exp One case in
            let vargs = [ None, vcase ; None, vcond ] in
            let rtts = [ typeof vcase ; typeof vcond ] in
            let overloading = Binary Ast.Eq, rtts in
            let vres = interpret_overloading One case.loc overloading vargs in
            (*  TODO: checkcast *)
            if extract (Single Bool) (cast vres (T (Single Bool))) then
              interpret_stmt body
            else
              loop rest
          | [] -> interpret_else default in
        loop cases
      | While (cond, body, belse) ->
        let rec loop () =
          let vcond = interpret_exp One cond in
          if extract (Single Bool) (cast vcond (T (Single Bool))) then
            match interpret_stmt body with
            | () -> loop ()
            | exception Continue -> loop ()
            | exception Break -> ()
        in loop () ;
        interpret_else belse
      | For ({ cstr = var }, range, body) ->
        let iter = interpret_range_iterator range in
        let rec loop () =
          match iter () with
          | None -> ()
          | Some v ->
            State.put state var v ;
            match interpret_stmt body with
            | () -> loop ()
            | exception Continue -> loop ()
            | exception Break -> ()
        in loop ()
      | Try (body, catcher) ->
        begin
          try interpret_stmt body with
          | Values.Bad_cast | Values.Bad_type | Values.Bad_index
          | Interp_error _ -> interpret_stmt catcher
        end
      | Comment _ -> (* Oh yeah! *) ()
      | Return -> raise Exit_function
      | Break -> raise Break
      | Continue -> raise Continue
    with
    | Values.Bad_cast ->
      error (Located (loc, Generic "uncaught impossible conversion"))
    | Values.Bad_type ->
      error (Located (loc, Generic "uncaught type error"))
    | Values.Bad_index ->
      error (Located (loc, Generic "uncaught out of range access"))

  and interpret_range_iterator range =
    let vrange = interpret_exp One range in
    let cpt = ref (-1) in
    let current () = incr cpt ; !cpt in
    match typeof vrange with
    | T (Null) -> (fun () -> None)
    | T (Atom) -> (fun () -> None)
    | T (Single _) ->
      (fun () -> if current () = 0 then Some vrange else None)
    | T (Matrix k) ->
      let m = extract (Matrix k) vrange in
      let w, h = matrix_size m in
      let size = w * h in
      (fun () ->
         let i = current () + 1 in
         if i > size then None
         else Some (inject (Single k) (matrix_get_linear m i)))
    | T (Vlist) ->
      let l = extract Vlist vrange in
      let size = vlist_length l in
      (fun () ->
         let i = current () + 1 in
         if i > size then None
         else Some (vlist_get l i))
    | _ ->
      error (Located (range.loc, Generic "unimplemented iterator"))

  and overload name res loc =
    match parse_overloading_notation name with
    | exception Failure _ -> ()
    | overloading, rttss ->
      match view res with
      | V (Macro, defun) ->
        let call =
          let callback = interpret_macro_call loc (defun : Ast.defun_params) in
          (* we reorder params for injection and extraction
             since our implementation differs *)
          match overloading  with
          | Extraction | Recursive_extraction ->
            (fun lhs -> function
               | v :: rest -> callback (rest @ [ v ])
               | _ -> raise Bad_type)
          | Injection ->
            (fun lhs -> function
               | v :: r :: rest -> callback (rest @ [ r ; v ])
               | _ -> raise Bad_type)
          | _ -> (fun lhs args -> callback args)
        in
        List.iter (fun rtts ->
            register
              ~frozen:false ~more:true
              overloading rtts 1
              { name = name ; call ;
                takes = ([], true) ; returns = ([], true) } !lib |> ignore (* FIXME: warn *) )
          rttss
      | _ ->
        let msg = "overloading is only possible with a user function" in
        message (Located (loc, Hint msg))

  and interpret_assignment exp res =
    let rec injection exp res =
      match exp.cstr with
      | Var var ->
        if typeof res <> T Null then begin
          let name = var_name var in
          State.put state var.cstr res ;
          message (Result (name, res)) ;
          if name.[0] = '%' then overload name res exp.loc
        end else begin
          State.clear state var.cstr ;
          messages
            [ Located (exp.loc, Werror (P Null_result)) ;
              Located (exp.loc, Warning (P (Variable_cleared (var_name var)))) ]
        end
      | Call (vexp, args, _) ->
        let vf = recursive_extraction vexp in
        let vargs, rtts = interpret_args args in
        let vargs = (None, vf) :: (None, res) :: vargs in
        let rtts = typeof vf :: typeof res :: rtts in
        let overloading = Injection, rtts in
        let vres = interpret_overloading One exp.loc overloading vargs in
        injection vexp vres
      | _ ->
        let msg = "this kind of expression is not a valid assignment destination" in
        error (Located (exp.loc, Generic msg))
    and recursive_extraction = function
      | { cstr = Var var ; loc } ->
        begin try State.get state var.cstr with
          | Not_found ->
            let vf = inject Atom () in
            messages [ Located (loc, Werror ScilintOptions.(L (Uninitialized_var (State.name var.cstr)))) ;
                       Located (loc, Result (State.name var.cstr, vf)) ] ;
            vf end
      | { cstr = Call (fexp, args, _) ; loc } ->
        let f = recursive_extraction fexp in
        let vargs, rtts = interpret_args args in
        let vargs = (None, f) :: vargs in
        let rtts = typeof f :: rtts in
        let overloading = Recursive_extraction, rtts in
        interpret_overloading One exp.loc overloading vargs
      | _ ->
        let msg = "this kind of expression is not a valid assignment destination" in
        error (Located (exp.loc, Generic msg))
    in injection exp res

  and interpret_overloading
    : type res. res dest -> loc ->
      (overloading * rtt list) -> (var option * value) list -> res
    = fun dest loc (overloading, rtts) vargs ->
      let default () =
        let lhs = expected dest in
        let res = try (lookup overloading rtts lhs !lib).call lhs vargs with
          | Not_found ->
            error (Located (loc, Unbound_overloading (overloading, rtts)))
          | Interp_error err ->
            error (Located (loc, err)) in
        res
      in
      let extract_tlist_field t l f =
        try match view f with
          | V (Single String, f) ->
            let l = extract (Tlist t) l in
            [ tlist_get l f ]
          | _ -> raise Bad_index
        with Bad_index -> default ()
      in
      let inject_tlist_field t l f v =
        try match view f with
          | V (Single String, f) ->
            let l = extract (Tlist t) (grab l) in
            tlist_set l f v ;
            [ inject (Tlist t) l ]
          | _ -> raise Bad_index
        with Bad_index -> default ()
      in
      let extract_mlist_field t l f =
        try match view f with
          | V (Single String, f) ->
            let l = extract (Mlist t) l in
            [ mlist_get l f ]
          | _ -> raise Bad_index
        with Bad_index -> default ()
      in
      let inject_mlist_field t l f v =
        try match view f with
          | V (Single String, f) ->
            let l = extract (Mlist t) (grab l) in
            mlist_set l f v ;
            [ inject (Mlist t) l ]
          | _ -> raise Bad_index
        with Bad_index -> default ()
      in
      let extract_tlist_index t l f =
        match view f with
        | V (Single (Number Real), f) ->
          let l = extract (Tlist t) l in
          let f = int_of_float f in
          if f = 1 then
            let fields = tlist_fields l in
            let m = matrix_create String (List.length fields + 1) 1 in
            matrix_set_linear m 1 t ;
            List.iteri (fun i f -> matrix_set_linear m (i + 2) f) fields ;
            [ inject (Matrix String) m ]
          else
            let f = f - 1 in
            [ tlist_get_by_index l f ]
        | _ -> raise Bad_index
      in
      let inject_tlist_index t l f v =
        match view f with
        | V (Single (Number Real), f) ->
          let l = extract (Tlist t) (grab l) in
          let f = int_of_float f in
          if f = 1 then
            let length = tlist_length l in
            let rec values n =
              if n > length then []
              else tlist_get_by_index l n :: values (n + 1) in
            [ inject Vlist (vlist_create (v :: values 1)) ]
          else
            let f = f - 1 in
            tlist_set_by_index l f v ;
            [ inject (Tlist t) l ]
        | _ -> raise Bad_index
      in
      let predef () = match overloading, rtts, vargs with
        | Recursive_extraction, [ T (Mlist t) ; T (Single String) ], [ _, tl ; _, f ] ->
          extract_mlist_field t tl f
        | Extraction, [ T (Mlist t) ; T (Single String) ], [ _, tl ; _, f ] ->
          extract_mlist_field t tl f
        | Injection, [ T (Mlist t) ; _ ; T (Single String) ], [ _, tl ; _, v ; _, f ] ->
          inject_mlist_field t tl f v
        | Recursive_extraction, [ T (Tlist t) ; T (Single String) ], [ _, tl ; _, f ] ->
          extract_tlist_field t tl f
        | Extraction, [ T (Tlist t) ; T (Single String) ], [ _, tl ; _, f ] ->
          extract_tlist_field t tl f
        | Injection, [ T (Tlist t) ; _ ; T (Single String) ], [ _, tl ; _, v ; _, f ] ->
          inject_tlist_field t tl f v
        | Recursive_extraction, [ T (Tlist t) ; T (Single (Number Real)) ], [ _, tl ; _, f ] ->
          extract_tlist_index t tl f
        | Extraction, [ T (Tlist t) ; T (Single (Number Real)) ], [ _, tl ; _, f ] ->
          extract_tlist_index t tl f
        | Injection, [ T (Tlist t) ; _ ; T (Single (Number Real)) ], [ _, tl ; _, v ; _, f ] ->
          inject_tlist_index t tl f v
        | _ -> default ()
      in
      filter dest loc (predef ())

  and interpret_args = function
    | (n, e) :: args ->
      let vargs, rtts = interpret_args args in
      let v = interpret_exp One e in
      (n, v) :: vargs, typeof v :: rtts
    | [] -> [], []

  and interpret_exp
    : type res. res dest -> exp -> res
    = fun dest { cstr = exp ; loc } ->
      match exp with
      | Error ->
        error (Located (loc, Generic "syntax error"))
      | Num f ->
        filter dest loc [ Values.(inject (Single (Number Real)) f) ]
      | Bool b ->
        filter dest loc [ Values.(inject (Single Bool) b) ]
      | String s ->
        filter dest loc [ Values.(inject (Single String) s) ]
      | Matrix [] ->
        filter dest loc [ Values.(inject Atom ()) ]
      | Cell_array rows (* TODO: what *)
      | Matrix rows ->
        let rec collate_rows : Values.value list list -> Values.value = function
          | [] -> Values.(inject Atom ())
          | [ row ] -> collate_cols row
          | row1 :: row2 :: rows ->
            let v1 = collate_cols row1 in
            let v2 = collate_cols row2 in
            let overloading = Matrix_vertical_collation, [ typeof v1 ; typeof v2 ] in
            let v = interpret_overloading One loc overloading [ None, v1 ; None, v2 ] in
            collate_rows ([ v ] :: rows)
        and collate_cols : Values.value list -> Values.value = function
          | [] -> Values.(inject Atom ())
          | [ v ] -> v
          | v1 :: v2 :: vs ->
            let overloading = Matrix_horizontal_collation, [ typeof v1 ; typeof v2 ] in
            let v = interpret_overloading One loc overloading [ None, v1 ; None, v2 ] in
            collate_cols (v :: vs)
        in
        let rows = List.map (fun { cstr } -> List.map (interpret_exp One) cstr) rows in
        filter dest loc [ collate_rows rows ]
      | Unop (unop, subexp) ->
        let subv = interpret_exp One subexp in
        let subt = Values.typeof subv in
        let overloading = Unary unop, [ subt ] in
        interpret_overloading dest loc overloading [ None, subv ]
      | Op (op, lexp, rexp) ->
        let lv = interpret_exp One lexp in
        let rv = interpret_exp One rexp in
        let lt = Values.typeof lv in
        let rt = Values.typeof rv in
        let overloading = Binary op, [ lt ; rt ] in
        interpret_overloading dest loc overloading [ None, lv ; None, rv ]
      | Var { cstr = var ; loc } ->
        begin try
            filter dest loc [ State.get state var ]
          with Not_found ->
            error (Located (loc, Werror (L (Uninitialized_var (State.name var)))))
        end
      | Colon ->
        begin try
            filter dest loc [ State.get state colon ]
          with Not_found ->
            error (Located (loc, Werror (L (Uninitialized_var ":"))))
        end
      | Call (fexp, args, _) -> (* TODO: isglobal, mlists, etc. *)
        let f = interpret_exp One fexp in
        let vargs, rtts = interpret_args args in
        begin match typeof f with
          | T Macro ->
            let defun = Values.extract Macro f in
            filter dest loc (interpret_macro_call loc (defun : Ast.defun_params) vargs)
          | T Primitive ->
            let name = Values.extract Primitive f in
            let rtts = match vargs with
              | (_, v) :: _ -> [ typeof v ]
              | [] -> []
            in
            let overloading = Function name, rtts in
            interpret_overloading dest loc overloading vargs
          | t ->
            let vargs = (None, f) :: vargs in
            let rtts = typeof f :: rtts in
            let overloading = Extraction, rtts in
            interpret_overloading dest loc overloading vargs
        end
      | Identity exps ->
        let res = List.map (interpret_exp One) exps in
        filter (Strictly dest) loc res
      | Range (lexp, sexp, rexp) ->
        let lv = interpret_exp One lexp in
        let rv = interpret_exp One rexp in
        let lt = Values.typeof lv in
        let rt = Values.typeof rv in
        let rtts, vargs = match sexp with
          | None -> [ lt ; rt ],  [ None, lv ; None, rv ]
          | Some sexp ->
            let sv = interpret_exp One sexp in
            let st = Values.typeof sv in
            [ lt ; st ; rt ], [ None, lv ; None, sv ; None, rv ]
        in
        let overloading = Colon, rtts in
        interpret_overloading dest loc overloading vargs

  and interpret_else = function
    | Some belse -> interpret_stmt belse
    | None -> ()

  and filter
    : type res. res dest -> loc -> ?strict:bool -> value list -> res
    = fun result loc ?(strict = false) res ->
      match result with
      | Strictly result ->
        filter result loc ~strict:true res
      | Zero ->
        begin match res with
          | [] -> ()
          | over ->
            let nover = List.length over in
            if strict then
              error (Located (loc, Werror (P (Too_many_results nover))))
            else
              message (Located (loc, Warning (P (Unused_results nover))))
        end
      | Min lhs ->
        let rec min_results lhs = function
          | res :: rest when lhs >= 1 ->
            res :: min_results (lhs - 1) rest
          | [] when lhs >= 1 ->
            error (Located (loc, Werror (P (Too_few_results lhs))))
          | [] -> []
          | over ->
            let nover = List.length over in
            if strict then
              error (Located (loc, Werror (P (Too_many_results nover))))
            else
              message (Located (loc, Warning (P (Unused_results nover)))) ;
            []
        in min_results lhs res
      | One ->
        begin match res with
          | [ res ] -> res
          | res :: over ->
            let nover = List.length over in
            if strict then
              error (Located (loc, Werror (P (Too_many_results nover))))
            else
              message (Located (loc, Warning (P (Unused_results nover)))) ;
            res
          | [] ->
            error (Located (loc, Werror (P (Too_few_results 1))))
        end

  and expected : type res. res dest -> int = function
    | Strictly result -> expected result
    | Zero -> 0
    | One -> 1
    | Min lhs -> lhs

  and interpret_macro_call loc defun vargs =
    (* right to left name assignment *)
    let rec assign_args acc args vargs =
      match args, vargs with (* TODO: null () argument *)
      | [], [] -> acc
      | [ { cstr = var } ], rest when var == varargin ->
        assign_varargin [] var rest :: acc
      | rargs, [] -> assign_missing (List.length rargs) acc rargs
      | arg :: rargs, (None, v) :: tvargs ->
        assign_args  ((arg.cstr, v) :: acc) rargs tvargs
      | _ :: rargs, (Some { cstr = n }, v) :: tvargs ->
        assign_args  ((n, v) :: acc) rargs tvargs
      | [], _ :: _ ->
        let nb = List.length vargs in
        error (Located (loc, (Werror (P (Too_many_arguments nb)))))
    and assign_missing nb acc args =
      match args with
      | [ { cstr = var } ] when var == varargin -> acc
      | _ :: args -> assign_missing nb acc args
      | [] ->
        message (Located (loc, (Warning (P (Too_few_arguments nb))))) ;
        acc
    and assign_varargin acc var vargs =
      match vargs with
      | [] ->
        (var, Values.(inject Vlist (vlist_create (List.rev acc))))
      | (None, v) :: rest ->
        assign_varargin (v :: acc) var rest
      | (Some lvar, v) :: rest ->
        message
          (Located (lvar.loc,
                    (Warning (P (Unused_argument_label (State.name lvar.cstr)))))) ;
        assign_varargin (v :: acc) var rest
    in
    let rec assign_rets acc rets =
      match rets with
      | [ { cstr = var } ] when var == varargout ->
        begin match Values.(extract Vlist (State.get state var)) with
          | l ->
            let rec unroll acc i =
              if i = 0 then acc else unroll (Values.vlist_get l i :: acc) (i - 1)
            in List.rev_append acc (unroll [] (Values.vlist_length l))
          | exception Not_found ->
            error (Werror (L (Unset_ret "varargout")))
          | exception Bad_type ->
            raise (Interp_error (Generic "return variable varargout should be a list"))
        end
      | { cstr = var } :: rs ->
        begin match State.get state var with
          | v -> assign_rets (v :: acc) rs
          | exception Not_found ->
            error (Werror (L (Unset_ret (State.name var))))
        end
      | [] -> List.rev acc
    in
    let arg_assignments = assign_args [] defun.args vargs in
    State.enter_scope state ;
    let old_lib = !lib in
    lib := Dispatcher.dup !lib ;
    try
      List.iter
        (fun (n, v) -> State.put state n v)
        arg_assignments ;
      interpret_stmt defun.body ;
      let rets = assign_rets [] defun.rets in
      State.exit_scope state ;
      lib := old_lib ;
      rets
    with err ->
      State.exit_scope state ;
      lib := old_lib ;
      match err with
      | Interp_error err ->
        raise (Interp_error (Located (loc, err)))
      | err -> raise err
  in
  List.iter interpret_stmt_toplevel (from_parser state ast)
