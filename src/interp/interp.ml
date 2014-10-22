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

exception Exit_function
exception Exit_program
exception Break
exception Continue

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

  and interpret_exp_for_one_result exp =
    match interpret_exp exp with
    | [ res ] -> res
    | _ -> assert false

  and interpret_else = function
    | Some belse -> interpret_stmt belse
    | None -> ()


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
        let res = interpret_exp_for_one_result exp in
        if typeof res <> T Null then begin
          State.put state ans res ;
          message (Result ("ans", res)) ;
        end else begin
          State.clear state ans ;
          let open ScilintWarning in
          output_messages !ScilintOptions.format
            [ exp.loc, Warning (P (Variable_cleared "ans")) ]
            stderr
        end
      | Assign (lexps, exp) -> (* TODO: argn, resume, etc. *)
        let lhs = List.length lexps in
        let res = interpret_exp ~lhs exp in
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
        let vcond = interpret_exp_for_one_result cond in
        let vcond = extract (Single Bool) (cast vcond (T (Single Bool))) in
        if vcond then interpret_stmt bthen
        else interpret_else belse
      | Select { cond ; cases ; default } ->
        let vcond = interpret_exp_for_one_result cond in
        let rec loop = function
          | (case, body) :: rest ->
            let vcase = interpret_exp_for_one_result case in
            let vargs = [ None, vcase ; None, vcond ] in
            let rtts = [ typeof vcase ; typeof vcond ] in
            let open Dispatcher in
            let overloading = Binary Ast.Eq, rtts in
            let vres = match (lookup overloading !lib).call vargs with
              | [ vres ] -> vres
              | [] ->
                error (Located (case.loc, Werror (P (Too_few_results 1))))
              | vres :: rest ->
                message (Located (case.loc, Warning (P (Unused_results (List.length rest))))) ;
                vres
              | exception Not_found ->
                error (Located (cond.loc, Unbound_overloading overloading))
              | exception Interp_error err -> 
                error (Located (cond.loc, err)) in
            (*  TODO: checkcast *)
            if extract (Single Bool) (cast vres (T (Single Bool))) then
              interpret_stmt body
            else
              loop rest
          | [] -> interpret_else default in
        loop cases
      | While (cond, body, belse) ->
        let rec loop () =
          let vcond = interpret_exp_for_one_result cond in
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
    let vrange = interpret_exp_for_one_result range in
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
    let open Dispatcher in
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
            (function
              | v :: rest -> callback (rest @ [ v ])
              | _ -> raise Bad_type)
          | Injection ->
            (function
              | v :: r :: rest -> callback (rest @ [ r ; v ])
              | _ -> raise Bad_type)
          | _ -> callback
        in
        List.iter (fun rtts ->
            register
              ~policy:(Replace (ref [])) ~force:false
              ~frozen:false ~more:true
              (overloading, rtts)
              { name = name ; call ;
                takes = [] ; returns = [] } !lib)
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
          let open ScilintWarning in
          output_messages !ScilintOptions.format
            [ exp.loc, Werror (P Null_result) ;
              exp.loc, Warning (P (Variable_cleared (var_name var))) ]
            stderr
        end
      | Call (vexp, args, _) -> (* TODO: mlists, etc. *)
        let vf = recursive_extraction vexp in
        let vargs, rtts = interpret_args args in
        let vargs = (None, vf) :: (None, res) :: vargs in
        let rtts = typeof vf :: typeof res :: rtts in
        let open Dispatcher in
        let overloading = Injection, rtts in
        let vres = match (lookup overloading !lib).call vargs with
          | [ vres ] -> vres
          | [] ->
            error (Located (exp.loc, Werror (P (Too_few_results 1))))
          | vres :: rest ->
            message (Located (exp.loc, Warning (P (Unused_results (List.length rest))))) ;
            vres
          | exception Not_found ->
            error (Located (exp.loc, Unbound_overloading overloading))
          | exception Interp_error err -> 
            error (Located (exp.loc, err)) in
        injection vexp vres
      | _ ->
        let msg = "this kind of expression is not a valid assignment destination" in
        error (Located (exp.loc, Generic msg))
    and recursive_extraction = function
      | { cstr = Var var ; loc } ->
        begin try State.get state var.cstr with
          | Not_found ->
            let vf = inject Atom () in
            messages [ Located (loc, Werror (L (Uninitialized_var (State.name var.cstr)))) ;
                       Located (loc, Result (State.name var.cstr, vf)) ] ;
            vf end
      | { cstr = Call (fexp, args, _) ; loc } -> (* TODO: mlists, etc. *)
        let f = recursive_extraction fexp in
        let vargs, rtts = interpret_args args in
        let vargs = (None, f) :: vargs in
        let rtts = typeof f :: rtts in
        let open Dispatcher in
        let overloading = Recursive_extraction, rtts in
        begin match (lookup overloading !lib).call vargs with
          | [ vres ] -> vres
          | [] ->
            error (Located (exp.loc, Werror (P (Too_few_results 1))))
          | vres :: rest ->
            message (Located (exp.loc, Warning (P (Unused_results (List.length rest))))) ;
            vres
          | exception Not_found ->
            error (Located (exp.loc, Unbound_overloading overloading))
          | exception Interp_error err -> 
            error (Located (exp.loc, err))
        end
      | _ ->
        let msg = "this kind of expression is not a valid assignment destination" in
        error (Located (exp.loc, Generic msg))
    in injection exp res

  and interpret_args = function
    | (n, e) :: args ->
      let vargs, rtts = interpret_args args in
      let v = interpret_exp_for_one_result e in
      (n, v) :: vargs, typeof v :: rtts
    | [] -> [], []

  and interpret_exp ?(lhs = 1) { cstr = exp ; loc } =
    let res = match exp with
      | Error ->
        error (Located (loc, Generic "syntax error"))
      | Num f ->
        [ Values.(inject (Single (Number Real)) f) ]
      | Bool b ->
        [ Values.(inject (Single Bool) b) ]
      | String s ->
        [ Values.(inject (Single String) s) ]
      | Matrix [] ->
        [ Values.(inject Atom ()) ]
      | Cell_array rows (* TODO: what *)
      | Matrix rows ->
        let open Dispatcher in
        let rec collate_rows : Values.value list list -> Values.value = function
          | [] -> Values.(inject Atom ())
          | [ row ] -> collate_cols row
          | row1 :: row2 :: rows ->
            let v1 = collate_cols row1 in
            let v2 = collate_cols row2 in
            let overloading = Matrix_vertical_collation, [ typeof v1 ; typeof v2 ] in
            match (lookup overloading !lib).call [ None, v1 ; None, v2 ] with
            | [ v ] -> collate_rows ([ v ] :: rows)
            | [] -> error (Werror (P (Too_few_results 1)))
            | _ :: rest ->
              let n = List.length rest in
              error (Werror (P (Too_many_results n)))
            | exception Not_found ->
              error (Located (loc, Unbound_overloading overloading ))
        and collate_cols : Values.value list -> Values.value = function
          | [] -> Values.(inject Atom ())
          | [ v ] -> v
          | v1 :: v2 :: vs ->
            let overloading = Matrix_horizontal_collation, [ typeof v1 ; typeof v2 ] in
            match (lookup overloading !lib).call [ None, v1 ; None, v2 ] with
            | [ v ] -> collate_cols (v :: vs)
            | [] -> error (Werror (P (Too_few_results 1)))
            | _ :: rest ->
              let n = List.length rest in
              error (Werror (P (Too_many_results n)))
            | exception Not_found ->
              error (Unbound_overloading overloading)
        in
        let rows = List.map (fun { cstr } -> List.map interpret_exp_for_one_result cstr) rows in
        [ collate_rows rows ]
      | Unop (unop, subexp) ->
        let subv = interpret_exp_for_one_result subexp in
        let subt = Values.typeof subv in
        let open Dispatcher in
        let overloading = Unary unop, [ subt ] in
        begin try
            (lookup overloading !lib).call [ None, subv ]
          with
          | Not_found ->
            raise (Interp_error (Located (loc, Unbound_overloading overloading)))
          | Interp_error err -> 
            raise (Interp_error (Located (loc, err)))
        end
      | Op (op, lexp, rexp) ->
        let lv = interpret_exp_for_one_result lexp in
        let rv = interpret_exp_for_one_result rexp in
        let lt = Values.typeof lv in
        let rt = Values.typeof rv in
        let open Dispatcher in
        let overloading = Binary op, [ lt ; rt ] in
        begin try
            (lookup overloading !lib).call [ None, lv ; None, rv ]
          with
          | Not_found ->
            raise (Interp_error (Located (loc, Unbound_overloading overloading)))
          | Interp_error err -> 
            raise (Interp_error (Located (loc, err)))
        end
      | Var { cstr = var ; loc } ->
        begin try
            [ State.get state var ]
          with Not_found ->
            error (Located (loc, Werror (L (Uninitialized_var (State.name var)))))
        end
      | Colon ->
        begin try
            [ State.get state colon ]
          with Not_found ->
            error (Located (loc, Werror (L (Uninitialized_var ":"))))
        end
      | Call (fexp, args, _) -> (* TODO: isglobal, mlists, etc. *)
        let f = interpret_exp_for_one_result fexp in
        let vargs, rtts = interpret_args args in
        begin match typeof f with
          | T Macro ->
            let defun = Values.extract Macro f in
            interpret_macro_call loc (defun : Ast.defun_params) vargs
          | T Primitive ->
            let name = Values.extract Primitive f in
            let rtts = match vargs with
              | (_, v) :: _ -> [ typeof v ]
              | [] -> []
            in
            let open Dispatcher in
            let overloading = Function name, rtts in
            begin try (lookup overloading !lib).call vargs with
              | Not_found ->
                raise (Interp_error (Located (fexp.loc, (Unbound_overloading overloading))))
              | Interp_error err -> 
                raise (Interp_error (Located (loc, err)))
            end
          | t ->
            let vargs = (None, f) :: vargs in
            let rtts = typeof f :: rtts in
            let open Dispatcher in
            let overloading = Extraction, rtts in
            begin try (lookup overloading !lib).call vargs with
              | Not_found ->
                raise (Interp_error (Located (fexp.loc, (Unbound_overloading overloading))))
              | Interp_error err -> 
                raise (Interp_error (Located (loc, err)))
            end
        end
      | Identity exps ->
        let res = List.map interpret_exp_for_one_result exps in
        let nres = List.length res in
        if nres > lhs then
          error (Located (loc, Werror (P (Too_many_results (nres - lhs)))))
        else if List.length res < lhs then
          error (Located (loc, Werror (P (Too_few_results (lhs - nres)))))
        else res
      | Range (lexp, sexp, rexp) ->
        let lv = interpret_exp_for_one_result lexp in
        let rv = interpret_exp_for_one_result rexp in
        let lt = Values.typeof lv in
        let rt = Values.typeof rv in
        let rtts, args = match sexp with
          | None -> [ lt ; rt ],  [ None, lv ; None, rv ]
          | Some sexp -> 
            let sv = interpret_exp_for_one_result sexp in
            let st = Values.typeof sv in
            [ lt ; st ; rt ], [ None, lv ; None, sv ; None, rv ]
        in
        let open Dispatcher in
        let overloading = Colon, rtts in
        begin try (lookup overloading !lib).call args with
          | Not_found ->
            error (Located (loc, Unbound_overloading overloading))
          | Interp_error err -> 
            error (Located (loc, err))
        end in
    let rec truncate lhs = function
      | res :: rest when lhs >= 1 ->
        res :: truncate (lhs - 1) rest
      | res :: _ ->
        message (Located (loc, Warning (P (Unused_results 1)))) ; []
      | [] when lhs >= 1 ->
        error (Located (loc, Werror (P (Too_few_results 1))))
      | [] -> []
    in truncate lhs res

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
    
