 (*  OCamlPro Scilab Toolbox - Scifind, syntax aware search & replace for scilab
  *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
  *
  *  This file must be used under the terms of the CeCILL.
  *  This source file is licensed as described in the file COPYING, which
  *  you should have received as part of this distribution.
  *  The terms are also available at
  *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabParserAst
open ScilabFiveParser
open ScilabLocations
open Printf

let print_memory = ref false
let print_memory_arg =
  ("-show-groups", Arg.Set print_memory,
   "Display the sub-expressions matched by named patterns")

let matches stmt pat =
  let memory = ref [] in
  let memorize n stmt =
    let len = String.length n in
    if len > 0 && n.[0] = '%' then
      if len > 1 then
        try
          if n.[1] = '%' || n.[1] = '_' then
            if len = 2 then true else
              let n = int_of_string (String.sub n 2 (len - 2)) in
              memory := (n, stmt) :: !memory ;
              true
          else false
        with _ -> false
      else false
    else false
  in
  let rec match_stmt stmt pat =
    match stmt.cstr, pat.cstr with
    | _, Exp { cstr = Var { cstr = n } } when memorize n stmt -> true
    | _, Seq [ { cstr = Exp { cstr = Var { cstr = n } } } ]  when memorize n stmt -> true
    | Seq (shd :: stl), Seq ({ cstr = Exp { cstr = Var { cstr = "%%" } } } :: ptl) ->
      match_stmt stmt ({ pat with cstr = Seq ptl })
      || match_stmt ({ stmt with cstr = Seq stl }) pat
    | Seq [], Seq [] -> true
    | Seq [], Seq _ | Seq _, Seq [] -> false
    | Seq (shd :: stl), Seq (phd :: ptl) ->
      match_stmt shd phd
      && match_stmt ({ stmt with cstr = Seq stl }) ({ pat with cstr = Seq ptl })
    | _, Seq _ -> match_stmt { stmt with cstr = Seq [ stmt ] } pat
    | Assign (lefts, right), Assign (plefts, pright) ->
      match_exps lefts plefts
      && match_exp right pright
    | Defun d, Defun p ->
      match_var d.name p.name
      && match_params d.args p.args
      && match_params d.rets p.rets
      && match_stmt d.body p.body
    | Exp exp, Exp pexp ->
      match_exp exp pexp
    | Comment text, Comment ptext ->
      match_string stmt text ptext
    | For (it, range, body), For (pit, prange, pbody) ->
      match_var it pit
      && match_exp range prange
      && match_stmt body pbody
    | If (cond, tbody, Some fbody), If (pcond, ptbody, Some pfbody)
    | While (cond, tbody, Some fbody), While (pcond, ptbody, Some pfbody) ->
      match_exp cond pcond
      && match_stmt tbody ptbody
      && match_stmt fbody pfbody
    | If (cond, tbody, None), If (pcond, ptbody, None)
    | While (cond, tbody, None), While (pcond, ptbody, None) ->
      match_exp cond pcond
      && match_stmt tbody ptbody
    | Select ({ default = None } as s), Select ({ default = None } as p) ->
      match_exp s.cond p.cond
      && match_cases s.cases p.cases
    | Select ({ default = Some sd } as s), Select ({ default = Some pd } as p) ->
      match_exp s.cond p.cond
      && match_cases s.cases p.cases
      && match_stmt sd pd
    | Try (tbody, cbody), Try (ptbody, pcbody) ->
      match_stmt tbody ptbody
      && match_stmt cbody pcbody
    | Return, Return -> true
    | Break, Break -> true
    | Continue, Continue -> true
    | _, _ -> false
  and match_exps exps pats =
    match exps, pats with
    | _, [ { cstr = Var { cstr = "%%" } } ] -> true
    | ehd :: etl, { cstr = Var { cstr = "%%" } } :: ptl ->
      match_exps exps ptl || match_exps etl pats
    | ehd :: etl, phd :: ptl ->
      match_exp ehd phd && match_exps etl ptl
    | [], [] -> true
    | [], _ | _, [] -> false
  and match_params params pats =
    let module SS = Set.Make (String) in
    let pats, wildcard =
      List.fold_left
        (fun (r, w) { cstr } -> if cstr = "%%" then r, true else cstr :: r, w)
        ([], false) pats
    and params = List. map (fun { cstr } -> cstr) params in
    let pats = List.sort compare pats in
    let params = List.sort compare params in
    let rec cmp params pats =
      match params, pats with
      | n :: ns, p :: ps ->
        if n = p then cmp ns ps
        else if n < p && wildcard then cmp ns pats
        else false
      | [], [] -> true
      | [], _ -> false
      | _, [] -> wildcard
    in cmp pats params
  and match_args args pats =
    (* TODO: order, wildcards *)
    match args, pats with
    | (Some n, ahd) :: atl, (Some pn, phd) :: ptl ->
      match_var n pn
      && match_exp ahd phd
      && match_args atl ptl 
    | (None, ahd) :: atl, (None, phd) :: ptl ->
      match_exp ahd phd
      && match_args atl ptl 
    | [], [] -> true
    | _, _ -> false
  and match_cases cases pats =
    (* TODO: order, wildcards *)
    match cases, pats with
    | (c, ahd) :: atl, (pc, phd) :: ptl ->
      match_exp c pc
      && match_stmt ahd phd
      && match_cases atl ptl 
    | [], [] -> true
    | _, _ -> false
  and match_var name pat =
    memorize pat.cstr { name with cstr = Exp { name with cstr = Var name } }
    || name.cstr = pat.cstr
  and match_string stmt ctns pat =
    memorize pat stmt
    || ctns = pat
  and match_rows rows prows =
    (* TODO: better wildcards ? *)
    match rows, prows with
    | r :: rs, pr :: prs ->
      match_exps r.cstr pr.cstr
    | [], [] -> true
    | _, _ -> false
  and match_exp exp pat =
    let stmt = { exp with cstr = Exp exp } in
    match exp.cstr, pat.cstr with
    | Error, _ -> false
    | _, Error -> false
    | _, Var { cstr = n } when memorize n stmt -> true
    | Call (name, args, kind), Call (pname, pargs, pkind) ->
      match_exp name pname
      && match_args args pargs
    | Identity args, Identity pargs ->
      match_exps args pargs
    | Range (sexp, None, eexp),
      Range (psexp, (None | Some { cstr = Var { cstr = "%%" } } ), peexp) ->
      match_exp sexp psexp
      && match_exp eexp peexp
    | Range (sexp, Some stepexp, eexp), Range (psexp, Some pstepexp, peexp) ->
      match_exp sexp psexp
      && match_exp eexp peexp
      && match_exp stepexp pstepexp
    | Var sym, Var psym ->
      match_var sym psym
    | Matrix rows, Matrix prows
    | Cell_array rows, Cell_array prows ->
      match_rows rows prows
    | Unop (unop, exp), Unop (punop, pexp) ->
      unop = punop
      && match_exp exp pexp
    | Op (op, lexp, rexp), Op (pop, plexp, prexp) ->
      op = pop
      && match_exp lexp plexp
      && match_exp rexp prexp
    | Bool n, Bool p -> n = p
    | Num n, Num p -> n = p
    | String s, String p -> match_string stmt s p
    | Colon, Colon -> true
    | _, _ -> false
  in
  match_stmt stmt pat, memory

let search ast pat =
  let wildcard = ghost (Exp (ghost (Var (ghost "%%")))) in
  let ppat = ghost (Seq ([ wildcard ] @ pat @ [ wildcard ])) in
  let result = ref [] in
  let rec search_seq acc ast pat =
    match ast, pat with
    | shd :: stl, phd :: ptl ->
      if fst (matches shd phd) then search_seq (shd :: acc) stl ptl ;
      search_seq [] stl pat
    | _, [] when acc <> [] ->
      result := (res acc, []) :: !result 
    | _ -> ()
  and res acc =
    let acc = List.rev acc in
    match acc with
    | [] -> ghost (Seq [])
    | [ res ] -> res
    | hd :: tl ->
      let rec stop = function
        | [] -> assert false
        | [ { loc = _, (_, stop) } ] -> stop
        | _ :: tl -> stop tl
      in
      let source, (start, _) = hd.loc in
      let stop = stop acc in
      { loc = source, (start, stop) ; cstr = Seq acc ;
        meta = [] ; comment = [] }
  in
  let collector = object (self)
    inherit ast_iterator as dad
    method! stmt stmt =
      match stmt.cstr, pat with
      | Seq ctns, [ _ ] ->
        List.iter self # stmt ctns
      | Seq ctns, _ ->
        List.iter self # stmt ctns ;
        search_seq [] ctns pat
      | Exp _, _ ->
        dad # stmt stmt
      | _ ->
        let res, mem = matches stmt ppat in
        if res then
          result := (stmt, !mem) :: !result ;
        dad # stmt stmt
    method! exp exp =
      let stmt = { exp with cstr = Exp exp } in
        let res, mem = matches stmt ppat in
        if res then
          result := (stmt, !mem) :: !result ;
      dad # exp exp
  end in
  let ast = res (List.rev ast) in
  collector # stmt ast ;
  !result


(** called by the main on each code source passed on th CLI *)
let search_in_source pattern source =
  let parse () =
    match source with
    | File fn -> parse_file fn
    | String (name, str) -> parse_string name str
    | _ -> assert false
  in 
  let ast = parse () in
  let places = search ast pattern in
  List.iter (fun (stmt, mem) ->
      printf "Occurence found at %s.\n"
        (ScilintWarning.string_of_loc stmt.loc) ;
      if !ScilintOptions.print_ast then begin
        printf "Raw syntax tree:\n" ;
        Sexp.pretty_output stdout [ stmt ] ;
        printf "\n"
      end ;
      if !ScilintOptions.pretty_print then begin
        printf "Pretty printed:\n" ;
        Pretty.pretty_output stdout [ stmt ] ;
        printf "\n"
      end ;
      if !print_memory then begin
        printf "Matched groups:\n" ;
        List.iter
          (fun (n, stmt) ->
             Printf.printf " - %%%i: " n ;
             Pretty.compact_output stdout [ stmt ] ;
             printf "\n")
          (List.sort compare mem) ;
      end)
    places

(** a small toplevel for experimentation purposes *)
let interactive pattern =
  ScilintOptions.print_ast := true ;
  let rec interp acc nb =
    let open Printf in
    Printf.printf "--> %!" ;
    let phrase =
      try input_line stdin
      with End_of_file -> exit 0
    in
    if phrase = "" then begin
      search_in_source pattern
        (String ("input-" ^ string_of_int nb, acc)) ;
      interp "" (succ nb)
    end else
      let acc = if acc = "" then acc else acc ^ "\n" in
      interp (acc ^ phrase) nb
  in
  printf "Welcome to Scifind's interactive mode\n%!" ;
  printf "Type your phrases, leave an empty line to submit, Ctrl-C to quit\n%!" ;
  interp "" 0

(** where the args are passed and all the fun starts *)
let main () =
  let open ScilintOptions in
  let sources : source list ref = ref [] in
  let options =
    [ print_ast_arg ; pretty_print_arg ; print_memory_arg ; print_time_arg ;
      format_arg ; toplevel_mode_arg ; cli_input_arg sources ]
  and usage_msg =
    "Hello, I am Scifind, a search & replace tool for Scilab.\n\
     Usage: scifind <pattern> [OPTIONS] <file1.sci> <file2.sci> ...\n\
    Where <patterns> is a scilab code extract with optional jokers\n\
    \  - %% means any single expression\n\
    \  - %_ means any (possibly empty) sequence of expressions\n\
    \  If a pattern is followed by a number (%%1, %%_3, ...), the matched expression\n\
    \  or sequence is stored in a correspondingly numbered memory cell.\n\
    \  It can then be reused in a -replace pattern or displayed using -show-groups.\n\
    Options:" ;
  in
  Arg.parse options (cli_input_anon sources) usage_msg ;
  let parse_pattern pattern =
    let pattern = parse_string "pattern" pattern in
    if !ScilintOptions.print_ast then begin
      printf "Raw syntax tree of pattern:\n" ;
      Sexp.pretty_output stdout pattern ;
      printf "\n"
    end ;
    if !ScilintOptions.pretty_print then begin
      printf "Pretty printed pattern:\n" ;
      Pretty.pretty_output stdout pattern ;
      printf "\n"
    end ;
    pattern
  in
  match List.rev !sources with
  | [ ScilabLocations.File pattern ] when !toplevel_mode ->
    interactive (parse_pattern pattern)
  | ScilabLocations.File pattern :: sources ->
    List.iter (search_in_source (parse_pattern pattern)) sources
  | _ -> Arg.usage options usage_msg

let _ = main ()
