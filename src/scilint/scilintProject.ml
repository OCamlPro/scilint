
let scilint_home = try
  let dir = Sys.getenv "SCILINT_HOME" in
  if Sys.file_exists dir then begin
    Printf.eprintf "Warning: dir specified by SCILINT_HOME does not exist\n%!";
    dir
  end else raise Not_found
with Not_found ->
  let exe_name = Sys.argv.(0) in
  let path = ScilabUtils.split_simplify exe_name '/' in (* TODO: windows *)
  match List.rev path with
  | _ :: "bin" :: _ -> (* installed -> PREFIX/share/scilint *)
    Filename.concat
      (Filename.concat
         (Filename.dirname (Filename.dirname exe_name))
         "share") "scilint"
  | _ :: "scilint" :: "_obuild" :: _ ->
    (* produced by ocp-build -> SRCDIR/share *)
    Filename.concat
      (Filename.dirname (Filename.dirname (Filename.dirname exe_name)))
      "share"
  | _ :: "scilint" :: _ -> (* produced by Makefile -> SRCDIR/share *)
    Filename.concat
      (Filename.dirname (Filename.dirname exe_name))
      "share"
  | _ -> "/usr/share/scilint" (* other cases *)

let path = ref []

let add_to_path dir =
  path := !path @ [dir]

type fun_decl = {
  fun_name : string;
  fun_args : string array;
  fun_loc : string * ScilabAst.location;
}

type fun_status =
    FunDeclared of fun_decl
  | FunFile of string
  | FunPrimitive
  | FunUnknown

let funs = Hashtbl.create 1113
let arguments = Hashtbl.create 1113

let init_primitives () =
  try
    let prims = ScilabUtils.lines_of_file
        (Filename.concat scilint_home "primitives.scilint") in
    List.iter (fun p ->
      Hashtbl.add funs p FunPrimitive
    ) prims
  with exc ->
    Printf.eprintf "Warning: exception %S during initialization\n%!"
      (Printexc.to_string exc)

let init_arguments () =
  try
    let prims = ScilabUtils.lines_of_file
        (Filename.concat scilint_home "arguments.scilint") in
    List.iter (fun p ->
      match ScilabUtils.split_simplify p ':' with
        prim :: arg_num :: enum ->
        Hashtbl.add arguments (prim, int_of_string arg_num) enum
      | _ -> assert false
    ) prims
  with exc ->
    Printf.eprintf "Warning: exception %S during initialization\n%!"
      (Printexc.to_string exc)

let init =
  let initialized = ref false in
  function () ->
    if not !initialized then begin
      initialized := true;
      init_primitives ();
      init_arguments ();
    end

let declare_function fun_decl =
  init ();
  ()

let find_function fun_name =
  init ();
  try Hashtbl.find funs fun_name with Not_found ->
    let basename = fun_name ^ ".sci" in
    let rec find_in_path path basename =
      match path with
        [] -> FunUnknown
      | dir :: tail ->
        let filename = Filename.concat dir basename in
        if Sys.file_exists filename then begin
          let res = FunFile filename in
          Hashtbl.add funs fun_name res;
          res
        end
        else
          find_in_path tail basename
    in
    find_in_path !path basename

let find_argument fun_name arg_num =
  init ();
  try
    Some (Hashtbl.find arguments (fun_name, arg_num))
  with Not_found -> None
