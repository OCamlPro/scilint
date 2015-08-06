(*  OCamlPro Scilab Toolbox - OcSciLab, standard library functions
 *  Copyright (C) 2014 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open InterpCore
open InterpCore.Values
open InterpCore.Dispatcher
open InterpMessages
open ScilintWarning

(** A global store for registering libraries or primitives, simply
    encoded as functions that transform the interpreter's state *)
let libraries : (state -> lib -> unit) list ref =
  ref []

let register_library f =
  libraries := f :: !libraries

let load_libraries state lib =
  List.iter (fun f -> f state lib) !libraries

(* from there: typed interface to the library, (* WIP *) *)

let register_primitive ?(more = false) ?(force = false) ?name lib (overloading, takes, returns) call =
  let name = match name with Some name -> name | None -> "(unnamed)" in
  let loc = ScilabLocations.External name, ((0,0), (0,0)) in
  let call lhs args =
    try call lhs args with
    | Interp_error err -> error (Located (loc, err))
    | Bad_type -> error (Located (loc, Generic "bad type"))
    | Bad_index -> error (Located (loc, Generic "bad index"))
    | Bad_cast -> error (Located (loc, Generic "bad cast"))
  in
  let primitive =
    { call ; name } in
  let frozen = true in
  try Dispatcher.(register ~frozen ~more overloading takes returns primitive lib |> ignore)
  with Failure "register" ->
    let msg = "cannot redefine this protected primitive" in
    if not force then raise (Interp_error (Located (loc, Generic msg)))

type (_, _, _) funtag =
  | Take : 'a argtag * ('b, 'b -> 'c, 'r) funtag -> ('a, 'a -> 'b -> 'c, 'r) funtag
  | Return :'a argtag *  'b argtag -> ('a, 'a -> 'b, 'b) funtag

and _ argtag =
  | Arg : 'a tag -> 'a argtag
  | Flag : (string * 'a) list -> 'a argtag
  | Seq : 'a argtag -> 'a list argtag
  | Opt : 'a argtag -> 'a option argtag
  | Pair : 'a argtag * 'b argtag -> ('a * 'b) argtag
  | Fake : 'a -> 'a argtag
  | Any : value argtag

(** Parameter constructors *)
let real = Arg (Single (Number Real))
let complex = Arg (Single (Number Complex))
let int8 = Arg (Single Int8)
let int16 = Arg (Single Int16)
let int32 = Arg (Single Int32)
let uint8 = Arg (Single Uint8)
let uint16 = Arg (Single Uint16)
let uint32 = Arg (Single Uint32)
let bool = Arg (Single Bool)
let flag flags = Flag flags
let string = Arg (Single String)
let null = Arg Null
let void = Fake ()
let vlist = Arg Vlist
let tlist n = Arg (Tlist n)
let mlist n = Arg (Mlist n)
let atom = Arg Atom
let any = Any
let matrix = function
  | Arg (Single k) -> Arg (Matrix k)
  | _ -> assert false
let poly = function
  | Arg (Single (Number k)) -> Arg (Single (Poly k))
  | _ -> assert false
let eye = function
  | Arg (Single k) -> Arg (Eye k)
  | _ -> assert false
let seq t = Seq t
let opt t = Opt t
let (@+) tl tr = Pair (tl, tr)

(** Main function builder combinator *)
let ( @-> )
  : type a b. a argtag -> b argtag -> (a, a -> b, b) funtag
  = fun l r -> Return (l, r)

(** Function argument builder combinator *)
let ( @* )
  : type a b c r. a argtag -> (b, b -> c, r) funtag -> (a, a -> b -> c, r) funtag
  = fun l r -> Take (l, r)

let rec arg_matcher : type p. p argtag -> matcher = function
  | Pair (lt, _) -> arg_matcher lt
  | Arg at -> Typed (T at)
  | Flag _ -> Typed (T (Single String))
  | Seq t -> arg_matcher t
  | Opt t -> arg_matcher t
  | Any -> Any
  | Fake _ -> raise Not_found

let rec first_arg_matcher : type a b r. (a, a -> b, r) funtag -> matcher = function
  | Return (Fake _, _) -> raise Not_found
  | Take (Fake _, (Take _ as r)) -> first_arg_matcher r
  | Take (Fake _, (Return _ as r)) -> first_arg_matcher r
  | Return (a, _) -> arg_matcher a
  | Take (a, _) -> arg_matcher a

let rec arity : type p. p argtag -> int = function
  | Pair (lt, rt) -> arity lt + arity rt
  | Arg at -> 1
  | Flag _ -> 1
  | Seq _ -> invalid_arg "InterpLib.arity"
  | Opt _ -> invalid_arg "InterpLib.arity"
  | Any -> 1
  | Fake _ -> 0

let rec return_arity : type a b r. (a, a -> b, r) funtag -> int = function
  | Return (_, r) -> arity r
  | Take (_, r) -> return_arity r

let decode_flag s fs =
  try List.assoc s fs with Not_found ->
    let rec msg = function
      | [] -> ""
      | [ single, _ ] -> single
      | [ prev, _ ; last, _ ] -> prev ^ " or " ^ last
      | (first, _) :: rest -> first ^ ", " ^ msg rest in
    error (Generic ("flag " ^ s ^ " unrecognised, expecting " ^ msg fs))

let rec get_arg
  : type a. a argtag -> (Ast.var option * value) list ->
    Ast.var option * a * (Ast.var option * value) list
  = fun t l -> match l, t with
    | rest, Fake v -> None, v, rest
    | [], Seq _ -> None, [], []
    | [], Opt _ -> None, None, []
    | [], Arg Null -> None, (), []
    | [], _ -> error (Werror (P (Too_few_arguments 1)))
    | (name, v) :: vs, Flag fs ->
      let s = extract (Single String) v in
      let enc = decode_flag s fs in
      name, enc, vs
    | (name, v) :: vs, Arg t -> name, extract t v, vs
    | (name, v) :: vs, Any -> name, v, vs
    | (name, v) :: vs, Opt (Flag fs) ->
      let s = extract (Single String) v in
      let enc = decode_flag s fs in
      name, Some enc, vs
    | (name, v) :: vs, Opt (Arg t) -> name, Some (extract t v), vs
    | (name, v) :: vs, Opt Any -> name, Some v, vs
    | (name, v) :: vs, Seq Any ->
      let _, vrest, vs = get_arg t vs in
      None, v :: vrest, vs
    | (name, v) :: vs, Seq (Arg tv) ->
      let _, vrest, vs = get_arg t vs in
      None, extract tv v :: vrest, vs
    | (name, v) :: vs, Seq (Flag fs) ->
      let s = extract (Single String) v in
      let enc = decode_flag s fs in
      let _, vrest, vs = get_arg t vs in
      None, enc :: vrest, vs
    | l, Pair (tl, tr) ->
      let _, vl, l = get_arg tl l in
      let _, vr, l = get_arg tr l in
      None, (vl, vr), l
    | (_, _) :: vs, Seq _ -> assert false
    | (_, _) :: vs, Opt _ -> assert false

let rec inject_result : type a. a argtag -> a -> value list = fun rt v ->
  match rt with
  | Arg tag -> [ inject tag v ]
  | Any -> [ v ]
  | Fake _ -> []
  | Pair (tl, tr) -> inject_result tl (fst v) @ inject_result tr (snd v)
  | Opt rt -> (match v with Some v -> inject_result rt v | None -> [])
  | Flag _ -> assert false
  | Seq _ -> assert false

let rec wrap_fun
  : type a f r. (a, f, r) funtag -> f -> (Ast.var option * value) list -> value list
  = fun funtag partial args ->
    match funtag with
    | Take (at, rt) ->
      let _, av, rest = get_arg at args in
      wrap_fun rt (partial av) rest
    | Return (at, rt) ->
      let _, av, rest = get_arg at args in
      if rest <> [] then error (Werror (P (Too_many_arguments (List.length rest)))) ;
      inject_result rt (partial av)

(** Registers a unary operator ; for immediate types, optionally
    extended to pointwise ([pw = true]) ; the first type is the
    parameter's type, used for conversion and dispatch ; the second is
    the return type, used for conversion *)
let register_unop ?(pw = false) lib op xt rt f =
  match xt, rt with
  | Arg (Single xt), Arg (Single rt) ->
    register_primitive lib
      (Unary op, [ Typed (T (Single xt)) ], 1)
      (fun lhs -> function
         | [ None, xv ] ->
           let x = extract (Single xt) xv in
           [ inject (Single rt) (f x) ]
         | _ -> raise Bad_type) ;
    if pw then
      register_primitive lib
        (Unary op, [ Typed (T (Matrix xt)) ], 1)
        (fun lhs -> function
           | [ None, xv ] ->
             let x = extract (Matrix xt) xv in
             let wx, hx = matrix_size x in
             let m = matrix_create rt wx hx in
             for i = 1 to wx do
               for j = 1 to hx do
                 matrix_set m i j (f (matrix_get x i j))
               done ;
             done ;
             [ inject (Matrix rt) m ]
           | _ -> raise Bad_type)
  | _ ->
    register_primitive lib
      (Unary op, [ arg_matcher xt ], 1)
      (fun lhs l ->
         let _, x, l = get_arg xt l in
         inject_result rt (f x))

(** Registers a binary operator ; for immediate types, optionally
    extended to pointwise ([pw = true]) and scalar-matrix ([scl / scr =
    true]) operators ; the two first types are the parameters' types,
    used for conversion and dispatch ; the last is the return type,
    used for conversion *)
let rec register_binop ?(pw = false) ?(scl = false) ?(scr = false) lib op xt yt rt f =
  match xt, yt, rt with
  | Arg (Single xt), Arg (Single yt), Arg (Single rt) ->
    register_primitive lib
      (Binary op, [ Typed (T (Single xt)) ; Typed (T (Single yt)) ], 1)
      (fun lhs -> function
         | [ None, xv ; None, yv ] ->
           let x = extract (Single xt) xv in
           let y = extract (Single yt) yv in
           [ inject (Single rt) (f x y) ]
         | _ -> raise Bad_type) ;
    if pw then register_pw_i_binop lib op xt yt rt f ;
    if scl then register_scl_i_binop lib op xt yt rt f ;
    if scr then register_scr_i_binop lib op xt yt rt f
  | _ ->
    assert (not (pw || scl || scr)) ;
    register_primitive lib
      (Binary op, [ arg_matcher xt ; arg_matcher yt ], 1)
      (fun lhs l ->
         let _, x, l = get_arg xt l in
         let _, y, l = get_arg yt l in
         inject_result rt (f x y))
and register_pw_i_binop lib op xt yt rt f =
  register_primitive lib
    (Binary op, [ Typed (T (Matrix xt)) ; Typed (T (Matrix yt)) ], 1)
    (fun lhs -> function
       | [ None, xv ; None, yv ] ->
         let x = extract (Matrix xt) xv in
         let wx, hx = matrix_size x in
         let y = extract (Matrix yt) yv in
         let wy, hy = matrix_size y in
         if wx <> wy || hx <> hy then
           error (Generic ("pointwise "
                           ^ Ast.string_of_op op
                           ^ " only works on matrices of the same size")) ;
         let m = matrix_create rt wx hx in
         for i = 1 to wx do
           for j = 1 to hx do
             matrix_set m i j (f (matrix_get x i j) (matrix_get y i j))
           done ;
         done ;
         [ inject (Matrix rt) m ]
       | _ -> raise Bad_type)

and register_scl_i_binop lib op xt yt rt f =
  register_primitive lib
    (Binary op, [ Typed (T (Single xt)) ; Typed (T (Matrix yt)) ], 1)
    (fun lhs -> function
       | [ None, xv ; None, yv ] ->
         let x = extract (Single xt) xv in
         let y = extract (Matrix yt) yv in
         let w, h = matrix_size y in
         let m = matrix_create rt w h in
         for i = 1 to w do
           for j = 1 to h do
             matrix_set m i j (f x (matrix_get y i j))
           done ;
         done ;
         [ inject (Matrix rt) m ]
       | _ -> raise Bad_type)
and register_scr_i_binop lib op xt yt rt f =
  register_primitive lib
    (Binary op, [ Typed (T (Matrix xt)) ; Typed (T (Single yt)) ], 1)
    (fun lhs -> function
       | [ None, xv ; None, yv ] ->
         let x = extract (Matrix xt) xv in
         let w, h = matrix_size x in
         let y = extract (Single yt) yv in
         let m = matrix_create rt w h in
         for i = 1 to w do
           for j = 1 to h do
             matrix_set m i j (f (matrix_get x i j) y)
           done ;
         done ;
         [ inject (Matrix rt) m ]
       | _ -> raise Bad_type)

(** Registers a range operator *)
let register_range lib xt st yt rt f =
  let cb =
    (fun lhs l ->
       let _, x, l = get_arg xt l in
       try
         let _, s, l = get_arg st l in
         let _, y, l = get_arg yt l in
         inject_result rt (f x ?step:(Some s) y)
       with _ ->
         let _, y, l = get_arg yt l in
         inject_result rt (f x ?step:None y)) in
  register_primitive lib
    (Colon, [ arg_matcher xt ; arg_matcher st ; arg_matcher yt ], 1) cb ;
  register_primitive lib ~more:true
    (Colon, [ arg_matcher xt ; arg_matcher yt ], 1) cb

(** Registers an extraction operator for standard matrices *)
let register_matrix_extraction lib xt =
  let extract_nil lhs = function
    | [ None, m ] -> [ m ]
    | _ -> raise Bad_index in
  let extract_one lhs = function
    | [ None, m ; None, i ] ->
      let i = Values.(extract (Single Int32) (cast i (T (Single Int32)))) in
      let m = Values.(extract (Matrix xt) (cast m (T (Matrix xt)))) in
      [ inject (Single xt) (matrix_get_linear m i) ]
    | _ -> raise Bad_index in
  let extract_two lhs = function
    | [ None, m ; None, i ; None, j ] ->
      let i = Values.(extract (Single Int32) (cast i (T (Single Int32)))) in
      let j = Values.(extract (Single Int32) (cast j (T (Single Int32)))) in
      let m = Values.(extract (Matrix xt) (cast m (T (Matrix xt)))) in
      [ inject (Single xt) (matrix_get m i j) ]
    | _ -> raise Bad_index in
  let indexes : matcher list = [
    Typed (T (Single (Number Real))) ;
    Typed (T (Single Int8)) ; Typed (T (Single Int16)) ; Typed (T (Single Int32)) ;
    Typed (T (Single Uint8)) ; Typed (T (Single Uint16)) ; Typed (T (Single Uint32)) ] in
  List.iter (fun overloading ->
      register_primitive lib ~more:false
        (overloading, [ Typed (T (Matrix xt)) ], 1)
        extract_nil ;
      register_primitive lib ~more:false
        (overloading, [ Typed (T (Single xt)) ], 1)
        extract_nil ;
      List.iter (fun ki ->
          register_primitive lib ~more:false
            (overloading, [ Typed (T (Matrix xt)) ; ki ], 1)
            extract_one ;
          register_primitive lib ~more:false
            (overloading, [ Typed (T (Single xt)) ; ki ], 1)
            extract_one ;
          List.iter (fun kj ->
              register_primitive lib ~force:true ~more:false
                (overloading, [ Typed (T (Matrix xt)) ; ki ; kj ], 1)
                extract_two ;
              register_primitive lib ~force:true ~more:false
                (overloading, [ Typed (T (Single xt)) ; ki ; kj ], 1)
                extract_two)
            indexes)
        indexes)
    [ Extraction ; Recursive_extraction]

(** Registers an extraction operator for standard matrices *)
let register_matrix_injection lib xt compatible =
  let inject_one lhs = function
    | [ None, m ; None, v ; None, i ] ->
      let m = Values.(extract (Matrix xt) (grab (cast m (T (Matrix xt))))) in
      let v = Values.(extract (Single xt) (cast v (T (Single xt)))) in
      let i = Values.(extract (Single Int32) (cast i (T (Single Int32)))) in
      matrix_set_linear m i v ;
      [ inject (Matrix xt) m ]
    | _ -> raise Bad_index in
  let inject_two lhs = function
    | [ None, m ; None, v ; None, i ; None, j ] ->
      let m = Values.(extract (Matrix xt) (grab (cast m (T (Matrix xt))))) in
      let v = Values.(extract (Single xt) (cast v (T (Single xt)))) in
      let i = Values.(extract (Single Int32) (cast i (T (Single Int32)))) in
      let j = Values.(extract (Single Int32) (cast j (T (Single Int32)))) in
      matrix_set m i j v ;
      [ inject (Matrix xt) m ]
    | _ -> raise Bad_index in
  let indexes : matcher list = [
    Typed (T (Single (Number Real))) ;
    Typed (T (Single Int8)) ; Typed (T (Single Int16)) ; Typed (T (Single Int32)) ;
    Typed (T (Single Uint8)) ; Typed (T (Single Uint16)) ; Typed (T (Single Uint32)) ] in
  List.iter (fun kv ->
      List.iter (fun ki ->
          register_primitive lib ~more:false
            (Injection, [ Typed (T (Matrix xt)) ; kv ; ki ], 1)
            inject_one ;
          register_primitive lib ~more:false
            (Injection, [ Typed (T (Single xt)) ; kv ; ki ], 1)
            inject_one ;
          List.iter (fun kj ->
              register_primitive lib ~force:true ~more:false
                (Injection, [ Typed (T (Matrix xt)) ; kv ; ki ; kj ], 1)
                inject_two ;
              register_primitive lib ~force:true ~more:false
                (Injection, [ Typed (T (Single xt)) ; kv ; ki ; kj ], 1)
                inject_two)
            indexes)
        indexes)
    compatible

(** Assuming that an homogeneous operator [t] for type [tr x tr -> a]
    exists, defines a new operator [t'] of type [t1 x t2 -> a] such as
    [t' (x, y) = t (cast (x, t1 => tr), cast (y, t2 => tr))] *)
let register_casted_homo_binop lib op (at1 : _ argtag) (at2 : _ argtag) (atr : _ argtag) =
  match at1, at2, atr with
  | Arg t1, Arg t2, Arg tr ->
    let f = Dispatcher.lookup (Binary op) [ T tr ; T tr ] 1 lib in
    register_primitive lib
      (Binary op, [ Typed (T t1) ; Typed (T t2) ], 1)
      (fun lhs l ->
         let _, x, l = get_arg at1 l in
         let _, y, l = get_arg at2 l in
         f.call lhs [ None, Values.cast (inject t1 x) (T tr) ;
                      None, Values.cast (inject t2 y) (T tr) ])
  | _ -> assert false

(** Register a primitive function and binds it to a name ; the type
    description is used to map the OCaml function type and select the
    right conversions ; also, the first type argument is used for
    dispatch *)
let register_function
  : type a b r. lib -> state -> string -> (a, a -> b, r) funtag -> (a -> b) -> unit
  = fun lib state name t f ->
    register_primitive lib ~name
      (Function name,
       (try [ first_arg_matcher t ] with Not_found -> []), (* drop compat to be better? *)
       return_arity t)
      (let cb = wrap_fun t f in fun lhs args -> cb args) ;
    let var = State.var state name in
    State.put state var (inject Primitive name)

(** Registers a predefined cast between two types under some name *)
let register_cast_function lib state name (T tag1 : rtt) (t2 : rtt) =
  register_primitive lib
    (Function name, [ Typed (T tag1) ], 1)
    (fun lhs -> function
       | [ None, v ] -> [ Values.cast v t2 ]
       | _ -> raise Bad_cast) ;
  let var = State.var state name in
  State.put state var (inject Primitive name)

(** Registers [,] and [;] operators for collating two matrices of a
    given element, also compatible with single elements of the type and
    empty matrices *)
let register_homo_collation lib itag =
  let mtag = Matrix itag in
  let register_with_casts kind collate tag1 tag2 =
    register_primitive lib
      (kind, [ tag1 ; tag2 ], 1)
      (fun lhs -> function
         | [ None, v1 ; None, v2 ] ->
           let v1 = Values.cast v1 (T mtag) in
           let v2 = Values.cast v2 (T mtag) in
           let x = extract mtag v1 in
           let y = extract mtag v2 in
           [ inject mtag (collate x y) ]
         | _ -> raise Bad_type)
  in
  List.iter (fun (kind, collate) ->
      List.iter (fun tag ->
          register_primitive lib (kind, [ Typed (T Atom) ; tag ], 1)
            (fun lhs -> function [ None, _ ; None, v ] -> [ v ] | _ -> raise Bad_type) ;
          register_primitive lib (kind, [ tag ; Typed (T Atom) ], 1)
            (fun lhs -> function [ None, v ; None, _ ] -> [ v ] | _ -> raise Bad_type))
        ([ Typed (T (Single itag)) ; Typed (T (Matrix itag)) ] : matcher list) ;
      List.iter (fun (tag1, tag2) ->
          register_with_casts kind collate tag1 tag2)
        ([ Typed (T (Single itag)), Typed (T (Single itag)) ;
           Typed (T (Matrix itag)), Typed (T (Matrix itag)) ;
           Typed (T (Single itag)), Typed (T (Matrix itag)) ;
           Typed (T (Matrix itag)), Typed (T (Single itag)) ] : (matcher * matcher) list))
    [ Matrix_horizontal_collation,
      (fun m1 m2 -> try
          Values.matrix_collate_horizontally m1 m2
        with Values.Bad_index ->
          error (Generic "inconsistent heights for horizontal matrix collation")) ;
      Matrix_vertical_collation,
      (fun m1 m2 -> try
          Values.matrix_collate_vertically m1 m2
        with Values.Bad_index ->
          error (Generic "inconsistent widths for vertical matrix collation")) ]

(** Scilab's standard library *)
let stdlib state lib =
  (*----- variable operations ---------------------------------------------*)
  register_function lib state "global"
    (seq string @-> null)
    (List.iter (fun n -> State.(global state (var state n)))) ;
  register_function lib state "clear"
    (seq string @-> null)
    (List.iter (fun n -> State.(clear state (var state n)))) ;
  State.put state (State.var state "argn") (inject Primitive "argn") ;
  State.put state (State.var state "stacksize") (inject Primitive "stacksize") ;
  register_function lib state "type"
    (any @-> real)
    (fun v ->
       let rec typenum : type t. t Values.tag -> float = function
         | Null -> 0.
         | Atom -> 0.
         | Single t -> itypenum t
         | Matrix t -> itypenum t
         | Sparse Bool -> 6.
         | Sparse _ -> 5.
         | Macro -> 13.
         | Vlist -> 15.
         | Tlist _ -> 16.
         | Mlist _ -> 17.
         | Primitive -> 130.
         | Handle -> 9.
         | Eye t -> itypenum t
       and itypenum : type t. t Values.itag -> float = function
         | Number _ -> 1.
         | Poly _ -> 2.
         | Bool -> 4.
         | Int8 -> 8.
         | Int16 -> 8.
         | Int32 -> 8.
         | Uint8 -> 8.
         | Uint16 -> 8.
         | Uint32 -> 8.
         | String -> 10. in
       let T t = Values.typeof v in
       typenum t) ;
  register_function lib state "typeof"
    (any @-> string)
    (fun v ->
       let rec typename : type t. t Values.tag -> string = function
         | Null -> raise Bad_type
         | Atom -> raise Bad_type
         | Single t -> itypename t
         | Matrix t -> itypename t
         | Sparse Bool -> "boolean sparse"
         | Sparse _ -> "sparse"
         | Macro -> "function"
         | Vlist -> "list"
         | Tlist n -> n
         | Mlist n -> n
         | Primitive -> "fptr"
         | Handle -> "handle"
         | Eye t -> itypename t
       and itypename : type t. t Values.itag -> string = function
         | Number _ -> "constant"
         | Poly _ -> "polynomial"
         | Bool -> "boolean"
         | Int8 -> "int8"
         | Int16 -> "int16"
         | Int32 -> "int32"
         | Uint8 -> "uint8"
         | Uint16 -> "uint16"
         | Uint32 -> "uint32"
         | String -> "string" in
       let T t = Values.typeof v in
       typename t) ;
  register_function lib state "inttype"
    (any @-> real)
    (fun v ->
       let rec typenum : type t. t Values.tag -> float = function
         | Single t -> itypenum t
         | Matrix t -> itypenum t
         | Eye t -> itypenum t
         | _ -> raise Bad_type
       and itypenum : type t. t Values.itag -> float = function
         | Int8 -> 1.
         | Int16 -> 2.
         | Int32 -> 4.
         | Uint8 -> 11.
         | Uint16 -> 12.
         | Uint32 -> 14.
         | _ -> raise Bad_type in
       let T t = Values.typeof v in
       typenum t) ;
  register_function lib state "error"
    (string @-> real)
    (fun msg -> raise (Interp_error (Generic msg))) ;
  (*----- lists ----------------------------------------------------------*)
  register_function lib state "list" (seq any @-> vlist) (Values.vlist_create) ;
  let list_extract lhs = function
    | [ None, l ; None, i ] ->
      let l = Values.(extract Vlist l) in
      let i = Values.(extract (Single Int32) (cast i (T (Single Int32)))) in
      [ Values.vlist_get l i ]
    | _ -> raise Bad_type in
  let list_inject lhs = function
    | [ None, l ; None, v ; None, i ] ->
      let l = Values.(extract Vlist (grab l)) in
      let i = Values.(extract (Single Int32) (cast i (T (Single Int32)))) in
      Values.vlist_set l i v ; [ Values.inject Vlist l ]
    | _ -> raise Bad_type in
  List.iter (fun (T ti) ->
      register_primitive lib
        (Injection, [ Typed (T Vlist) ; Typed (T ti) ; Any ], 1)
        list_inject ;
      register_primitive lib
        (Recursive_extraction, [ Typed (T Vlist) ; Typed (T ti) ], 1)
        list_extract ;
      register_primitive lib
        (Extraction, [ Typed (T Vlist) ; Typed (T ti) ], 1)
        list_extract)
    [ T (Single (Number Real)) ;
      T (Single Int8) ; T (Single Int16) ; T (Single Int32) ;
      T (Single Uint8) ; T (Single Uint16) ; T (Single Uint32) ] ;
  register_function lib state "tlist"
    (matrix string @* seq any @-> any)
    (fun t contents ->
       let name, fields = match matrix_size t with
         | (1, h) ->
           let rec collect i =
             if i > h then []
             else matrix_get t 1 i :: collect (i + 1) in
           matrix_get t 1 1, collect 2
         | (w, 1) ->
           let rec collect i =
             if i > w then []
             else matrix_get t i 1 :: collect (i + 1) in
           matrix_get t 1 1, collect 2
         | _ -> raise Bad_type
       in inject (Tlist name) (Values.tlist_create name fields contents)) ;
  register_function lib state "tlist"
    (string @* seq any @-> any)
    (fun name contents -> inject (Tlist name) (Values.tlist_create name [] contents)) ;
  register_function lib state "mlist"
    (matrix string @* seq any @-> any)
    (fun t contents ->
       let name, fields = match matrix_size t with
         | (1, h) ->
           let rec collect i =
             if i > h then []
             else matrix_get t 1 i :: collect (i + 1) in
           matrix_get t 1 1, collect 2
         | (w, 1) ->
           let rec collect i =
             if i > w then []
             else matrix_get t i 1 :: collect (i + 1) in
           matrix_get t 1 1, collect 2
         | _ -> raise Bad_type
       in inject (Mlist name) (Values.mlist_create name fields contents)) ;
  register_function lib state "mlist"
    (string @* seq any @-> any)
    (fun name contents -> inject (Mlist name) (Values.mlist_create name [] contents)) ;
  (*----- misc operations -------------------------------------------------*)
  register_function lib state "quit" (void @-> null)
    (fun () -> raise Exit) ;
  let disp l =
    List.iter
      (fun v -> Printf.printf "%s\n%!" (string_of_value v))
      (List.rev l) in
  register_function lib state "disp" (seq any @-> null) disp ;
  (*----- string operations -----------------------------------------------*)
  register_binop lib Ast.Plus ~pw:true ~scl:true ~scr:true string string string ( ^ ) ;
  register_function lib state "string" (any @-> string) string_of_value ;
  register_binop lib Ast.Eq string string bool (fun x y -> x = y) ;
  (*----- int arithmetics -------------------------------------------------*)
  let rec intpow a p = match p with
    | 0 -> 1
    | 1 -> a
    | n ->
      let b = intpow a (n / 2) in
      b * b * (if n mod 2 = 0 then 1 else a) in
  List.iter (fun itag ->
      let tag = Arg (Single itag) in
      let bound = Values.icast Int32 itag in
      register_unop lib Ast.Unary_plus tag tag (fun x -> bound (~+ x)) ;
      register_unop lib Ast.Unary_minus tag tag (fun x -> bound (~- x)) ;
      register_binop lib Ast.Eq tag tag bool (fun x y -> x = y) ;
      register_binop lib Ast.Plus tag tag tag (fun x y -> bound (x + y)) ;
      register_binop lib Ast.Minus tag tag tag (fun x y -> bound (x - y)) ;
      register_binop lib Ast.Times tag tag tag (fun x y -> bound (x * y)) ;
      register_binop lib Ast.Power tag tag tag (fun x y -> bound (intpow x y)) ;
      register_binop lib Ast.Rdivide tag tag tag (fun x y -> bound (x / y)) ;
      register_binop lib Ast.Ldivide tag tag tag (fun x y -> bound (y / x)))
    [ Int8 ; Int16 ; Int32 ; Uint8 ; Uint16 ; Uint32 ] ;
  (*----- real arithmetics ------------------------------------------------*)
  register_unop ~pw:true lib Ast.Unary_plus real real ( ~+. ) ;
  register_unop ~pw:true lib Ast.Unary_minus real real ( ~-. ) ;
  register_binop ~pw:true ~scl:true ~scr:true lib Ast.Eq real real bool ( = ) ;
  register_binop ~pw:true ~scl:true ~scr:true lib Ast.Plus real real real ( +. ) ;
  register_binop ~pw:true ~scl:true ~scr:true lib Ast.Minus real real real ( -. ) ;
  register_binop ~scl:true ~scr:true lib Ast.Times real real real ( *. ) ;
  register_binop ~scr:true lib Ast.Rdivide real real real ( /. ) ;
  register_binop ~scl:true lib Ast.Ldivide real real real (fun x y -> y /. x) ;
  register_binop ~scl:true lib Ast.Power real real real ( ** ) ;
  (*----- int / real casts ------------------------------------------------*)
  List.iter (fun t1 ->
      List.iter (fun (n, t2) ->
          register_cast_function lib state n t1 (T (Single t2)))
        [ "int8", Int8 ; "int16", Int16 ; "int32", Int32 ;
          "uint8", Uint8 ; "uint16", Uint16 ; "uint32", Uint32 ])
    [ T (Single (Number Real)) ;
      T (Single Int8) ; T (Single Int16) ; T (Single Int32) ;
      T (Single Uint8) ; T (Single Uint16) ; T (Single Uint32) ] ;
  (*----- casted int / real arithmetics for +-/* --------------------------*)
  List.iter (fun op ->
      List.iter (fun tx ->
          List.iter (fun ty ->
              if tx <> ty then
                let tr = max tx ty in
                register_casted_homo_binop lib op tx ty tr)
            [ int8 ; int16 ; int32 ; uint8 ; uint16 ; uint32 ])
        [ int8 ; int16 ; int32 ; uint8 ; uint16 ; uint32 ] ;
      List.iter (fun tr ->
          register_casted_homo_binop lib op real tr tr ;
          register_casted_homo_binop lib op tr real tr)
        [ int8 ; int16 ; int32 ; uint8 ; uint16 ; uint32 ])
    Ast.[Plus ; Minus ; Times ; Rdivide ; Ldivide ] ;
  (*----- casted int / real equality --------------------------------------*)
  List.iter (fun tx ->
      List.iter (fun ty ->
          if tx <> ty then
            let tr = max tx ty in
            register_casted_homo_binop lib Ast.Eq tx ty tr)
        [ int8 ; int16 ; int32 ; uint8 ; uint16 ; uint32 ])
    [ int8 ; int16 ; int32 ; uint8 ; uint16 ; uint32 ] ;
  List.iter (fun tr ->
      register_casted_homo_binop lib Ast.Eq real tr real ;
      register_casted_homo_binop lib Ast.Eq tr real real)
    [ int8 ; int16 ; int32 ; uint8 ; uint16 ; uint32 ] ;
  (*----- homogeneous matrix collation ------------------------------------*)
  register_primitive lib (Matrix_horizontal_collation, [ Typed (T Atom) ; Typed (T Atom) ], 1)
    (fun lhs -> function [ None, v ; None, _ ] -> [ v ] | _ -> raise Bad_type) ;
  register_primitive lib (Matrix_vertical_collation, [ Typed (T Atom) ; Typed (T Atom) ], 1)
    (fun lhs -> function [ None, v ; None, _ ] -> [ v ] | _ -> raise Bad_type) ;
  register_homo_collation lib (Number Real) ;
  register_homo_collation lib String ;
  register_homo_collation lib Int8 ;
  register_homo_collation lib Int16 ;
  register_homo_collation lib Int32 ;
  register_homo_collation lib Uint8 ;
  register_homo_collation lib Uint16 ;
  register_homo_collation lib Uint32 ;
  (*----- matrix size -----------------------------------------------------*)
  let size_matrix w h =
    let res = matrix_create (Number Real) 2 1 in
    matrix_set res 1 1 w ;
    matrix_set res 2 1 h ;
    res in
  register_function lib state "size" (eye real @* opt any @-> matrix real)
    (fun _ _ -> size_matrix (-. 1.) (-. 1.)) ;
  register_function lib state "size" (eye real @* opt any @-> real @+ real)
    (fun _ _ -> (-. 1., -. 1.)) ;
  register_function lib state "size" (atom @* opt any @-> matrix real)
    (fun () _ -> size_matrix 0. 0.) ;
  register_function lib state "size" (atom @* opt any @-> real @+ real)
    (fun () _ -> (0., 0.)) ;
  List.iter
    (function
      | T (Matrix itag) ->
        register_function lib state "size"
          (Arg (Single itag) @* opt any @-> matrix real)
          (fun _ _ -> size_matrix 1. 1.) ;
        register_function lib state "size"
          (Arg (Single itag) @* opt any @-> real @+ real)
          (fun _ _ -> (1., 1.)) ;
        register_function lib state "size"
          (Arg (Matrix itag) @* opt any @-> matrix real)
          (fun mat flag ->
             let w, h = matrix_size mat in
             size_matrix (float w) (float h)) ;
        register_function lib state "size"
          (Arg (Matrix itag) @* opt any @-> real @+ real)
          (fun mat flag ->
             let w, h = matrix_size mat in
             float h, float w)
      | _ -> assert false)
    [ T (Matrix (Number Real)) ; T (Matrix (Number Complex)) ;
      T (Matrix (Poly Real)) ; T (Matrix (Poly Complex)) ;
      T (Matrix String) ; T (Matrix Bool) ;
      T (Matrix Int8) ; T (Matrix Int16) ; T (Matrix Int32) ;
      T (Matrix Uint8) ; T (Matrix Uint16) ; T (Matrix Uint32) ] ;
  register_function lib state "size"
    (vlist @-> real)
    (fun l -> float (vlist_length l)) ;
  (*----- matrix extraction -----------------------------------------------*)
  register_matrix_extraction lib (Number Real) ;
  register_matrix_extraction lib Int8 ;
  register_matrix_extraction lib Int16 ;
  register_matrix_extraction lib Int32 ;
  register_matrix_extraction lib Uint8 ;
  register_matrix_extraction lib Uint16 ;
  register_matrix_extraction lib Uint32 ;
  register_matrix_extraction lib String ;
  (*----- matrix injection -----------------------------------------------*)
  let numbers : matcher list =
    [ Typed (T (Single (Number Real))) ;
      Typed (T (Single Int8)) ; Typed (T (Single Int16)) ; Typed (T (Single Int32)) ;
      Typed (T (Single Uint8)) ; Typed (T (Single Uint16)) ; Typed (T (Single Uint32)) ] in
  register_matrix_injection lib (Number Real) numbers ;
  register_matrix_injection lib Int8 numbers ;
  register_matrix_injection lib Int16 numbers ;
  register_matrix_injection lib Int32 numbers ;
  register_matrix_injection lib Uint8 numbers ;
  register_matrix_injection lib Uint16 numbers ;
  register_matrix_injection lib Uint32 numbers ;
  register_matrix_injection lib String [ Typed (T (Single String)) ] ;
  (*----- eye -------------------------------------------------------------*)
  register_function lib state "eye" (void @-> eye real) (fun () -> 1.) ;
  register_binop lib Ast.Plus (eye real) (eye real) (eye real) ( +. ) ;
  register_binop lib Ast.Minus (eye real) (eye real) (eye real) ( -. ) ;
  register_binop lib Ast.Times (eye real) real (eye real) ( *. ) ;
  register_binop lib Ast.Times real (eye real) (eye real) ( *. ) ;
  register_binop lib Ast.Rdivide (eye real) real (eye real) ( /. ) ;
  register_binop lib Ast.Rdivide real (eye real) real ( /. ) ;
  register_binop lib Ast.Ldivide real (eye real) (eye real) ( /. ) ;
  register_binop lib Ast.Ldivide (eye real) real real ( /. ) ;
  List.iter (fun itag ->
      let tag = Arg (Single itag) in
      let bound = Values.icast Int32 itag in
      register_binop lib Ast.Plus (eye real) (eye tag) (eye tag)
        (fun f i -> int_of_float f + i |> bound) ;
      register_binop lib Ast.Plus (eye tag) (eye real) (eye tag)
        (fun i f -> i + int_of_float f |> bound) ;
      register_binop lib Ast.Minus (eye real) (eye tag) (eye tag)
        (fun f i -> int_of_float f - i |> bound) ;
      register_binop lib Ast.Minus (eye tag) (eye real) (eye tag)
        (fun i f -> i - int_of_float f |> bound) ;
      register_binop lib Ast.Times (eye real) tag (eye tag)
        (fun f i -> int_of_float f * i |> bound) ;
      register_binop lib Ast.Times real (eye tag) (eye tag)
        (fun f i -> int_of_float f * i |> bound) ;
      register_binop lib Ast.Times (eye tag) real (eye tag)
        (fun i f -> i * int_of_float f |> bound) ;
      register_binop lib Ast.Times tag (eye real) (eye tag)
        (fun i f -> i * int_of_float f |> bound) ;
      List.iter (fun itag' ->
          let tag' = Arg (Single itag') in
          let ritag = max itag itag' in
          let rtag = Arg (Single ritag) in
          let bound = Values.icast Int32 ritag in
          register_binop lib Ast.Plus (eye tag) (eye tag') (eye rtag)
            (fun x y -> x + y |> bound) ;
          register_binop lib Ast.Minus (eye tag) (eye tag') (eye rtag)
            (fun x y -> x - y |> bound) ;
          register_binop lib Ast.Times (eye tag) tag' (eye rtag)
            (fun x y -> x * y |> bound) ;
          register_binop lib Ast.Times tag (eye tag') (eye rtag)
            (fun x y -> x * y |> bound))
        [ Int8 ; Int16 ; Int32 ; Uint8 ; Uint16 ; Uint32 ])
    [ Int8 ; Int16 ; Int32 ; Uint8 ; Uint16 ; Uint32 ] ;
  (*----- range operations ------------------------------------------------*)
  List.iter
    (fun k ->
       register_range lib
         (Arg (Single k)) (Arg (Single k)) (Arg (Single k))
         (matrix (Arg (Single k)))
         (fun s ?(step = 1) e ->
            let size = max 0 ((e - s) / step + 1) in
            let m = matrix_create k size 1 in
            for i = 0 to size - 1 do matrix_set m (i + 1) 1 (s + i * step) done ; m))
    [ Int8 ; Int16 ; Int32 ; Uint8 ; Uint16 ; Uint32 ] ;
  register_range lib real real real (matrix real)
    (fun s ?(step = 1.) e ->
       let size = max 0 (int_of_float (floor ((e -. s) /. step +. 1.))) in
       let m = matrix_create (Number Real) size 1 in
       for i = 0 to size - 1 do matrix_set m (i + 1) 1 (s +. float i *. step) done ; m) ;
  register_range lib string real string string (* Scilab may return [] *)
    (fun s ?(step = 0.) e ->
       if String.length s <> 1 || String.length e <> 1 then
         error (Generic "two single ASCII characters expected") ;
       let s = Char.code (String.get s 0) |> float in
       let e = Char.code (String.get e 0) |> float in
       let size = max 0 (int_of_float (floor ((e -. s) /. step +. 1.))) in
       let m = Bytes.create size in
       for i = 0 to size - 1 do
         Bytes.set m i (Char.chr (int_of_float (s +. float i *. step)))
       done ;
       Bytes.unsafe_to_string m) ;
  (*----- trigonometry ----------------------------------------------------*)
  register_function lib state "sin" (real @-> real) sin ;
  register_function lib state "cos" (real @-> real) cos ;
  register_function lib state "tan" (real @-> real) tan ;
  (*----- polynomials -----------------------------------------------------*)
  register_function lib state "poly" (matrix real @* string @* seq string @-> poly real)
    (fun items name -> function
       | [ "coeff" ] ->
         let res = poly_create Real name in
         let w, h = matrix_size items in
         for i = 1 to h * w  do
           poly_set res i (matrix_get_linear items i)
         done ; res
       | _ -> raise Bad_type);
  (*----- constants -------------------------------------------------------*)
  register_function lib state "zeros" (real @* real @-> matrix real)
    (fun w h -> matrix_create (Number Real) (int_of_float w) (int_of_float h)) ;
  register_function lib state "null" (void @-> null) (fun () -> ()) ;
  State.put state (State.var state ":") (inject (Eye (Number Real)) 1.) ;
  let dol = let edol = poly_create Real "$" in poly_set edol 2 1. ; edol in
  State.put state (State.var state "$") (inject (Single (Poly Real)) dol) ;
  State.put state (State.var state "%i") (inject (Single (Number Complex)) (0., 1.)) ;
  let pi = 3.141592653589793115997963468544185161590576171875_00 in
  State.put state (State.var state "%pi") (inject (Single (Number Real)) pi) ;
  let e = 2.718281828459045090795598298427648842334747314453125_00 in
  State.put state (State.var state "%e") (inject (Single (Number Real)) e)

(* register all the predefined primitives *)
let () =
  register_library stdlib
