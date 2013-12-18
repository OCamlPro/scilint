(*  OCamlPro Scilab Toolbox - Resilient parser for Scilab 5's syntax
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

(** This files implements a parser for a syntax as close as possible
    to Scilab 5's. It is resilient, in order to serve both as a
    classic frontend or as a real time syntax checker for helping
    Scilab developpers. To mimic the behaviour of Scilab 5's big
    FORTRAN blob, and to produce neat messages, it is completely
    handwritten as a recursive descent.

    The implementation is decomposed in 4 layers :

    1. The StreamReader module defines a simple character stream
       interface to read the raw code efficiently, allowing
       backtracking and checkpointing.

    2. The ScilabStreamReader decorates the previous module by
       handling code locations and taking care of Scilab's line
       continuators and comments.

    3. The ScilabTokenReader module implements a regexp-like pattern
       matching engine on top of ScilabStreamReader.

    4 Finally, the parser code is written as a set of contextual,
      mutually recursive OCaml functions of which the two main ones
      are {!parse_file} and {!parse_string}.

    Thank you for reading ! *)

(** A simple module to consume a stream character per character. Hides
    an internal buffer for performance and the ability to put
    characters back in the stream and modify it in place. *)
module StreamReader : sig
  type state
  type position

  val generic_reader : (unit -> string) -> state
  val string_reader : string -> state
  val channel_reader : in_channel -> state
  val read : state -> char
  val write : state -> char -> unit
  val peek : state -> char
  val poke : state -> char -> unit
  val skip : state -> int -> unit
  val discard : state -> unit
  val rewind : state -> int -> unit
  val pos : state -> position
  val go_to : state -> position -> unit
  val extract_between : state -> position -> position -> string
  val extract_from : state -> position -> string
end = struct
  type state = {
    mutable buffer : string ;
    mutable position : int ;
    mutable limit : int ;
    raw_read : unit -> string ;
  }
  and position = int

  let generic_reader raw_read =
    { buffer = "" ; limit = 0 ; position = 0 ; raw_read }

  let string_reader str =
    let already_read = ref false in
    let raw_read () =
      if not !already_read then (already_read := true ; str) else ""
    in generic_reader raw_read

  let channel_reader chan =
    let buf = String.make 10_000 '\000' in
    let raw_read () =
      let len = Pervasives.input chan buf 0 10_000 in
      String.sub buf 0 len
    in generic_reader raw_read

  let fill state bytes =
    let buffer_chunk_size = 10_000 in
    let len = String.length state.buffer in
    let bytes_len = String.length bytes in
    let new_len = state.limit + bytes_len in
    if new_len >= len then begin
      let new_len = new_len + buffer_chunk_size in
      let new_buffer = String.make new_len '\000' in
      String.blit state.buffer 0 new_buffer 0 state.limit ;
      state.buffer <- new_buffer
    end ;
    String.blit bytes 0 state.buffer state.limit bytes_len ;
    state.limit <- state.limit + bytes_len

  let rec read state =
    if state.position < state.limit then
      let res = String.unsafe_get state.buffer state.position in
      state.position <- state.position + 1 ;
      res
    else
      let new_bytes = state.raw_read () in
      if String.length new_bytes = 0 then '\000'
      else (fill state new_bytes ; read state)

  let rec write state char =
    if state.position < state.limit then begin
      String.unsafe_set state.buffer state.position char ;
      state.position <- state.position + 1
    end else
      let new_bytes = state.raw_read () in
      fill state (if String.length new_bytes = 0 then "_" else new_bytes) ;
      write state char

  let rec peek state =
    if state.position < state.limit then
      String.unsafe_get state.buffer state.position
    else
      let new_bytes = state.raw_read () in
      if String.length new_bytes = 0 then '\000'
      else (fill state new_bytes ; peek state)

  let rec poke state char =
    if state.position < state.limit then
      String.unsafe_set state.buffer state.position char
    else
      let new_bytes = state.raw_read () in
      if String.length new_bytes = 0 then invalid_arg "poke"
      else (fill state new_bytes ; poke state char)

  let discard state =
    if state.position < state.limit then
      state.position <- state.position + 1
    else
      let new_bytes = state.raw_read () in
      if String.length new_bytes <> 0 then
        fill state new_bytes

  let rewind state n =
    if state.position - n < 0 then
      invalid_arg "ScilabFiveParser.StreamReader.rewind" ;
    state.position <- state.position - n

  let skip state n =
    if state.position + n >= state.limit then
      invalid_arg "ScilabFiveParser.StreamReader.skip" ;
    state.position <- state.position + n

  let pos state =
    state.position

  let go_to state pos =
    if pos < 0 || pos > state.limit then
      invalid_arg "ScilabFiveParser.StreamReader.go_to" ;
    state.position <- pos

  let extract_from state pos =
    String.sub state.buffer pos (state.position - pos)

  let extract_between state start_pos end_pos =
    String.sub state.buffer start_pos (end_pos - start_pos + 1)
end

(** A character per character reader with Scilab specific hacks.  It
    maintains the current code location, takes care of line feed
    inconsistencies (only returns '\n') and hides line continuators
    ("..[. ]*(\n|\r|\r\n)"). *)
module ScilabStreamReader : sig
  type state
  type point = int * int
  type checkpoint

  val generic_reader : (unit -> string) -> state
  val string_reader : string -> state
  val channel_reader : in_channel -> state
  val point : state -> point
  val here : state -> point * point
  val checkpoint : state -> checkpoint
  val checkpoint_point : checkpoint -> point
  val restore : state -> checkpoint -> unit
  val read : state -> char
  val advance : state -> int -> unit
  val advance_1 : state -> unit
  val advance_when : bool -> state -> int -> bool
  val advance_1_when : bool -> state -> bool
  val peek : state -> char
  val peek_ahead : state -> int -> char
  val extract_from : state -> checkpoint -> string * (point * point)
  val from : state -> checkpoint -> (point * point)
  val eat_comment : state -> unit
end = struct
  type state = {
    reader_state : StreamReader.state ;
    mutable point : point ;
  }
  and point = int * int
  and checkpoint = StreamReader.position * point

  let generic_reader row_reader =
    { reader_state = StreamReader.generic_reader row_reader ;
      point = (1, 0) }

  let string_reader str =
    { reader_state = StreamReader.string_reader str ;
      point = (1, 0) }

  let channel_reader chan =
    { reader_state = StreamReader.channel_reader chan ;
      point = (1, 0) }

  let checkpoint state =
    StreamReader.pos state.reader_state, state.point

  let point state =
    state.point

  let here state =
    state.point, state.point

  let checkpoint_point = snd

  let restore state (pos, loc) =
    StreamReader.go_to state.reader_state pos ;
    state.point <- loc

  let line_feed state =
    let (line, column) = state.point in
    state.point <- (line + 1, 0)

  let column_feed state =
    let (line, column) = state.point in
    state.point <- (line, column + 1)

  let eat_lf state =
    (* See {!read} for explanation. *)
    match StreamReader.peek state.reader_state with
    | '\n' -> StreamReader.write state.reader_state '\001'
    | _ -> ()

  let rec eol_dots state =
    (* See {!read} for explanation. *)
    match StreamReader.read state.reader_state with
    | '.' | ' ' -> eol_dots state
    | '\n' -> line_feed state ; true
    | '\r' -> line_feed state ; eat_lf state ; true
    | _ -> false

  let rec read state =
    (* To be able to extract text and to speed up, we scan for line
       continuations only once. After that, we change the stream,
       putting '\001' for a skippable character, '\002' for a
       skippable line break and '\003' for meaningful dots that have
       already been scanned as such. *)
    match StreamReader.read state.reader_state with
    | '.' -> 
      StreamReader.rewind state.reader_state 1 ;
      let spos = StreamReader.pos state.reader_state in
      if eol_dots state then begin
        let epos = StreamReader.pos state.reader_state in
        StreamReader.go_to state.reader_state spos ;
        StreamReader.write state.reader_state '\002' ;
        while StreamReader.pos state.reader_state < epos do
          StreamReader.write state.reader_state '\001'
        done ;
        line_feed state ; read state
      end else begin
        StreamReader.go_to state.reader_state spos ;
        while StreamReader.peek state.reader_state = '.' do
          StreamReader.write state.reader_state '\003'
        done ;
        StreamReader.go_to state.reader_state spos ;
        StreamReader.discard state.reader_state ;
        column_feed state ; '.'
      end
    | '\r' ->
      StreamReader.rewind state.reader_state 1 ;
      StreamReader.write state.reader_state '\n' ;
      eat_lf state ; line_feed state ; '\n'
    | '\n' as res -> line_feed state ; res
    | '\000' -> '\000'
    | '\001' -> read state
    | '\002' -> line_feed state ; read state
    | '\003' -> column_feed state ; '.'
    | res -> column_feed state ; res

  let advance state n =
    for i = 1 to n do ignore (read state) done

  let advance_1 state =
    ignore (read state)

  let advance_when cond state n =
    if cond then for i = 1 to n do ignore (read state) done ; cond

  let advance_1_when cond state =
    if cond then ignore (read state) ; cond

  let peek state =
    let cp = checkpoint state in
    let res = read state in
    restore state cp ; res

  let peek_ahead state n =
    if n < 0 then
      invalid_arg "ScilabFiveParser.ScilabStreamReader.peek_ahead" ;
    let cp = checkpoint state in
    for i = 0 to n - 1 do ignore (read state) done ;
    let res = peek state in
    restore state cp ;
    res

  let correct s =
    (* See {!read} and {!eol_dots} for explanation. *)
    let len = String.length s in
    let i = ref 0 and j = ref 0 in
    while !i < len do
      match s.[!i] with
      | '\000' | '\001' | '\002' -> incr i
      | '\003' -> s.[!j] <- '.' ; incr i ; incr j
      | c -> s.[!j] <- c ; incr i ; incr j
    done ;
    String.sub s 0 !j

  let extract_from state (pos, loc) =
    let s = StreamReader.extract_from state.reader_state pos in
    let s = correct s in
    (s, (loc, point state))

  let from state (pos, loc) =
    (loc, point state)

  let eat_comment state =
    (* Ugly hack, because "..( .)*\n" are not taken into account after
       a "//". This function has no other use case than eating
       comments. It positions the state at line's end and declares all
       dots as meaningful ones by changing them into '\003'. See {!read}.  *)
    let rec eat () =
      match StreamReader.read state.reader_state with
      | '\r' | '\n' -> StreamReader.rewind state.reader_state 1
      | '.' ->
        StreamReader.rewind state.reader_state 1 ;
        StreamReader.write state.reader_state '\003' ;
        column_feed state ; eat ()
      | '\000' -> ()
      | _ -> column_feed state ; eat ()
    in eat ()
end

(** A quick and dirty regexp-like matching module based on
    {!ScilabStreamReader}, with operators and predefined Scilab
    patterns constructs. *)
module ScilabTokenReader : sig
  open ScilabStreamReader
  type regexp
  type group
  exception Unmatched_group
  val exec : regexp -> state -> bool
  val discard : regexp -> state -> unit
  val plus : regexp -> regexp
  val star : regexp -> regexp
  val maybe : regexp -> regexp
  val phantom : regexp -> regexp
  val alts : regexp list -> regexp
  val seq : regexp list -> regexp
  val string : string -> regexp
  val char : char -> regexp
  val any : regexp
  val but_char : char -> regexp
  val any_of : string -> regexp
  val any_but : string -> regexp
  val comment : regexp
  val (|||) : regexp -> regexp -> regexp
  val (--) : char -> char -> string
  val callback : (state -> bool) -> regexp
  val save : regexp -> state -> string * (point * point)
  val discard : regexp -> state -> unit
  val group : unit -> group
  val store : group -> regexp -> regexp
  val extract : group -> string * (point * point)
  val before : group -> point
  val after : group -> point
  val around : group -> point * point
  val text : group -> string
end = struct
  open ScilabStreamReader

  type regexp = state -> bool

  let exec f x = f x

  let rec eat f state =
    while f state do () done

  let discard = eat

  let plus f state =
    if f state then (eat f state ; true) else false

  let star f state =
    eat f state ; true

  let maybe f state =
    ignore (f state) ; true

  let (|||) f1 f2 =
    fun state -> f1 state || f2 state

  let phantom f state =
    let cp = checkpoint state in
    let res = f state in
    restore state cp ; res

  let alts fs state =
    let rec loop fs =
      match fs with
      | [] -> false
      | f :: fs -> f state || loop fs
    in loop fs

  let seq fs state =
    let cp = checkpoint state in
    let rec loop fs =
      match fs with
      | [] -> true
      | f :: fs ->
        if f state then
          loop fs
        else
          (restore state cp ; false)
    in loop fs

  let string s state =
    if peek state = s.[0] then
      let cp = checkpoint state in
      try
        for i = 0 to String.length s - 1 do
          if read state <> s.[i] then raise Exit
        done ; true
      with Exit -> restore state cp ; false
    else false

  let comment state =
    if string "//" state then (eat_comment state ; true) else false

  let string_alts ss =
    alts (List.map string ss)

  let char char state =
    if peek state = char then (advance_1 state ; true) else false

  let but_char char state = 
    match peek state with
    | c when char = c -> false
    | '\000' -> false
    | _ -> advance_1 state ; true

  let any state =
    read state <> '\000'

  let any_of chars =
    let table = Array.make 256 false in
    String.iter (fun c -> table.(Char.code c) <- true) chars ;
    (fun state ->
       advance_1_when
         (Array.unsafe_get table (Char.code (peek state)))
         state)

  let global_any_of_cache = Hashtbl.create 100

  let any_of chars =
    match String.length chars with
    | 1 ->
      let c0 = chars.[0] in
      (fun state ->
         let c = peek state in
         advance_1_when (c = c0) state)
    | 2 ->
      let c0 = chars.[0] and c1 = chars.[1] in
      (fun state ->
         let c = peek state in
         advance_1_when (c = c0 || c = c1) state)
    | 3 ->
      let c0 = chars.[0] and c1 = chars.[1] and c2 = chars.[2] in
      (fun state ->
         let c = peek state in
         advance_1_when (c = c0 || c = c1 || c = c2) state)
    | 4 ->
      let c0 = chars.[0] and c1 = chars.[1]
      and c2 = chars.[2] and c3 = chars.[3] in
      (fun state ->
         let c = peek state in
         advance_1_when (c = c0 || c = c1 || c = c2 || c = c3) state)
    | _ ->
      try
        Hashtbl.find global_any_of_cache chars
      with Not_found ->
        let f = any_of chars in
        Hashtbl.replace global_any_of_cache chars f ;
        f

  let any_but chars =
    let table = Array.make 256 true in
    String.iter (fun c -> table.(Char.code c) <- false) chars ;
    let recognizer state =
      let cp = checkpoint state in
      if table.(Char.code (read state)) then true
      else (restore state cp ; false)
    in
    recognizer

  let global_any_but_cache = Hashtbl.create 100

  let any_but chars =
    try
      Hashtbl.find global_any_but_cache chars
    with Not_found ->
      let f = any_but chars in
      Hashtbl.replace global_any_but_cache chars f ;
      f

  let (--) c1 c2 =
    let c1 = Char.code c1 and c2 = Char.code c2 in
    let s = String.make (c2 - c1 + 1) '_' in
    for i = c1 to c2 do
      s.[i - c1] <- Char.chr i
    done ;
    s

  type group = (string * (point * point)) option ref

  exception Unmatched_group

  let group () =
    ref None

  let store g f state =
    let cp = checkpoint state in
    if f state then begin
      g := Some (extract_from state cp) ;
      true
    end else false

  let extract g =
    match !g with
    | Some res -> res
    | None -> raise Unmatched_group

  let before g = fst (snd (extract g))
  let after g = snd (snd (extract g))
  let around g = snd (extract g)
  let text g = fst (extract g)

  let callback f = f

  let save f state =
    let cp = checkpoint state in
    if f state then extract_from state cp
    else raise Unmatched_group

  let discard f state =
    ignore (f state)
end

module ParserInternals : sig
  (** Returns the list of warnings along with the ast. *)
  val parse :
    ?allow_toplevel_exprs:bool ->
    ScilabStreamReader.state -> ScilabFiveParserAst.source ->
    ScilabFiveParserAst.ast
  (** operator string x its precedence and ast term *)
  val prefix_table :
    (string * (int * ScilabFiveParserAst.unop)) list
  (** operator string x precedence, associativity and ast term *)
  val infix_table :
    (string * (int * [ `Left | `Right ] * ScilabFiveParserAst.op)) list
end = struct
  open ScilabStreamReader
  open ScilabTokenReader
  open ScilabFiveParserAst
  open Printf

  (* These tables have been hand-retrieved, the numbers are arbitrary. *)
  let prefix_table =
    [ "-", (9, Unary_minus) ; "+", (11, Unary_plus) ;
      "~", (4, Not) ; "@", (4, Not) ]
  and infix_table =
    [ "+", (6, `Left, Plus) ; "-", (6, `Left, Minus) ;
      "\\", (7, `Right,  Ldivide) ; ".\\"  , (7, `Left, Dot_ldivide) ; 
      ".\\.", (7, `Left, Kron_ldivide) ; "\\.", (7, `Left, Control_ldivide) ;
      "*", (8, `Left, Times) ; "/", (8, `Left, Rdivide) ;
      ".*", (8, `Left, Dot_times) ; "./", (8, `Left, Dot_rdivide) ;
      ".*.", (8, `Left, Kron_times) ; "./.", (8, `Left, Kron_rdivide) ;
      "*.", (8, `Left, Control_times) ; "/.", (8, `Left, Control_rdivide) ;
      "^", (10, `Right, Power) ; "**", (10, `Right, Power) ;
      ".^", (10, `Right, Dot_power) ;
      "==", (5, `Left, Eq) ; "<>", (5, `Left, Ne) ;
      "~=", (5, `Left, Ne) ; "<", (5, `Left, Lt) ;
      "<=", (5, `Left, Le) ; ">", (5, `Left, Gt) ;
      ">=", (5, `Left, Ge) ; "=", (5, `Left, Eq) ; 
      "&", (3, `Left, And) ; "|", (2, `Left, Or) ;
      (* Scilab 6 only *)
      "&&", (3, `Left, Seq_and) ; "||", (2, `Left, Seq_or) ]

  (* pre-allocated regexpes *)
  let space = char ' ' ||| char '\t'
  let digit = any_of ('0' -- '9')
  let alpha = any_of ('a' -- 'z' ^ 'A' -- 'Z')
  let alnum = any_of ('a' -- 'z' ^ 'A' -- 'Z' ^ '0' -- '9')
  let spaces = star (char '\t' ||| char ' ')
  let string_delim = any_of "'\""
  let matrix_delim = any_of "[{"
  let matrix_end = any_of "}]"
  let before_string = phantom (any_of "'\"")
  let before_matrix = phantom (any_of "[{")
  let field_dot = seq [ char '.' ; phantom (any_but "*^/\\'") ]
  let before_field_dot =
    seq [ star space ; phantom (seq [ char '.' ; any_but "*^/\\'" ]) ]
  let colon_op = seq [ star space ; char ':' ; star space ]
  let before_paren =
    let in_matrix = phantom (char '(')
    and out_of_matrix = seq [ star space ; phantom (char '(') ]
    in function true -> in_matrix | false -> out_of_matrix
  let after_equal = seq [ star space ; char '=' ]
  let float =
    seq [ alts [ seq [ plus digit ; maybe (char '.') ; star digit ] ;
                 seq [ char '.' ; plus digit ] ] ;
          maybe (seq [ any_of "eEdD" ; (* d and D are exponent starters *)
                       maybe (any_of "+-") ;
                       star digit (* 3.e is a valid number *) ]) ]
  let utf = alts [
      seq [ any_of ('\xC2'--'\xDF') ; any_of ('\x80'--'\xBF') ] ;
      seq [ char '\xE0' ; any_of ('\xA0'--'\xBF') ; any_of ('\x80'--'\xBF') ] ;
      seq [ any_of ('\xE1'--'\xEC') ; any_of ('\x80'--'\xBF') ;
            any_of ('\x80'--'\xBF') ] ;
      seq [ char '\xED' ; any_of ('\x80'--'\x9F') ; any_of ('\x80'--'\xBF') ] ;
      seq [ any_of ('\xEE'--'\xEF') ; any_of ('\x80'--'\xBF') ;
            any_of ('\x80'--'\xBF') ] ;
      seq [ char '\xF0' ; any_of ('\x90'--'\xBF') ;
            any_of ('\x80'--'\xBF') ; any_of ('\x80'--'\xBF') ] ;
      seq [ any_of ('\xF1'--'\xF3') ; any_of ('\x80'--'\xBF') ;
            any_of ('\x80'--'\xBF') ; any_of ('\x80'--'\xBF') ] ;
      seq [ char '\xF4' ; any_of ('\x80'--'\x8F') ;
            any_of ('\x80'--'\xBF') ; any_of ('\x80'--'\xBF') ] ]
  let utf_opt = callback (fun state ->
      if peek state >= Char.chr 128 then exec utf state else false)
  let ident =
    let spchars_first = "_%#?$!" in
    let spchars_next = "_%#?$!" ^ '0' -- '9' in
    seq [ 
      (any_of ('a'--'z' ^ 'A'--'Z' ^ spchars_first) ||| utf_opt) ;
      star (any_of ('a'--'z' ^ 'A'--'Z' ^ spchars_next) ||| utf_opt) ]
    ||| char ':'
  let unop = any_of "@~-+"
  let binop =
    alts [
      (* *.3 is parsed as * .3, not *. 3 *)
      seq [ any_of "*/\\" ; star space (* handle 'x * . y' *) ; char '.' ;
            phantom (any_but ("." ^ '0'--'9')) ] ;
      string "**" ; string "~=" ;
      string "==" ; string "@=" ; string "<>" ;
      string "<=" ; string ">=" ; char '=' (* after "==" *);
      any_of "|&*-+/`\\^<>" ;
      seq [ char '.' ; star space ; any_of "*/\\" ; star space ; char '.' ;
            phantom (any_but ("." ^ '0'--'9')) ] ;
      seq [ char '.' ; star space ; any_of "*/\\^" ] ]

  let drop_spaces op bounds =
    (* clean spaces in element wise and kronecker operators *)
    let rec clean s i j =
      if i = String.length s then
        String.sub s 0 j
      else if s.[i] = ' ' then
        clean s (succ i) j
      else (s.[j] <- s.[i] ; clean s (succ i) (succ j))
    in
    let nop = clean op 0 0 in
    if nop <> op then
      nop, [ Replace (bounds, nop, "spaces in operator") ]
    else op, []

  let line_end = star (any_but "\n\000")
  let instr_end = (* FIXME: strings *) star (any_but ",;\n\000")
  let empty_instr = seq [ star space ; maybe comment ; any_of ",;\n\000" ]
  let empty_line = seq [ star space ; maybe comment ; any_of ",;\n\000" ]
  let empty_lines =
    seq [ star (seq [ star space ; maybe comment ; any_of ",;\n" ]) ;
          empty_line ]

  (* parsing context *)
  type context = {
    src : source ; (* to build locations *)
    kwd : string * (point * point) ; (* last struct entered, for term insertion *)
    st : state ; (* reqder state  *)
    in_matrix : bool ; (* for spaces handling *)
    in_function : bool ; (* for parsing toplevel expresions or shell calls *)
    allow_toplevel_exprs : bool ; (* if false, toplevel phrases starting with
                                     an ident are always parsed as shell calls *)
    in_loop : bool ; (* for warning about breaks *)
    next : context option ; (* link to the previous / upper context frame *)
  }

  let descr ?(warns = []) ?(comment = []) cstr ((sl, sc), (el, ec)) ctx =
    { cstr ; comment ; loc = loc ctx.src sl sc el ec ; meta = warns }

  let descr_for_seq ?(warns = []) ?(comment = []) cstr seq =
    { cstr ; loc = merge_descr_locs seq ; meta = warns ; comment }

  let descr_exp descr =
    { descr with cstr = Exp descr ; meta = [] ; comment = [] }

  let string_descr ?warns (tok, (s, e)) ctx =
    descr ?warns tok (s, e) ctx

  let var_descr (text, bounds) ctx =
    match text with
    | "%t" | "%T" -> descr (Bool true) bounds ctx
    | "%f" | "%F" -> descr (Bool false) bounds ctx
    | ":" -> descr Colon bounds ctx
    | _ -> descr (Var (string_descr (text, bounds) ctx)) bounds ctx

  let push kwd ?in_matrix ?in_function ?in_loop ctx =
    { kwd ; src = ctx.src ; st = ctx.st ; next = Some ctx ;
      allow_toplevel_exprs = ctx.allow_toplevel_exprs ;
      in_matrix = (match in_matrix with None -> ctx.in_matrix | Some v -> v) ;
      in_function = (match in_function with None -> ctx.in_function | Some v -> v) ;
      in_loop = (match in_loop with None -> ctx.in_loop | Some v -> v) }

  let rec beginning_of kwd ctx =
    if fst ctx.kwd = kwd then
      fst (snd ctx.kwd)
    else
      match ctx.next with
      | None -> raise Not_found
      | Some ctx -> beginning_of kwd ctx

  let from_last kwd ctx =
    (beginning_of kwd ctx, point ctx.st)

  let rec inside kwds ctx =
    if List.mem (fst ctx.kwd) kwds then
      true
    else
      match ctx.next with
      | None -> false
      | Some ctx -> inside kwds ctx

  let closer str =
    match str.[0], str with
    | '(', _ -> ")"
    | '[', _ -> "]"
    | '{', _ -> "}"
    | 'e',"expression" -> ";"
    | 'f', "function" -> "endfunction"
    | 'p', "program" -> "\000"
    | _ -> "end" 

  let closing_keyword str =
    if String.length str = 0 then false else
      match str.[0], str with
      | 'e', "end" | 'e', "endfunction"
      | 't', "then" | 'e', "else" | 'e', "elseif"
      | 'c', "case" | 'c', "catch" -> true
      | _ -> false

  let terminate term cp kwds ctx =
    (* this function is called when a closing keyword is
       encountered, it does keyword skipping and keyword
       insertion depending on the context *)
    if inside kwds ctx then
      if List.mem (fst ctx.kwd) kwds then
        (* we got a valid terminator *)
        `End (fst term, [])
      else
        (* do not consume the keyword for piggybactracking and
           produce a warning and return a fake terminator *)
        let closer = closer (fst ctx.kwd) in
        restore ctx.st cp ;
        `End (closer,
              [ Insert (point ctx.st, closer,
                        "unterminated " ^ fst ctx.kwd) ])
    else
      (* consume the keyword and produce an error statement. *)
      let warns = [ Drop (snd term, "unexpected '" ^ fst term ^ "'") ] in
      `Stmt (descr_exp (descr ~warns Error (snd term) ctx))

  let rec terminate_expr term cp kwds ctx =
    (* similar to terminate, but never consumes keywords (an
       unexpected keyword will be leaved to the surrounding
       statement to treat) *)
    if List.mem (fst ctx.kwd) kwds then
      (`Term (snd term), fst term, `Ok)
    else
      let closer = closer (fst ctx.kwd) in
      restore ctx.st cp ;
      (`Fake, closer,
       `Warns [ Insert (point ctx.st, closer,
                        "unterminated " ^ fst ctx.kwd)] )

  let parse_string ctx =
    (* Consumes the contents of a string that has been started by
       delimiter [delim]. This function accepts all of Scilab 5's string
       forms, such as [quote dquote quote dquote] which means a single
       character string consisting of a [quote]. Delim could be used to
       display a warning in case of inconsistent delimiters. *)
    let cp = checkpoint ctx.st in
    let opn, _ = save string_delim ctx.st in
    let buf = Buffer.create 80 in
    let rec loop () =
      match read ctx.st with
      | '\'' | '"' as cls ->
        (match peek ctx.st with
         | '\'' | '"' as c ->
           advance_1 ctx.st ; Buffer.add_char buf c ; loop ()
         | _ ->
           let warns =
             if opn.[0] <> cls then
               [ Warning "inconsistent string delimiters" ]
             else []
           in
           descr ~warns (String (Buffer.contents buf)) (from ctx.st cp) ctx)
      | '\000' | '\n' ->
        let warns = [ Recovered "unterminated string" ] in
        descr ~warns Error (from ctx.st cp) ctx
      | c ->
        Buffer.add_char buf c ; loop ()
    in loop ()

  let drop_token ctx =
    let cp = checkpoint ctx.st in
    discard spaces ctx.st ;
    let rec drop_word () =
      match peek ctx.st with
      | '\'' -> ignore (parse_string ctx)
      | '/' when exec comment ctx.st -> discard (maybe (char '\n')) ctx.st
      | '.' | '0' .. '9' when exec float ctx.st -> drop_quote ()
      | ')' | ']' | ':' -> advance_1 ctx.st ; drop_quote ()
      | _ when exec ident ctx.st -> drop_quote ()
      | _ when exec binop ctx.st -> ()
      | _ -> advance_1 ctx.st
    and drop_quote () =
      match peek ctx.st with
      | '\'' -> advance_1 ctx.st ; drop_quote ()
      | _ -> ()
    in
    drop_word () ;
    extract_from ctx.st cp

  (* we patch the text representation to make it more C-like and pass
       it to OCaml's converter, meaning we convert any 'D' exponent prefix
       to an 'E' and put a '0' instead of an empty exponent *)
  let float_of_string str =
    (try str.[String.index str 'd'] <- 'e' with Not_found -> ()) ;
    (try str.[String.index str 'D'] <- 'e' with Not_found -> ()) ;
    let last =  str.[String.length str - 1] in
    let str = if last = 'e' || last = 'E' then str ^ "0" else str in
    float_of_string str

  let rec parse_statement ctx =
    discard spaces ctx.st ;
    let cp = checkpoint ctx.st in
    let default id =
      restore ctx.st cp ;
      if id then
	if ctx.in_function || not ctx.allow_toplevel_exprs then
	  (* nothing worked: parse as a shell call *)
	  `Stmt (parse_shell_call ctx)
	else
          (* if we're at toplevel, it could also be an expr *)
          let as_expr = parse_toplevel_expr ctx in
          match as_expr.cstr with
          (* TODO: macro generate 'typeof id = function' *)
          | Error -> `Stmt (parse_shell_call ctx)
          | _ -> `Stmt (descr_exp as_expr)
      else  `Stmt (descr_exp (parse_toplevel_expr ctx))
    in
    match peek ctx.st with
    | '\n' | ',' | ';' -> advance_1 ctx.st ; parse_statement ctx
    | '\000' -> terminate ("\000", from ctx.st cp) cp [ "program" ] ctx
    | '/' when peek_ahead ctx.st 1 = '/' ->
      let (text, bounds) = save comment ctx.st in
      `Stmt (descr (Comment text) bounds ctx)
    | '[' | '{' ->
      (* try and parse multiple lvalues *)
      let ctxm = push (save any ctx.st) ~in_matrix:true ctx in
      discard (spaces ) ctx.st ;
      let rec loop acc =
        match parse_expr ctxm with
        | expr, ((`Fake | `Term _), (";" | "\n" | "\000")) ->
          (* woops, it was a toplevel matrix expression *)
          default false
        | expr, ((`Fake | `Term _), ("]" | "}")) ->
          let exprs = expr :: acc in
          if exec after_equal ctx.st && peek ctx.st <> '=' then
            (* it is indeed an assignment and not a matrix *)
            (* FIXME: check that expressions are only valid lvalues *)
            let expr = parse_toplevel_expr ctx in
            let phrase = Assign (List.rev exprs, expr) in
            `Stmt (descr phrase (from ctx.st cp) ctx)
          else default false
        | expr, ((`Fake | `Term _), ("," | _ (* just spaces *))) ->
          discard (seq [ star space ; maybe (char ',') ]) ctx.st ;
          loop (expr :: acc)
      in loop []
    | c ->
      if exec ident ctx.st then
        let id_text, id_bounds as id = extract_from ctx.st cp in
        if peek ctx.st = '.' then
          (* an extraction is performed if a dot follows an ident
               directly ("x.f = 3" but not "x . f = 3") even in
               presence of a keyword *)
          let lexpr = parse_extraction (var_descr id ctx) ctx in
          if exec after_equal ctx.st && peek ctx.st <> '=' then
            (* parse as an assignment *)
            let expr = parse_toplevel_expr ctx in
            let phrase = Assign ([ lexpr ], expr) in
            `Stmt (descr phrase (fst id_bounds, point ctx.st) ctx)
          else default true
        else if exec after_equal ctx.st then
          (* detect simple assignments before identifying keywords *)
          if peek ctx.st <> '=' then
            (* "id =..." -> parse as an assignment *)
            let expr = parse_toplevel_expr ctx in
            let phrase = Assign ([ var_descr id ctx ], expr) in
            `Stmt (descr phrase (fst id_bounds, point ctx.st) ctx)
          else default true
        else
          match id_text.[0], id_text with
          (* keywords *)
          | 'i', "if" -> `Stmt (parse_if (push id ctx))
          | 's', "select" -> `Stmt (parse_select (push id ctx))
          | 't', "try" -> `Stmt (parse_try (push id ctx))
          | 'w', "while" ->
            `Stmt (parse_while (push id ~in_loop:true ctx))
          | 'f', "for" ->
            `Stmt (parse_for (push id ~in_loop:true ctx))
          (* instruction keywords *)
          | 'r', "return" when exec empty_instr ctx.st ->
            `Stmt (descr Return id_bounds ctx)
          | 'b', "break" ->
            let warns =
              (if not ctx.in_loop then
                 [ Warning "break outside of a loop" ]
               else [])
              @ (if not (exec empty_instr ctx.st) then
                   [ Warning "break takes no argument" ]
                 else [])
            in
            `Stmt (descr ~warns Break id_bounds ctx)
          | 'c', "continue" ->
            let warns =
              (if not ctx.in_loop then
                 [ Warning "continue outside of a loop" ]
               else [])
              @ (if not (exec empty_instr ctx.st) then
                   [ Warning "continue takes no argument" ]
                 else [])
            in
            `Stmt (descr ~warns Continue id_bounds ctx)
          | 'f', "function" ->
            `Stmt (parse_function (push id ~in_loop:false ~in_function:true ctx))
          (* potential terminators *)
          | 'e', "end" ->
	    terminate id cp [ "select" ; "if" ; "for" ; "while" ; "try" ] ctx
          | 'e', "endfunction" ->
	    terminate id cp [ "function" ] ctx
          | ('t', "then") | ('d', "do")->
            let msg = id_text ^ " only allowed on the right of a condition" in
            let warns = [ Drop (id_bounds, msg) ] in
            `Stmt (descr_exp (descr ~warns Error id_bounds ctx))
          | 'e', "else"
          | 'e', "elseif" ->
            terminate id cp [ "select" ; "if" ] ctx
          | 'c', "case" -> terminate id cp [ "select" ] ctx
          | 'c', "catch" -> terminate id cp [ "try" ] ctx
          | _ (* not a keyword *) ->
            (* detect injections "a (expr) = ... " *)
            if exec (before_paren false) ctx.st then
              let lexpr = parse_extraction (var_descr id ctx) ctx in
              if exec empty_instr ctx.st then
                `Stmt (descr_exp lexpr)
              else if exec after_equal ctx.st && peek ctx.st <> '=' then
                (* parse as an assignment *)
                let expr = parse_toplevel_expr ctx in
                let phrase = Assign ([ lexpr ], expr) in
                `Stmt (descr phrase (fst id_bounds, point ctx.st) ctx)
              else
                (* '(' read -> parse as expr, not shell call *)
                (restore ctx.st cp ; `Stmt (descr_exp (parse_toplevel_expr ctx)))
            else default true
      else default false
    
  and parse_shell_args ctx =
    let rec skip acc =
      match peek ctx.st with
      | '\'' | '"' ->
        let str = parse_string ctx in
        skip (str :: acc)
      | ' ' | '\t' -> advance_1 ctx.st ; skip acc
      | ',' | ';' | '\n' | '\000' -> advance_1 ctx.st ; List.rev acc
      | _ -> grab (checkpoint ctx.st) acc
    and grab cp acc =
      match peek ctx.st with
      | ' ' | '\t' | ',' | ';' | '\n' | '\000' ->
        let ctns, bounds = extract_from ctx.st cp in
        let warns =
          if closing_keyword ctns then
            let msg = sprintf
                "this %s will not be interpreted as a terminator, \
                 insert a line break before it or surround it with \
                 double quotes to dismiss this warning" ctns
            in
            [ Replace (bounds, "\"" ^ ctns ^ "\"", msg) ]
          else []
        in
        skip (descr ~warns (String ctns) bounds ctx :: acc)
      | _ -> advance_1 ctx.st ; grab cp acc
    in
    skip []

  and parse_shell_call ctx =
    let cp = checkpoint ctx.st in
    if exec ident ctx.st then
      let var = var_descr (extract_from ctx.st cp) ctx in
      let args = parse_shell_args ctx in
      let nargs = List.map (fun n -> (None, n)) args in
      let call = descr_for_seq (Call (var, nargs, Shell)) (var :: args) in
      descr_exp call
    else
      descr_exp (parse_toplevel_expr ctx)

  and parse_main_expr ?terminators ctx =
    parse_expr ?terminators (push ("expression", here ctx.st) ctx)

  and parse_cond_expr terminators ctx =
    match parse_main_expr ~terminators (push ("expression", here ctx.st) ctx) with
    | expr, (`Term loc, ("," | ";" | "\n" as term)) as res ->
      discard spaces ctx.st ;
      let cp = checkpoint ctx.st in
      if exec ident ctx.st then
        let nterm, nloc = extract_from ctx.st cp in
        if List.mem nterm terminators then
          let msg = sprintf "unexpected %S between condition and %S" term nterm in
          let nexpr = { expr with meta = Drop (loc, msg) :: expr.meta } in
          nexpr, (`Term nloc, nterm)
        else (restore ctx.st cp ; res)
      else (restore ctx.st cp ; res)
    | res -> res

  and parse_toplevel_expr ctx =
    let expr, term = parse_main_expr ctx in
    if fst term = `Fake then (
      discard spaces ctx.st ;
      let cp = checkpoint ctx.st in
      if exec (phantom (any_but ",;\000\n")) ctx.st then
        if exec (ident ||| unop ||| any_of "{[(") ctx.st then
          let msg = "unterminated statement" in
          restore ctx.st cp ;
          { expr with meta = Insert (checkpoint_point cp, ";", msg) :: expr.meta }
        else
          let text, bounds = drop_token ctx in
          let msg = sprintf "unexpected token %S after expression" text in
          { expr with meta = Drop (bounds, msg) :: expr.meta }
      else expr)
    else expr

  and parse_extraction lexpr ctx =
    if exec (before_paren ctx.in_matrix) ctx.st then
      let args, warns = parse_args (push ~in_matrix:false (save any ctx.st) ctx) in
      let _, (sloc, _) = lexpr.loc in
      let expr = Call (lexpr, args, Tuplified) in
      let expr = descr ~warns expr (sloc, point ctx.st) ctx in
      parse_extraction expr ctx
    else
      let blanks = ref false in
      let cp = checkpoint ctx.st in
      blanks := !blanks || exec (plus space) ctx.st ;
      if exec field_dot ctx.st then begin
        (* let dot, dot_bounds = extract_from ctx.st cp in *)
        blanks := !blanks || exec (plus space) ctx.st ;
        if exec ident ctx.st then
          let name, name_bounds = extract_from ctx.st cp in
          let rexpr = descr (String name) name_bounds ctx in
	  let expr = Call (lexpr, [ None, rexpr], Field) in
          let warns =
            if not !blanks then []
            else [ Warning "avoid spaces around dot field accessors" ]
          in
          let expr = descr_for_seq ~warns expr [ lexpr ; rexpr ] in
          parse_extraction expr ctx
        else begin
          discard instr_end ctx.st ;
          let warns = [ Recovered "a name is expected after this dot" ] in
          let rexpr = descr ~warns Error (from ctx.st cp) ctx in
          descr_for_seq (Call (lexpr, [ None, rexpr], Field)) [ lexpr ; rexpr ]
        end
      end else
        (restore ctx.st cp ; parse_comment lexpr ctx)

  and parse_statements ctx =
    let rec loop acc =
      match parse_statement ctx with
      | `Stmt s -> loop (s :: acc)
      | `End term -> List.rev acc, term
    in loop []

  and parse_seq ctx =
    match parse_statements ctx with
    | [i], (term, warns) -> { i with meta = warns @ i.meta }, term
    | seq, (term, warns) -> descr_for_seq ~warns (Seq seq) seq, term

  and parse_matrix ctx sloc =
    let rec discard_empty (res, coms) =
      (* returns true if a row change occured *)
      discard spaces ctx.st ;
      match peek ctx.st with
      | '\n' | ';' -> advance_1 ctx.st ; discard_empty (true, coms)
      | ',' | ' ' | '\t' -> advance_1 ctx.st ; discard_empty (res, coms)
      | '/' when peek_ahead ctx.st 1 = '/' ->
        let com = string_descr (save comment ctx.st) ctx in
        discard_empty (res, com :: coms)
      | _ -> res, coms
    in
    let change_row (acc, line_acc) =
      if line_acc <> [] then
        let columns = List.rev line_acc in
        let line = descr_for_seq columns columns in
        (line :: acc, [])
      else (acc, line_acc)
    in
    let rec loop (acc, line_acc) =
      if peek ctx.st = ']' || peek ctx.st = '}' then
        finish (acc, line_acc) (fst (save any ctx.st))
      else
        match parse_expr ctx with
        | expr, ((`Fake | `Term _), (";" | "\n")) ->
          let _, comment = discard_empty (false, []) in
          let accs = (acc, { expr with comment } :: line_acc) in
          loop (change_row accs)
        | expr, ((`Fake | `Term _), ("]" | "}" as clo)) ->
          finish (acc, expr :: line_acc) clo
        | expr, ((`Fake | `Term _), ("," | " ")) ->
          let em, comment = discard_empty (false, []) in
          let accs = (acc, { expr with comment } :: line_acc) in
          if em  then
            loop (change_row accs)
          else
            loop accs
        | expr, _ -> assert false
    and finish accs clo =
      let warns =
        match fst ctx.kwd, clo with
        | "{", "]" | "[", "}" ->
          [ Warning "Inconsistent matrix delimiters" ]
        | _ -> []
      in
      let lines = List.rev (fst (change_row accs)) in
      descr ~warns (Matrix lines) (sloc, point ctx.st) ctx
    in
    let _, comment = discard_empty (false, []) in
    { (loop ([], [])) with comment }

  and parse_function ctx =
    discard spaces ctx.st ;
    let error msg =
      (* in case of error, we drop the rest of the function definition *)
      discard instr_end ctx.st ;
      ignore (parse_seq ctx) ;
      (* and reraise the error *)
      let warns = [ Recovered msg ] in
      descr_exp (descr ~warns Error (from_last "function" ctx) ctx)
    in
    let rec name rets =
      discard spaces ctx.st ;
      let warns = match peek ctx.st with
        | '=' ->
          advance_1 ctx.st ; discard spaces ctx.st ; []
	| _ ->
          [ Insert (point ctx.st, "=",
                    "equal sign required after return parameters") ]
      in
      let id = group () in
      if exec (seq [ spaces ; store id ident ; spaces ]) ctx.st then
        let with_missing = Warning "missing function parameters" :: warns in
        match peek ctx.st with
        | '(' -> advance_1 ctx.st ;
          args (string_descr ~warns (extract id) ctx) rets
        | '\n' -> advance_1 ctx.st ;
          body (string_descr ~warns:with_missing (extract id) ctx) rets []
        | '/' when peek_ahead ctx.st 1 = '/' ->
          advance_1 ctx.st ;
          body (string_descr ~warns:with_missing (extract id) ctx) rets []
        | _ -> error "syntax error after function name"
      else
        error "missing or invalid function name"
    and rets () =
      if exec matrix_delim ctx.st then
        let rec collect acc =
          discard spaces ctx.st ;
          let cp = checkpoint ctx.st in
          if exec ident ctx.st then
            let var = string_descr (extract_from ctx.st cp) ctx in
            discard spaces ctx.st ;
            match peek ctx.st with
            | ',' -> advance_1 ctx.st ; collect (var :: acc)
            | ';' ->
	      let sloc = point ctx.st in
	      advance_1 ctx.st ;
	      let msg = Drop ((sloc, point ctx.st), "bad separator \";\"") in
	      let var = { var with meta = msg :: var.meta } in
	      collect (var :: acc)
            | 'a'..'z' | 'A'..'Z' ->
	      let msg = Insert (point ctx.st, ",", "missing separator") in
	      let var = { var with meta = msg :: var.meta } in
	      collect (var :: acc)
            | '}' | ']' -> advance_1 ctx.st ; name (List.rev (var :: acc))
            | _ -> error "syntax error in return parameters"
          else
            error "syntax error in return parameters"
        in
        discard spaces ctx.st ;
        if exec matrix_end ctx.st then name [] else collect []
      else
        let rec collect acc =
          discard spaces ctx.st ;
          let cp = checkpoint ctx.st in
          if exec ident ctx.st then
            let var = string_descr (extract_from ctx.st cp) ctx in
            discard spaces ctx.st ;
            match acc, peek ctx.st with
            | [], '(' ->
              advance_1 ctx.st ; args var []
            | [], ('\n' | ',' | ';') ->
              body
		{ var with meta = Warning "missing parameters" :: var.meta }
		[] []
            | [], '/' when peek_ahead ctx.st 1 = '/' ->
              let var =
                { var with meta = Warning "missing parameters" :: var.meta }
              in
              body var [] []
            | _, '(' ->
              error "invalid function name"
            | _, ',' ->
              advance_1 ctx.st ; collect (var :: acc)
            | _, _ (* let's hope for '=' *) ->
              name (List.rev (var :: acc))
          else if acc = [] then
            error "missing or invalid function name"
          else
            error "syntax error in return parameters"
        in collect []
    and args name rets =
      let check_eol () =
        if not (exec (seq [ star space ; comment ||| any_of ",;\n" ]) ctx.st) then
          [ Insert (point ctx.st, "\n",
		    "terminator expected after function parameters") ]
        else []
      in
      let rec loop acc =
        discard spaces ctx.st ;
        if exec (char ')') ctx.st then
          body { name with meta = check_eol () @ name.meta } rets []
        else
          let cp = checkpoint ctx.st in
          if exec ident ctx.st then
            let var = string_descr (extract_from ctx.st cp) ctx in
            discard spaces ctx.st ;
            match peek ctx.st with
            | ',' -> advance_1 ctx.st ; loop (var :: acc)
            | ';' ->
	      let sloc = point ctx.st in
	      advance_1 ctx.st ;
	      let msg = Drop ((sloc, point ctx.st), "bad separator \";\"") in
	      let var = { var with meta = msg :: var.meta } in
	      loop (var :: acc)
            | 'a'..'z' | 'A'..'Z' ->
	      let msg = Insert (point ctx.st, ",", "missing separator") in
	      let var = { var with meta = msg :: var.meta } in
	      loop (var :: acc)
            | ')' ->
              advance_1 ctx.st ;
              body { name with meta = check_eol () @ name.meta } rets
                (List.rev (var :: acc))
            | '\n' ->
              error "unterminated parameter list, expecting a ')'"
            | '/' when peek_ahead ctx.st 1 = '/' ->
              parse_comment
                (error "unterminated parameter list, expecting a ')'")
                ctx
            | _ ->
              error "syntax error in parameters"
              (* TODO: default values *)
          else
            error "syntax error in parameters"
      in
      discard spaces ctx.st ; loop []
    and body name rets args =
      let comment = parse_comment_block ctx in
      let body, _ = parse_seq ctx in
      descr ~comment (Defun { name ; args ; rets ; body })
	(from_last "function" ctx) ctx
    in rets ()

  and parse_comment : 'a. 'a descr -> context -> 'a descr
    = fun descr ctx ->
      let cp = checkpoint ctx.st in
      discard spaces ctx.st ;
      let cp' = checkpoint ctx.st in
      if exec comment ctx.st then begin
        let comment = string_descr (extract_from ctx.st cp') ctx in
        { descr with comment = descr.comment @ [ comment ] }
      end else begin
        restore ctx.st cp ; descr
      end

  and parse_comment_block ctx =
    let rec loop acc =
      let cp = checkpoint ctx.st in
      discard spaces ctx.st ;
      let cp' = checkpoint ctx.st in
      if exec comment ctx.st then begin
        let text = string_descr (extract_from ctx.st cp') ctx in
        loop (text :: acc)
      end else begin
        restore ctx.st cp ; List.rev acc
      end
    in loop []

  and parse_if ctx =
    let rec parse_one_if () =
      let cond, _ = parse_cond_expr [ "then" ] ctx in
      let phrases, term = parse_seq ctx in
      match term with
      | "else" ->
        let else_phrases, _ = parse_seq ctx in
        descr (If (cond, phrases, Some else_phrases)) (from_last "if" ctx) ctx
      | "elseif" ->
        let else_branch = parse_one_if () in
        descr (If (cond, phrases, Some else_branch)) (from_last "if" ctx) ctx
      | _ (* "end" *) ->
        descr (If (cond, phrases, None)) (from_last "if" ctx) ctx
    in parse_one_if ()

  and parse_select ctx =
    let cond, _ = parse_main_expr ctx in
    discard empty_lines ctx.st ;
    let rec parse_case term default cases =
      match term, default with
      | "else", None ->
        let phrases, term = parse_seq ctx in
        parse_case term (Some phrases) cases
      | "else", Some _ ->
        let phrases, term = parse_seq ctx in
        let warn = Drop (snd phrases.loc, "duplicate else branch") in
        let res = parse_case term default cases in
        { res with meta = warn :: res.meta }
      | "case", Some _ ->
        let _ = parse_cond_expr [ "then" ] ctx in
        let phrases, term = parse_seq ctx in
        let warn = Drop (snd phrases.loc, "useless case after else branch") in
        let res = parse_case term default cases in
        { res with meta = warn :: res.meta }
      | "case", None ->
        let test, _ = parse_cond_expr [ "then" ] ctx in
        let phrases, term = parse_seq ctx in
        parse_case term None ((test, phrases) :: cases)
      | "end", default ->
        let cases = List.rev cases in
        descr (Select { cond ; cases ; default }) (from_last "select" ctx) ctx
      | _ -> assert false
    in
    let phrases, term = parse_seq ctx in
    let res = match term with
      | "case" -> parse_case term None []
      | "end" ->
        let warns = [ Recovered "empty select" ] in
        descr ~warns (Select { cond ; cases = [] ; default = None })
          (from_last "select" ctx) ctx
      | "else" ->
        let warn = Recovered "expecting at least one case" in
        let res = parse_case term None [] in
        { res with meta = warn :: res.meta }
      | _ -> assert false
    in
    if phrases.cstr <> Seq [] then
      let msg = "the condition must be followed by a case" in
      let warn = Drop (snd phrases.loc, msg) in
      { res with meta = warn :: res.meta }
    else res

  and parse_try ctx =
    let phrases, term = parse_seq ctx in
    match term with
    | "catch" ->
      let catch_phrases, _ = parse_seq ctx in
      (* fixme: check "end" *)
      descr (Try (phrases, catch_phrases)) (from_last "try" ctx) ctx ;
    | _ (* "end" *) ->
      let warns = [ Warning "missing catch in try block" ] in
      let sloc = point ctx.st in
      let fake = descr (Seq []) (sloc, sloc) ctx in
      descr ~warns (Try (phrases, fake )) (from_last "try" ctx) ctx

  and parse_while ctx =
    let cond, _ = parse_cond_expr [ "do" ; "then" ] ctx in
    let phrases, _ = parse_seq ctx in
    descr (While (cond, phrases)) (from_last "while" ctx) ctx

  and parse_for ctx =
    let iter = group () in
    let before_range = seq [ star space ; store iter ident ;
                             star space ; char '=' ; star space ] in
    if not (exec before_range ctx.st) then
      let warns = [ Recovered "'for' must be followed by an assignment" ] in
      let res = descr_exp (descr ~warns Error (from_last "for" ctx) ctx) in
      ignore (parse_seq ctx) ; res
    else
      let (n, n_bounds) = extract iter in
      let msg = "for iterator cannot be a ':'" in
      let warns = if n = ":" then [ Recovered msg ] else [] in
      let var = descr ~warns n n_bounds ctx in
      let range, _ = parse_cond_expr [ "do" ] ctx in
      let phrases, _ = parse_seq ctx in
      descr (For (var, range, phrases)) (from_last "for" ctx) ctx

  and parse_identity_args ctx =
    let rec loop acc ws =
      let arg, term = parse_expr ctx in
      let cp = checkpoint ctx.st in
      match term with
      | (`Fake | `Term _), "," ->
        loop (arg :: acc) ws
      | (`Fake | `Term _), ")" ->
        List.rev (arg :: acc), ws
      | (`Fake | `Term _), "\000" ->
        let w = Recovered "unterminated argument list" in
        List.rev (arg :: acc), (w :: ws)
      | (`Fake | `Term _), "\n" ->
        let w = Insert (point ctx.st, ")",
			"unsupported line break in argument list") in
        List.rev (arg :: acc), (w :: ws)
      | (`Fake | `Term _), ";" ->
        let w = Drop (from_last "(" ctx,
		      "unsupported argument separator \";\"") in
        loop (arg :: acc) (w :: ws)
      | _ ->
        restore ctx.st cp ;
        let w = Insert (point ctx.st, ")",
			"unterminated argument list") in
        List.rev (arg :: acc), (w :: ws)
    in
    if exec (seq [ star space ; char ')' ]) ctx.st then
      [], []
    else loop [] []

  and parse_args ctx =
    let rec loop acc ws =
      let g = group () in
      let def_exp =
        seq [ star space ; store g ident ; star space ;
	      char '=' ; phantom (any_but "=") ]
      in
      let name =
        if exec def_exp ctx.st then
          Some (string_descr (extract g) ctx)
        else None
      in
      let arg, term = parse_expr ctx in
      let arg = name, arg in
      let cp = checkpoint ctx.st in
      match term with
      | (`Fake | `Term _), ")" ->
        List.rev (arg :: acc), ws
      | (`Fake | `Term _), "," ->
        loop (arg :: acc) ws
      | (`Fake | `Term _), "\000" ->
        let w = Recovered "unterminated argument list" in
        List.rev (arg :: acc), (w :: ws)
      | (`Fake | `Term _), "\n" ->
        let w = Insert (point ctx.st, ")",
			"unsupported line break in argument list") in
        List.rev (arg :: acc), (w :: ws)
      | (`Fake | `Term _), ";" ->
        let w = Drop (from_last "(" ctx,
		      "unsupported argument separator \";\"") in
        loop (arg :: acc) (w :: ws)
      | _ ->
        restore ctx.st cp ;
        let w = Insert (point ctx.st, ")",
			"unterminated argument list") in
        List.rev (arg :: acc), (w :: ws)
    in
    if exec (seq [ star space ; char ')' ]) ctx.st then
      [], []
    else loop [] []

  and detect_end_of_expr terminators ctx =
    let cp = checkpoint ctx.st in
    let terminate opn = terminate_expr (extract_from ctx.st cp) cp opn ctx in
    match read ctx.st with
    | ')' -> terminate [ "(" ]
    | ']' | '}' -> terminate [ "{" ; "[" ]
    | '\000' -> terminate [ "program" ; "expression" ]
    | ',' -> terminate [ "{" ; "[" ; "(" ; "expression" ]
    | ';' -> terminate [ "{" ; "[" ; "(" ; "expression" ]
    | '\n' -> terminate [ "{" ; "[" ; "expression" ]
    | ':' ->
      (* leave the range separator aloooone *)
      restore ctx.st cp ; `Fake, ":", `Ok
    | _ ->
      let fake = if ctx.in_matrix then "," else "\n" in
      if exec ident ctx.st then
        let id, id_bounds = extract_from ctx.st cp in
        if List.mem id terminators then
          (* terminator keyword, drop it and return it *)
          `Term id_bounds, id, `Ok
        else (
          (* unknown keyword, keep it *)
          restore ctx.st cp ;
          let w = [ Insert (point ctx.st, "\n",
                            sprintf "missing terminator before %S" id) ] in
          (`Term (here ctx.st), fake, `Warns w))
      else
        (* drop any other kind of token *)
        let text, bounds = restore ctx.st cp ; drop_token ctx in
        let warns = [ Drop (bounds, sprintf "unexpected token %S" text) ] in
        let err = descr ~warns Error bounds ctx in
        (`Fake, fake, `Err err)

  and parse_expr ?(terminators = []) ctx =
    (* expression parsing is decomposed into three steps : collect
       tokens, apply priorities and parse range expressions. The
       collection is decomposed into mutually recursive functions to
       parse atomic expressions (idents, constants and calls plus
       postfix operations), prefix operators, infix operators, and
       function arguments. The second step uses a fairly simple
       precedence fold to build the expression tree. Finally, the
       operation is repeated in case of a ':' operator to form range
       expressions. *)
    let tokenize () =
      let rec prefix eacc =
	discard spaces ctx.st ;
	let cp = checkpoint ctx.st in
	let loc = point ctx.st in
	if exec matrix_delim ctx.st then begin
          let tok = extract_from ctx.st cp in
          let mat = parse_matrix (push tok ~in_matrix:true ctx) loc in
          transpose loc mat eacc
	end else if exec before_string ctx.st then begin
          let str = parse_string ctx in
          postfix loc str eacc
	end else if exec (char '(') ctx.st then
          let tok = extract_from ctx.st cp in
          let args, warns = parse_identity_args (push tok ~in_matrix:false ctx) in
          let expr = descr ~warns (Identity args) (loc, point ctx.st) ctx in
          transpose loc expr eacc
	else if exec unop ctx.st then
          let tok = extract_from ctx.st cp in
          prefix (`Prefix tok :: eacc)
	else if exec float ctx.st then
          let fs, f_bounds = extract_from ctx.st cp in
          let warns =
            match fs.[ String.length fs - 1], peek ctx.st with
            | '.',  ('/' | '*' | '^' | '\\') ->
              let _, (fl, fc) = f_bounds in
              [ Drop (((fl, fc), (fl, fc - 1)),
                      "this dot is the end of a number, \
                       not the start of an element-wise or Kronecker operator, \
                       drop it or insert a space to disambiguate") ]
            | _ -> []
          in
          transpose loc (descr ~warns (Num (float_of_string fs)) f_bounds ctx) eacc
	else if exec ident ctx.st then
          let var = var_descr (extract_from ctx.st cp) ctx in
          if exec (before_field_dot ||| before_paren ctx.in_matrix) ctx.st then 
            (* TODO: warn about spaces *)
            let expr = parse_extraction var ctx in
            transpose loc expr eacc
          else
            transpose loc var eacc
	else
        if exec comment ctx.st then
          let com = extract_from ctx.st cp in
          let term = detect_end_of_expr terminators ctx in
          List.rev (`Comment com :: eacc), term
        else
          List.rev eacc,
          detect_end_of_expr terminators ctx
      and transpose sloc acc eacc =
	if exec (char '\'') ctx.st then
          let exp = Unop (Transpose_conjugate, acc) in
          let exp = descr exp (sloc, point ctx.st) ctx in
          postfix sloc exp eacc
	else
          postfix sloc acc eacc
      and postfix sloc acc eacc =
	if exec (seq [ star space ; string ".'" ]) ctx.st then
          let exp = Unop (Transpose_non_conjugate, acc) in
          let exp = descr exp (sloc, point ctx.st) ctx in
          postfix sloc exp eacc
	else
          infix (`Atom acc :: eacc)
      and infix eacc =
	let cp = checkpoint ctx.st in
	let after_spaces () =
          let eacc =
            if exec comment ctx.st then begin
              let tok = extract_from ctx.st cp in
              (* discard (maybe (char '\n')) ctx.st ; *)
              `Comment tok :: eacc
            end else eacc
          in
          let cp = checkpoint ctx.st in
          if exec binop ctx.st then
            let op, op_bounds as tok = extract_from ctx.st cp in
            let op, warns = drop_spaces op op_bounds in
            let warns =
              if op = "=" then
		Replace (op_bounds, "==", "deprecated operator \"=\"") :: warns
              else
                match (op.[String.length op - 1],
                       peek ctx.st, peek_ahead ctx.st 1) with
                | ('/' | '*' | '^' | '\\'), '.', ('0' .. '9') ->
                  let cp = checkpoint ctx.st in
                  let dot, dot_bounds = save any ctx.st in
                  restore ctx.st cp ;
                  Drop (dot_bounds,
                        "this dot is the start of the number, \
                         not the end of a Kronecker operator, \
                         drop it or insert a space after it to \
                         disambiguate") :: warns
                | _ -> warns
            in
            prefix (`Infix ((op, op_bounds), warns) :: eacc)
          else
            List.rev eacc,
            detect_end_of_expr terminators ctx
	in
	if ctx.in_matrix && exec spaces ctx.st then
          match peek ctx.st with
          | '.' when exec float ctx.st ->
            restore ctx.st cp ;
            List.rev eacc, (`Term (here ctx.st), " ", `Ok)
          | '.' | ':' | ']' | '}' | '=' -> after_spaces ()
          | '/' when peek_ahead ctx.st 1 = '/' -> after_spaces ()
          | _ ->
            let cp = checkpoint ctx.st in
            if exec binop ctx.st then          
              let op, op_bounds = extract_from ctx.st cp in
              let op, warns = drop_spaces op op_bounds in
              prefix (`Infix ((op, op_bounds),warns) :: eacc)
            else List.rev eacc, (`Term (here ctx.st), " ", `Ok)
	else (discard spaces ctx.st ; after_spaces ())
      in prefix []
    in
    let rec apply_priorities tokens =
      let rec prefix lvl tokens =
        match tokens with
        | `Prefix (op, (sloc, eloc)) :: rest ->
          let op_lvl, op = List.assoc op prefix_table in
          let first, rest = infix op_lvl rest in
          let _, (_, eloc) = first.loc in
          descr (Unop (op, first)) (sloc, eloc) ctx , rest
        | `Atom e :: rest ->
          e, rest
        | `Error msg :: [] ->
          let loc = point ctx.st, point ctx.st in
          let warns = [ Recovered msg ] in
          descr ~warns Error loc ctx, []
        | `Comment (text, ((sloc, _) as bounds)) :: [] ->
          let comment = [ descr text bounds ctx ] in
          let warns = [ Recovered "unterminated expression" ] in
          descr ~warns ~comment Error (sloc, sloc) ctx, []
        | [] ->
          let loc = point ctx.st, point ctx.st in
          let warns = [ Recovered "unterminated expression" ] in
          descr ~warns Error loc ctx, []
        | _ -> assert false
      and infix lvl tokens =
        let rec loop acc prest =
          match prest with
          | [] -> acc, []
          | `Comment (text, bounds) :: rest ->
            let comment = [ descr text bounds ctx ] in
            loop { acc with comment } rest
          | `Atom ({ cstr = Error ; meta }) :: rest ->
            (* a bit of a hack: this happens when an unexpected token
               is dropped, we change this error to a warning instead
               of dropping the whole expressiom *)
            loop { acc with meta = meta @ acc.meta } rest
          | `Infix ((op, _), warns) :: rest ->
            let op_lvl, op_assoc, op = List.assoc op infix_table in
            if op_lvl < lvl then
              acc, prest
            else
              let op_lvl = if op_assoc = `Left then op_lvl + 1 else op_lvl in 
              let right, rest = infix op_lvl rest in
	      let expr = Op (op, acc, right) in
              let expr = descr_for_seq ~warns expr [ acc ; right ] in
              loop expr rest
          | _ -> assert false
        in
        let left, rest = prefix lvl tokens in
        loop left rest
      in
      infix 0 tokens
    in
    let rec parse_range acc =
      let tokens, (tt, tn, ws) = tokenize () in
      let exp =
        match ws with
        | `Ok ->
          fst (apply_priorities tokens)
        | `Err err ->
          fst (apply_priorities (tokens @ [ `Atom err ]))
        | `Warns ws ->
          let exp = fst (apply_priorities tokens) in
          { exp with meta = ws @ exp.meta }
      in
      match tt, tn with 
      | `Fake, ":" ->
        discard colon_op ctx.st ;
        parse_range (exp :: acc)
      | term -> List.rev (exp :: acc), term
    in
    match parse_range [] with
    | [], _ -> assert false
    | [e], term -> e, term
    | [l;r], term ->
      descr_for_seq (Range (l, None, r)) [ l ; r ], term
    | [l;m;r], term ->
      descr_for_seq (Range (l, Some m, r)) [ l ; r ], term
    | seq, term ->
      let warns = [ Recovered "too many ':'" ] in 
      descr_for_seq ~warns Error seq, term

  let parse ?(allow_toplevel_exprs = false) state src =
    let ctx = { src ; kwd = ("program", ((0,0), (0,0))) ; st = state ;
                in_matrix = false ; in_loop = false ; in_function = false ;
                next = None ; allow_toplevel_exprs } in
    let ast, _ = parse_statements ctx in
    ast
end

(** Builds an AST for the file at the given path and returns the list
    of warnings that may have happened during its parsing. Actually,
    these warnings are recovered errors, and a file is completely
    valid iff it produces zero warning. In any other case, either its
    AST is partial or the parser made non trivial decisions to build
    it. This error resilient parsing should only be used by tools to
    be able to give some information even on incomplete files. *)
let parse_file ?(allow_toplevel_exprs = false) name =
  let chan = open_in name in
  let reader = ScilabStreamReader.channel_reader chan in
  let res = ParserInternals.parse
	      ~allow_toplevel_exprs
	      reader (ScilabFiveParserAst.File name) in
  close_in chan ;
  res

(** Builds an AST from the program text contained in the passed
    string. See {!parse_file} for a disclaimer. *)
let parse_string ?(allow_toplevel_exprs = true) str =
  let reader = ScilabStreamReader.string_reader str in
  ParserInternals.parse
    ~allow_toplevel_exprs
    reader (ScilabFiveParserAst.String str)
