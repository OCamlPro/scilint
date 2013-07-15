{

  open Lexing
  open ScilabParser


  exception Err_str of string

  exception Lex_err of string

  let matrix_level = ref 0

  let last_token = ref SOF

  let str_cmt = ref ""

  let str = ref ""

  let shellmode_on = ref false

  (* We need this when we parse several files *)
  let init_lexer_var () =
    shellmode_on := false;
    last_token := SOF;
    matrix_level := 0

  let print_pos pos =
    Printf.printf "%i %i %i" pos.pos_lnum pos.pos_bol pos.pos_cnum

  let print_lexbuf lexbuf =
    Printf.printf "st :"; print_pos lexbuf.lex_start_p;
    Printf.printf "; curr :"; print_pos lexbuf.lex_curr_p;
    Printf.printf "; st_pos :%i" lexbuf.lex_start_pos;
    Printf.printf "; curr_pos :%i \n" lexbuf.lex_curr_pos

  let return_token tok =
    if !shellmode_on then shellmode_on := false;
    last_token := tok;
    tok

  let return_id tok =
    last_token := tok;
    tok

  let return_control lexbuf =
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
      pos_cnum = lexbuf.lex_curr_p.pos_cnum - 1 };
    lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1

  let return_shell tok =
    last_token := tok;
    shellmode_on := false;
    tok


  let is_transposable () = match !last_token with
    | ID _ | RBRACK | RBRACE | VARINT _ | VARFLOAT _
    | RPAREN | NUM _ | BOOLTRUE | BOOLFALSE | QUOTE -> true
    | _ -> false

  let is_EOL () = match !last_token with
    | EOL -> true
    | _ -> false

  let is_SOF () = match !last_token with
    | SOF -> true
    | _ -> false

  let is_plus () = match !last_token with
    | PLUS -> true
    | _ -> false

  let is_comma () = match !last_token with
    | COMMA -> true
    | _ -> false

  let in_matrix () =
    !matrix_level <> 0

  let set_last_token_spaces () =
    if (in_matrix () & (!last_token = COMMA || !last_token = PLUS)) ||
       (!last_token = EOL  || !last_token = SOF)
    then ()
    else
      last_token := SPACES
      (* if !matrix_level = 0  *)
      (* then last_token := EOF *)
      (* else () *)

  let make_error_string lexbuf =
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = "at line " ^ (string_of_int curr.Lexing.pos_lnum) in
    let cnum = ", chararacter " ^ (string_of_int (curr.Lexing.pos_cnum - curr.Lexing.pos_bol - 1)) in
    line ^ cnum ^ "."^(!str)


 let newline_lex lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let end_cmt lexbuf =
    lexbuf.lex_curr_pos <- lexbuf.lex_start_pos;
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
      pos_cnum = lexbuf.lex_start_p.pos_cnum;
    }

  (** xd-y -> xe-y
      xD-y -> xe-y **)
  let convert_scientific_notation str =
    let s = String.copy str in
    for i = 0 to String.length s - 1 do
      match s.[i] with
      'd' | 'D' -> s.[i] <- 'e'
      | _ -> ()
    done;
    s

  let warning_only_scila5 msg lexbuf =
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = (curr.Lexing.pos_cnum - curr.Lexing.pos_bol - 1) in
    Printf.printf "Warning : %s at line %i, character %i." msg line cnum


(* Convertion to UTF32LE *)
  let unicode_to_utf32le u =
    let c0 = (u lsr 24) land 255 and
        c1 = (u lsr 16) land 255 and
        c2 = (u lsr 8) land 255 and
        c3 = u land 255 in
    let utf32 = String.make 4 '0' in
    utf32.[0] <- Char.chr c3;
    utf32.[1] <- Char.chr c2;
    utf32.[2] <- Char.chr c1;
    utf32.[3] <- Char.chr c0;
    utf32

  let utf_8_normalize s =
    let b = Buffer.create (String.length s * 3) in
    (* let n = Uunf.create nf in *)
    let rec add v = match (* Uunf.add n *) v with
      | `Uchar u -> Buffer.add_string b (unicode_to_utf32le u); add `Await
      | _ -> ()
    in
    let add_uchar _ _ = function
      | `Malformed _ -> add (`Uchar Uutf.u_rep)
      | `Uchar c as u -> add u
    in
    Uutf.String.fold_utf_8 add_uchar () s; add `End; Buffer.contents b


}

let spaces    = [' ' '\t']


let newline   = ('\010' | '\013' | "\013\010")
let blankline = spaces+ newline
let empty     = spaces | [','';']
let emptyline = newline empty+ newline
let next      = ".." | "..."

let integer   = ['0'-'9']+
let number    = ['0'-'9']+(".")['0'-'9']*
let little    = (".")['0'-'9']+

let mantise   = little | integer | number
let exposant  = ['+''-']? integer
let floating  =  mantise ['d''e''D''E'] exposant

let utf2  = ['\xC2'-'\xDF']['\x80'-'\xBF']

let utf31 = ['\xE0']['\xA0'-'\xBF']['\x80'-'\xBF']
let utf32 = ['\xE1'-'\xEC']['\x80'-'\xBF']['\x80'-'\xBF']
let utf33 = ['\xED']['\x80'-'\x9F']['\x80'-'\xBF']
let utf34 = ['\xEE'-'\xEF']['\x80'-'\xBF']['\x80'-'\xBF']
let utf41 = ['\xF0']['\x90'-'\xBF']['\x80'-'\xBF']['\x80'-'\xBF']
let utf42 = ['\xF1'-'\xF3']['\x80'-'\xBF']['\x80'-'\xBF']['\x80'-'\xBF']
let utf43 = ['\xF4']['\x80'-'\x8F']['\x80'-'\xBF']['\x80'-'\xBF']

let utf3 = utf31 | utf32 | utf33 | utf34
let utf4 = utf41 | utf42 | utf43

let utf  =  utf2 | utf3 | utf4

let id1  = ['a'-'z''A'-'Z''_''%''#''?''$''!'] | utf
let id2  = ['a'-'z''A'-'Z''_''0'-'9''#''?''$''!'] | utf
let id   = id1 id2*
(* let id   = ['a'-'z''A'-'Z''_''%''#''?''$']['a'-'z''A'-'Z''_''0'-'9''#''?''$'] * *)

let boolnot    = "@" | "~"
let booltrue   = "%t" | "%T"
let boolfalse  = "%f" | "%F"
let booland    = "&"
let boolandand = "&&"
let boolor     = "|"
let booloror   = "||"

let lbrack = "["
let rbrack = "]"

let lparen = "("
let rparen = ")"

let lbrace = "{"
let rbrace = "}"

let dollar = "$"

let semicolon = ";"
let comma     = ","
let colon     = ":"

let startlinecomment  = "//"
let startblockcomment = "/*"
let endblockcomment   = "*/"

let dquote = "\""
let quote  = "'"

let dot        = "."
let dotquote   = ".'"
let dottimes   = ".*"
let dotrdivide = "./"
let dotldivide = ".\\"
let dotpower   = ".^" | ".**"

let plus    = "+"
let minus   = "-"
let rdivide = "/"
let ldivide = "\\"
let times   = "*"
let power   = "^" | "**"

let equal        = "=="
let notequal     = "~=" | "@=" | "<>"
let lowerthan    = "<"
let greaterthan	 = ">"
let lowerequal   = "<="
let greaterequal = ">="

let krontimes   = ".*."
let kronrdivide = "./."
let kronldivide = ".\\."

let controltimes   = "*." [^'0'-'9''.']
let controlrdivide = "/." [^'0'-'9''.']
let controlldivide = "\\." [^'0'-'9''.']

let wierd_op_version5 = "`--"

let lb = next spaces*

let shellmode_arg = [^ ' ''\t''\r''\n'','';''\'''\"' '(' '=' '/' '+' '|' '&'][^ '\t''\r''\n'','';''\'''\"'' ']*

let assign = "="

rule token = parse
  | spaces                       { set_last_token_spaces ();
                                   if !shellmode_on
                                   then shellmode lexbuf
                                   else token lexbuf }
  | blankline                    { newline_lex lexbuf;
                                   if !shellmode_on then shellmode_on := false;
                                   if (is_EOL ()) then token lexbuf else return_token EOL }
  | newline                      { newline_lex lexbuf;
                                   if !shellmode_on then shellmode_on := false;
                                   if (is_EOL ()) then token lexbuf else return_token EOL }
  | emptyline                    { newline_lex lexbuf;
                                   newline_lex lexbuf;
                                   if !shellmode_on then shellmode_on := false;
                                   if (is_EOL ()) then token lexbuf else return_token EOL }
  | startlinecomment             { str_cmt := ""; comment lexbuf }
  | startblockcomment            { str_cmt := ""; commentblock lexbuf }
  | dquote                       { str := ""; doublestr lexbuf }
  | quote                        { if (is_transposable ())
                                   then return_token QUOTE
                                   else begin str := ""; simplestr lexbuf end }
  | "if"                         { if not (in_matrix ())
                                   then return_token IF
                                   else return_token (ID ("if")) }
  | "then"                       { if not (in_matrix ())
                                   then return_token THEN
                                   else return_token (ID ("then")) }
  | "else"                       { if not (in_matrix ())
                                   then return_token ELSE
                                   else return_token (ID ("else")) }
  | "elseif"                     { if not (in_matrix ())
                                   then return_token ELSEIF
                                   else return_token (ID ("elseif")) }
  | "end"                        { if not (in_matrix ())
                                   then return_token END
                                   else return_token (ID ("end")) }
  | "select"                     { if not (in_matrix ())
                                   then return_token SELECT
                                   else return_token (ID ("select")) }
  | "switch"                     { if not (in_matrix ())
                                   then return_token SWITCH
                                   else return_token (ID ("switch")) }
  | "otherwise"                  { if not (in_matrix ())
                                   then return_token OTHERWISE
                                   else return_token (ID ("otherwise")) }
  | "case"                       { if not (in_matrix ())
                                   then return_token CASE
                                   else return_token (ID ("case")) }
  | "while"                      { if not (in_matrix ())
                                   then return_token WHILE
                                   else return_token (ID ("while")) }
  | "do"                         { if not (in_matrix ())
                                   then return_token DO
                                   else return_token (ID ("do")) }
  | "try"                        { if not (in_matrix ())
                                   then return_token TRY
                                   else return_token (ID ("try")) }
  | "catch"                      { if not (in_matrix ())
                                   then return_token CATCH
                                   else return_token (ID ("catch")) }
  | "return"                     { return_token RETURN } (* return in matrix ?? *)
  | "break"                      { if not (in_matrix ())
                                   then return_token BREAK
                                   else return_token (ID ("break")) }
  | "continue"                   { if not (in_matrix ())
                                   then return_token CONTINUE
                                   else return_token (ID ("continue")) }
  | assign                       { return_token ASSIGN }
  | "for"                        { if not (in_matrix ())
                                   then return_token FOR
                                   else return_token (ID ("for")) }
  | "hidden"                     { if not (in_matrix ())
                                   then return_token HIDDEN
                                   else return_token (ID ("hidden")) }
  | "function"                   { if not (in_matrix ())
                                   then return_token FUNCTION
                                   else return_token (ID ("function")) }
  | "#function"                  { if not (in_matrix ())
                                   then return_token HIDDENFUNCTION
                                   else return_token (ID ("hiddenfunction")) }
  | "endfunction"                { if not (in_matrix ())
                                   then return_token ENDFUNCTION
                                   else return_token (ID ("endfunction")) }
  | dot                          { return_token DOT }
  | dotquote                     { return_token DOTQUOTE }
  | lb newline                   { newline_lex lexbuf; token lexbuf
                                   (* if is_plus () || is_comma () *)
                                   (* then token lexbuf *)
                                   (* else *)
                                   (*   if !matrix_level > 0  *)
                                   (*   then return_token EOL  *)
                                   (*   else token lexbuf *) }
  (* | lb newline spaces* plus      { return_token PLUS } *)
  (* | lb newline spaces* minus     { return_token MINUS } *)
  (* | lb newline spaces* times     { return_token TIMES } *)
  (* | lb newline spaces* ldivide   { return_token LDIVIDE } *)
  (* | lb newline spaces* rdivide   { return_token RDIVIDE } *)
    (* We need those for 'op1 linebreak operator op2' cases *)
  | lb startlinecomment          { discardcomment lexbuf }
  | next                         { return_token LINEBREAK }
  | plus                         { return_token PLUS }
  | minus                        { return_token MINUS }
  | rdivide                      { return_token RDIVIDE }
  | dotrdivide                   { return_token DOTRDIVIDE }
  | controlrdivide               { return_control lexbuf; return_token CONTROLRDIVIDE }
  | kronrdivide                  { return_token KRONRDIVIDE }
  | ldivide                      { return_token LDIVIDE }
  | dotldivide                   { return_token DOTLDIVIDE }
  | controlldivide               { return_control lexbuf; return_token CONTROLLDIVIDE }
  | kronldivide                  { return_token KRONLDIVIDE }
  | times                        { return_token TIMES }
  | dottimes                     { return_token DOTTIMES }
  | controltimes                 { return_control lexbuf; return_token CONTROLTIMES }
  | krontimes                    { return_token KRONTIMES }
  | power                        { return_token POWER }
  | dotpower                     { return_token DOTPOWER }
  | equal                        { return_token EQ }
  | notequal                     { return_token NE }
  | lowerthan                    { return_token LT }
  | greaterthan                  { return_token GT }
  | lowerequal                   { return_token LE }
  | greaterequal                 { return_token GE }
  | comma                        { return_token COMMA }
  | semicolon                    { if !shellmode_on then shellmode_on := false;
                                   return_token SEMI }
  | colon                        { return_token COLON }
  | integer as inum              { let num = float_of_string inum in
                                   (* Printf.printf "varint[%f]\n" num; *)
                                   return_token (VARINT num) }
  | number as nnum               { let num = float_of_string nnum in
                                   (* Printf.printf "num[%s = %.15f]\n" nnum num; *)
                                   return_token (NUM num) }
  | little as lnum               { let num = float_of_string lnum in
                                   (* Printf.printf "little[%f]\n" num; *)
                                   return_token (NUM num) }
  | floating as float            { let f = (float_of_string (convert_scientific_notation float)) in
                                   (* Printf.printf "float[%s = %f]\n" float f; *)
                                   NUM f }
  | lparen                       { if !shellmode_on then shellmode_on := false; return_token LPAREN }
  | rparen                       { return_token RPAREN }
  | lbrace                       { incr matrix_level; return_token LBRACE }
  | rbrace                       { decr matrix_level; return_token RBRACE }
  | lbrack                       { incr matrix_level; return_token LBRACK }
  | rbrack                       { decr matrix_level; return_token RBRACK }
  | dollar                       { return_token DOLLAR }
  | boolnot                      { return_token NOT }
  | booltrue                     { return_token BOOLTRUE }
  | boolfalse                    { return_token BOOLFALSE }
  | booland                      { return_token AND }
  | boolandand                   { return_token ANDAND }
  | boolor                       { return_token OR }
  | booloror                     { return_token OROR }
  | id as ident                  { if (not (in_matrix ())) & (is_EOL () || is_SOF ())
                                   then shellmode_on := true;
                                   let id8 = (* utf_8_normalize *) ident in
                                   return_id (ID id8)
                                   (* (\*shellmode lexbuf*\) *)
                                   (*   else return_token (ID ident) *)
                                   (* else return_token (ID ident) *) }
  | eof                          { return_token EOF }
  | _                            { raise (Lex_err ("Error : Unknow character ")) }

and discardcomment = parse
  | newline                      { token lexbuf }
  | eof                          { return_token EOF }
  | _                            { discardcomment lexbuf }

and comment = parse
  | newline                      { end_cmt lexbuf; return_token (COMMENT !str_cmt) }
  | eof                          { return_token (COMMENT !str_cmt) }
  | _ as c                       { str_cmt := !str_cmt^(String.make 1 c); comment lexbuf }

and commentblock = parse
  | endblockcomment              { return_token (COMMENT !str_cmt) }
  | newline                      { newline_lex lexbuf; str_cmt := !str_cmt^(String.make 1 '\n'); commentblock lexbuf }
  | eof                          { return_token (COMMENT !str_cmt) }
  | _ as c                       { str_cmt := !str_cmt^(String.make 1 c); commentblock lexbuf }

and doublestr = parse
  | dquote                       { let s = utf_8_normalize !str in
                                   return_token (STR s) }
  | dquote dquote                { str := !str^"\"\""; doublestr lexbuf }
  | dquote quote                 { str := !str^"\"\'"; doublestr lexbuf }
  | quote dquote                 { str := !str^"\'\""; doublestr lexbuf }
  | quote quote                  { str := !str^"\'\'"; doublestr lexbuf }
  | quote                        { let msg = "Heterogeneous string, starting with \" and ending with \' only allowed in scilab 5" in
                                   warning_only_scila5 msg lexbuf;
                                   let s = utf_8_normalize !str in
                                   return_token (STR s) }
  | next newline                 { newline_lex lexbuf; doublestr lexbuf }
  | newline                      { raise (Err_str "Error : unexpected newline in a string ") }
  | eof                          { raise (Err_str "Error : unexpected end of file in a string ") }
  | _ as c                       { str := !str^(String.make 1 c); doublestr lexbuf }

and simplestr = parse
  | quote                        { let s = utf_8_normalize !str in
                                   return_token (STR s) }
  | dquote dquote                { str := !str^"\"\""; simplestr lexbuf }
  | dquote quote                 { str := !str^"\"\'"; simplestr lexbuf }
  | quote dquote                 { str := !str^"\'\""; simplestr lexbuf }
  | quote quote                  { str := !str^"\'\'"; simplestr lexbuf }
  | dquote                       { let msg = "Heterogeneous string, starting with \' and ending with \" only allowed in scilab 5" in
                                   warning_only_scila5 msg lexbuf;
                                   let s = utf_8_normalize !str in
                                   return_token (STR s) }
  | next newline                 { newline_lex lexbuf; simplestr lexbuf }
  | newline                      { raise (Err_str "Error : unexpected newline in a string ") }
  | eof                          { raise (Err_str "Error : unexpected end of file in a string ") }
  | utf as u                     { str := !str ^ u; simplestr lexbuf }
  | _ as c                       { str := !str^(Char.escaped c); simplestr lexbuf }

and shellmode = parse
  | spaces+                      { shellmode lexbuf }
  | startlinecomment             { shellmode_on := false;
                                   comment lexbuf }
  | plus                         { return_shell PLUS }
  | minus                        { return_shell MINUS }
  | rdivide                      { return_shell RDIVIDE }
  | dotrdivide                   { return_shell DOTRDIVIDE }
  | controlrdivide               { return_control lexbuf; return_shell CONTROLRDIVIDE }
  | kronrdivide                  { return_shell KRONRDIVIDE }
  | ldivide                      { return_shell LDIVIDE }
  | dotldivide                   { return_shell DOTLDIVIDE }
  | controlldivide               { return_control lexbuf; return_shell CONTROLLDIVIDE }
  | kronldivide                  { return_shell KRONLDIVIDE }
  | times                        { return_shell TIMES }
  | dottimes                     { return_shell DOTTIMES }
  | controltimes                 { return_control lexbuf; return_shell CONTROLTIMES }
  | krontimes                    { return_shell KRONTIMES }
  | power                        { return_shell POWER }
  | dotpower                     { return_shell DOTPOWER }
  | equal                        { return_shell EQ }
  | notequal                     { return_shell NE }
  | lowerthan                    { return_shell LT }
  | greaterthan                  { return_shell GT }
  | lowerequal                   { return_shell LE }
  | greaterequal                 { return_shell GE }
  | semicolon                    { return_shell SEMI }
  | comma                        { return_shell COMMA }
  | assign                       { return_shell ASSIGN }
  | lparen                       { return_shell LPAREN }
  | lowerthan                    { return_shell LT }
  | greaterthan                  { return_shell GT }
  | boolnot                      { return_shell NOT }
  | booland                      { return_shell AND }
  | boolandand                   { return_shell ANDAND }
  | boolor                       { return_shell OR }
  | booloror                     { return_shell OROR }
  | quote                        { shellmode_on := false;
                                   if (is_transposable ())
                                   then return_shell QUOTE
                                   else begin str := ""; simplestr lexbuf end }
  | dquote                       { shellmode_on := false; str := ""; doublestr lexbuf }
  | shellmode_arg as arg         { return_token (STR arg) }
  | eof                          { return_shell EOF }

(* and matrix = parse *)
(*   | spaces+ *)
(*       { Printf.printf " "; *)
(*         matrix lexbuf} *)
(*   | integer as inum *)
(*       { let num = int_of_string inum in *)
(*         Printf.printf "%d" num; *)
(*         matrix lexbuf} *)
(*   | number as nnum *)
(*       { let num = float_of_string nnum in *)
(*         Printf.printf "%f" num; *)
(*         matrix lexbuf} *)
(*   | little as lnum *)
(*       { let num = float_of_string lnum in *)
(*         Printf.printf "%f" num; *)
(*         matrix lexbuf} *)
(*   | rbrack *)
(*       {token lexbuf} *)


















