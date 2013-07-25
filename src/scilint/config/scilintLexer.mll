{

  open ScilintParser
  open Lexing

  let str = ref ""
    
  let newline_lex lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
      
}

let spaces    = [' ' '\t']
let newline   = ('\010' | '\013' | "\013\010")

let lbrack = "["
let rbrack = "]"

let dquote = "\""
let quote  = "'"

let assign = "="

rule token = parse
  | spaces                       { token lexbuf }
  | newline                      { newline_lex lexbuf; token lexbuf }
  | "files"                      { FILES }
  | assign                       { EQ }
  | lbrack                       { LB }
  | rbrack                       { RB }
  | quote                        { str := ""; simplestr lexbuf}
  | dquote                       { str := ""; doublestr lexbuf}
  | _ as c                       { print_char c; token lexbuf }

and doublestr = parse
  | dquote                       { STR !str }
  | quote                        { failwith "quote" }
  | newline                      { failwith "Error : unexpected newline in a string " }
  | eof                          { failwith "Error : unexpected end of file in a string " }
  | _ as c                       { str := !str^(String.make 1 c); doublestr lexbuf }

and simplestr = parse
  | quote                        { STR !str }
  | dquote                       { failwith "dquote" }
  | newline                      { failwith "Error : unexpected newline in a string " }
  | eof                          { failwith "Error : unexpected end of file in a string "}
  | _ as c                       { str := !str^(Char.escaped c); simplestr lexbuf }
