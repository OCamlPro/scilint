type token =
  | SOF
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | DOLLAR
  | SPACES
  | COMMA
  | EOL
  | SEMI
  | IF
  | THEN
  | ELSE
  | ELSEIF
  | END
  | WHILE
  | DO
  | COLON
  | ASSIGN
  | ID of (string)
  | FOR
  | FUNCTION
  | ENDFUNCTION
  | HIDDEN
  | HIDDENFUNCTION
  | PLUS
  | MINUS
  | RDIVIDE
  | LDIVIDE
  | TIMES
  | POWER
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE
  | SELECT
  | SWITCH
  | OTHERWISE
  | CASE
  | TRY
  | CATCH
  | RETURN
  | BREAK
  | CONTINUE
  | BOOLTRUE
  | BOOLFALSE
  | QUOTE
  | AND
  | ANDAND
  | NOT
  | DOT
  | DOTQUOTE
  | DOTTIMES
  | DOTLDIVIDE
  | DOTRDIVIDE
  | DOTPOWER
  | OR
  | OROR
  | KRONTIMES
  | CONTROLTIMES
  | CONTROLLDIVIDE
  | CONTROLRDIVIDE
  | LINEBREAK
  | KRONLDIVIDE
  | KRONRDIVIDE
  | WIERDOP
  | VARINT of (float)
  | VARFLOAT of (float)
  | NUM of (float)
  | COMMENT of (string)
  | STR of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ScilabAst.ast
