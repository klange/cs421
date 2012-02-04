type token =
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | STRING of (string)
  | IDENT of (string)
  | OPCOM of ((int*int))
  | CLCOM of ((int*int))
  | SCLCOM of ((int*int))
  | DBLSEMI
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | DPLUS
  | DMINUS
  | DTIMES
  | DDIV
  | CARAT
  | EXP
  | LT
  | GEQ
  | LEQ
  | GT
  | EQUALS
  | AND
  | OR
  | PIPE
  | ARROW
  | DCOLON
  | LET
  | REC
  | SEMI
  | IN
  | IF
  | THEN
  | ELSE
  | FUN
  | LBRAC
  | RBRAC
  | LPAREN
  | RPAREN
  | COMMA
  | UNIT
  | ERROR
  | EOF
  | TRY
  | WITH
  | UNDERSCORE
  | RAISE
  | HEAD
  | TAIL
  | PRINT
  | NEG
  | FST
  | SND

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Mp8common.toplvl
