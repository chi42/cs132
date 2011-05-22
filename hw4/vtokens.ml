(* for parsing and lexing *)
type token = 
  | CONST
  | VAR
  | FUNC
  | IN
  | OUT
  | LOCAL
  | EQUAL
  | PLUS
  | MINUS
  | IF
  | IF_NOT
  | GOTO
  | CALL
  | RET
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | EOL 
  | NEG_DIGITS of string
  | DIGITS of string
  | PLAIN_IDENT of string
  | REG_IDENT of string
  | CODE_LABEL_IDENT of string
  | LABEL_REF_IDENT of string
  | STR of string
  | EOF 



