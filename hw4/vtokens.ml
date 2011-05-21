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
  
(* for building the parse tree *)
type t_nodes = 
  | 
type t_program = 
  | Program of (t_function * t_data_segment) list
and t_data_segment = 
  | Data_segment of token * t_ident * (t_data_value list) 
and t_function =
  | Function of t_ident * (t_var_ref_no_reg list) * token * token * token *
    t_codelabel * (t_instr list) * t_instr
  | 
