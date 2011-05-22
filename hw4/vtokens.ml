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

type end_of_file = EOF 
  
(* for building the parse tree *)
type t_nodes = 
  | Program of (t_nodes list) * end_of_file
  | Data_segment of t_nodes * t_nodes * (t_nodes list)
  | Function of t_nodes * (t_nodes list) * ((t_nodes * t_nodes * t_nodes) list) * (t_nodes   
  | Instr of t_nodes
  | Mem_read of t_nodes * t_nodes
  | Mem_write of t_nodes * t_nodes
  | Mem_ref of t_nodes 
  | Stack_mem_ref of
  | Global_mem_ref of
  | Assign of
  | Branch of 
  | Goto of
  | Call of
  | Builtin of
  | Return of
  | Data_value of
  | Operand of
  | Label_ref of
  | Code_addr of
  | Data_addr of
  | Func_addr of
  | Code_label_ref of
  | Data_segment_ref of
  | Func_ref of
  | Var_ref of 
  | Var_ref_no_reg of
  | Operand_no_reg of
  | Lit_int of
  | Lit_str of
  | Ident of
  | Code_label of
  | Eol of 

type t_program = 
  | Program of (t_function * t_data_segment) list
and t_data_segment = 
  | Data_segment of token * t_ident * (t_data_value list) 
and t_function =
  | Function of t_ident * (t_var_ref_no_reg list) * token * token * token *
    t_codelabel * (t_instr list) * t_instr
  | 
