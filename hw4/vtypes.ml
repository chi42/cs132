(* for building the parse tree *)
type t_nodes = 
  | T_const
  | T_var
  | T_in
  | T_out
  | T_local
  | T_if
  | T_if0
  | T_func
  | T_goto
  | T_ret 
  | T_call

  | Cl_and_instr of t_nodes * t_nodes
  
  | Neg_digits of string
  | Digits of string
  | Plain_ident of string
  | Reg_ident of string
  | Code_label_ident of string
  | Label_ref_ident of string

  | Program of (t_nodes list) 
  | Data_segment of t_nodes * t_nodes * ((t_nodes list) list)

  | Function of t_nodes * (t_nodes list) * 
      ((t_nodes * t_nodes * t_nodes) list) * (t_nodes list)
  | Func_cl_eol of t_nodes
  | Func_cl_instr of t_nodes * t_nodes
  | Func_instr of t_nodes

  | Instr of t_nodes
  | Mem_read of t_nodes * t_nodes
  | Mem_write of t_nodes * t_nodes
  | Mem_ref of t_nodes 
  | Stack_mem_ref of t_nodes * t_nodes
  | Global_mem_ref of t_nodes * ((t_nodes * t_nodes) list)
  | Assign of t_nodes * t_nodes 
  | Branch of t_nodes * t_nodes * t_nodes 
  | Goto of t_nodes
  | Call of (t_nodes list) * t_nodes * (t_nodes list) 
  | Builtin of (t_nodes list) * t_nodes * (t_nodes list)
  | Return of t_nodes list
  | Data_value of t_nodes
  | Operand of t_nodes
  | Label_ref of t_nodes
  | Code_addr of t_nodes
  | Data_addr of t_nodes
  | Func_addr of t_nodes
  | Code_label_ref of t_nodes
  | Data_segment_ref of t_nodes
  | Func_ref of t_nodes
  | Var_ref of t_nodes
  | Var_ref_no_reg of t_nodes
  | Operand_no_reg of t_nodes
  | Lit_int of t_nodes
  | Lit_str of t_nodes
  | Ident of t_nodes
  | Code_label of t_nodes
  | Eol of t_nodes list
 
