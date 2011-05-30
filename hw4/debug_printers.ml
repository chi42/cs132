open Vtypes

let bb_print n = 
  match n with
  | Cl_and_instr (cl,instr) -> 
    (match cl with 
    | Code_label cl_str -> Printf.printf "\t%s\n" cl_str
    | _ -> ()) 
  | Instr _ -> Printf.printf "\tINSTR\n" 
  | _ -> ()  

let rec print_type x =
  match x with 
  | Dummy _ -> Printf.printf "dummy\n" 

  | Cl_and_instr _ -> Printf.printf "Cl_and_instr\n" 
  | Neg_digits a -> Printf.printf "Neg_digits: %s\n" a
  | Digits a -> Printf.printf "Digits: %s\n" a
  | Plain_ident a -> Printf.printf "Plain_ident: %s \n" a
  | Reg_ident a -> Printf.printf "Reg_ident: %s\n" a
  | Code_label_ident a -> Printf.printf "Code_label_ident: %s\n" a
  | Label_ref_ident a -> Printf.printf "Label_ref_ident:%s \n" a
  | Program _ -> Printf.printf "Program\n" 
  | Data_segment _ -> Printf.printf "Data_segment\n" 
  | Function _ -> Printf.printf "Function\n" 
  | Func_cl_eol _ -> Printf.printf "Func_cl_eol\n" 
  | Func_cl_instr _ -> Printf.printf "Func_cl_instr\n" 
  | Func_instr _ -> Printf.printf "Func_instr\n" 
  | Instr i -> 
    Printf.printf "Instr: ";
    print_type i; 
  | Mem_read _ -> Printf.printf "Mem_read\n" 
  | Mem_write _ -> Printf.printf "Mem_write\n" 
  | Mem_ref _ -> Printf.printf "Mem_ref\n" 
  | Stack_mem_ref _ -> Printf.printf "Stack_mem_ref\n" 
  | Global_mem_ref _ -> Printf.printf "Global_mem_ref\n" 
  | Assign _ -> Printf.printf "Assign\n" 
  | Branch _ -> Printf.printf "Branch\n" 
  | Goto _ -> Printf.printf "Goto\n" 
  | Call _ -> Printf.printf "Call\n" 
  | Builtin _ -> Printf.printf "Builtin\n" 
  | Return _ -> Printf.printf "Return\n" 
  | Data_value _ -> Printf.printf "Data_value\n" 
  | Operand _ -> Printf.printf "Operand\n" 
  | Label_ref _ -> Printf.printf "Label_ref\n" 
  | Code_addr _ -> Printf.printf "Code_addr\n" 
  | Data_addr _ -> Printf.printf "Data_addr\n" 
  | Func_addr _ -> Printf.printf "Func_addr\n" 
  | Code_label_ref _ -> Printf.printf "Code_label_ref\n" 
  | Data_segment_ref _ -> Printf.printf "Data_segment_ref\n" 
  | Func_ref _ -> Printf.printf "Func_ref\n" 
  | Var_ref _ -> Printf.printf "Var_ref\n" 
  | Var_ref_no_reg _ -> Printf.printf "Var_ref_no_reg\n" 
  | Operand_no_reg _ -> Printf.printf "Operand_no_reg\n" 
  | Lit_int _ -> Printf.printf "Lit_int\n" 
  | Lit_str _ -> Printf.printf "Lit_str\n" 
  | Ident _ -> Printf.printf "Ident\n" 
  | Code_label _ -> Printf.printf "Code_label\n" 
  | Eol _ -> Printf.printf "Eol\n" 
  | T_const -> Printf.printf "T_const\n"
  | T_var -> Printf.printf "T_var\n"
  | T_in -> Printf.printf "T_in\n"
  | T_out -> Printf.printf "T_out\n"
  | T_local -> Printf.printf "T_local\n"
  | T_if -> Printf.printf "T_if\n"
  | T_if_not -> Printf.printf "T_if_not\n"
  | T_func -> Printf.printf "T_func\n"
  | T_goto -> Printf.printf "T_goto\n"
  | T_ret  -> Printf.printf "T_ret \n"
  | T_call -> Printf.printf "T_call\n"
  | T_plus -> Printf.printf "T_plus\n"
  | T_minus -> Printf.printf "T_minus\n"



let bb_print n = 
  match n with
  | Cl_and_instr (cl,instr) -> 
    (match cl with 
    | Code_label cl_str -> Printf.printf "\t%s\n" cl_str
    | _ -> ()) 
  | Instr _ -> Printf.printf "\tINSTR\n" 
  | _ -> ()  

