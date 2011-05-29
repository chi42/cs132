open Vtypes
open Exceptions
open Debug_printers

(* contains list of instrs list of uses list of defines *)
type bblock =
 Bblock of (t_nodes list) * (string list) * (string list)   


let block_builder x = 
  let rec blockerize instrs so_far =
    match instrs with 
    | [] -> instrs,so_far
    | hd::tl -> 
      match hd with 
      | Cl_and_instr _ -> instrs,so_far
      | Code_label _ -> instrs,so_far
      | Instr i -> 
        (match i with 
        | Branch _ -> tl,(so_far@[hd])
        | Goto _ -> tl,(so_far@[hd])
        | Return _ -> tl,(so_far@[hd])
        | Builtin (_,(Plain_ident "Error"),_) -> tl,(so_far@[hd])
        | _ -> blockerize tl (so_far@[hd]) )
      | _ -> raise (Block_error "bad token, blockerize")
  in 
  let rec block_all instrs blocks =
    match instrs with 
    | [] -> blocks 
    | i_hd::i_tl ->
      match (blockerize i_tl [i_hd]) with
      | n_instrs,blk ->
        Printf.printf "!NEW BLOCK!\n";
        List.iter print_type blk;
        match n_instrs with 
        | _ -> block_all n_instrs blocks@[blk]
  in  
    match x with 
    | Function (name,vars,_,cl_and_i) -> 
      (match name with 
       | Plain_ident ttemp -> Printf.printf "func name: %s\n" ttemp 
       |_ -> ()
      );
      [name,vars,(block_all cl_and_i [])]

    | Data_segment _ -> [] 
    | _ -> raise (Block_error "bad token, block_builder")
    

  let get_var_name x =  
    match x with 
    | Label_ref (Label_ref_ident v) -> [v]
    | Var_ref 
    | _ -> []

let get_string f0 -> 
  match f0 with 
  | Label_ref (Label_ref_ident v) -> [v]
  | Ident (Plain_ident v) -> [v] 
  | Reg_ident v -> [v]  
  | Lit_str _ -> []
  | Lit_int _ -> []
  | _ -> raise (Block_error "token in operand_travel")
let get_string_memref f0 -> 
  match f0 with 
  | Global_mem_ref ((),_) ->  

let use_and_define il = 
  let rec generate_use_def instrs use def = 
    match instrs with
    | [] -> use,def 
    | hd::tl -> 
      match hd with 
      | Return [Operand f0] -> get_string f0  
      | Return [] -> [] 
      | Mem_read (f0,f1) -> get_string f0 
      | Mem_write
      | Assign
      | Branch
      | Goto
      | Call
      | Builtin   
