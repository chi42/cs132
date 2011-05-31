open Vtypes
open Exceptions
open Debug_printers

(* contains: 
 * list of instrs 
 * list of uses
 * list of defines *)
type bblock =
 Bblock of (t_nodes list) * (string list) * (string list)   

(* contains, current block,
 * list of references to in blocks
 * list of references to out blocks *)
type cfg_node = 
  Cfg_node of Bblock * ((bblock ref) list) * ((bblock ref) list)

(* these are helpers *)

let plain_ident_only lis ele = 
  match ele with 
  | Plain_ident x -> 
    (match (List.exists ((=) x) lis) with 
    | true -> lis
    | false -> x::lis)
  | _ -> lis 

(* not tail recursive *)
let rec iter_and_cons f lis = 
  match lis with
  | [] -> []
  | hd::tl -> 
    (f hd)@(iter_and_cons f tl)

(* now the fun begins *)

let block_build_single_func x = 
  let rec blockerize instrs so_far =
    match instrs with 
    | [] -> instrs,so_far
    | hd::tl -> 
      match hd with 
      (* we'll seperate cl_and_instr into
       * its code_label and instr components *)
      | Cl_and_instr (l,i) ->
        [l;i]@instrs,so_far 
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
        | _ -> block_all n_instrs (blk::blocks)
  in  
    match x with 
    | Function (name,vars,_,cl_and_i) -> 
      (match name with 
       | Plain_ident str_name -> 
           Printf.printf "func name: %s\n" str_name ;
         [str_name,vars,(List.rev (block_all cl_and_i []))]
       |_ -> raise (Block_error "block_builder 2"))
    | Data_segment _ -> [] 
    | _ -> raise (Block_error "bad token, block_builder")

let rec operand_traveler x = 
  match x with
    (* Label_ref_ident *)
  | Operand (Label_ref f0) -> Label_ref f0 
    (* don't really want lit_int and lit_str, but strip it out later *)
  | Operand (Lit_int f0) -> Lit_int f0
  | Operand (Lit_str f0) -> Lit_str f0
    (* Reg_ident *)
  | Operand (Var_ref f0) -> var_ref_traveler (Var_ref f0)  
  | _ -> raise (Block_error "operand_traveler")
  
(* not a bug, we really do just return x=(Plain_ident "str") *)
and ident_traveler x = 
  match x with 
  | Plain_ident f0 -> x
  | _ -> raise (Block_error "ident_traveler2")

and var_ref_traveler x = 
  match x with 
  | Var_ref (Plain_ident f0) -> ident_traveler (Plain_ident f0)
  | Var_ref (Reg_ident f0) -> (Reg_ident f0) 
  | _ -> raise (Block_error "var_ref_traveler")

and mem_ref_traveler x =
  match x with 
  | Mem_ref (Global_mem_ref (f0,f1)) -> 
    global_mem_ref_traveler (Global_mem_ref (f0,f1))
  | _ -> raise (Block_error "mem_ref_traveler")

and global_mem_ref_traveler x =
  match x with 
  | Global_mem_ref (f0,_) -> data_addr_traveler f0
  | _ -> raise (Block_error "global_ref_traveler")

and data_addr_traveler x =
  match x with 
  | Data_addr (Data_segment_ref (Label_ref_ident f0)) ->
    Label_ref_ident f0 
  | Data_addr f0 -> var_ref_traveler f0  
  | _ -> raise (Block_error "data_addr_traveler")

and code_label_ref_traveler x = 
  match x with 
  | Code_label_ref (Label_ref_ident f0) -> Label_ref_ident f0  
  | _ -> raise (Block_error "code_label_ref_traveler")

and code_addr_traveler x = 
  match x with 
  | Code_label_ref _ -> code_label_ref_traveler x
  | Var_ref _  -> var_ref_traveler x
  | _ -> raise (Block_error "code_addr_traveler")

and func_addr_traveler x = 
  match x with
  | Func_addr (Label_ref_ident f0) -> Label_ref_ident f0
  | Func_addr (Var_ref f0) -> var_ref_traveler (Var_ref f0)
  | _ -> raise (Block_error "func_addr_traveler")

let use_def_single lis = 
  let rec defines instrs = 
    match instrs with
    | [] -> []
    | hd::tl -> 
      match hd with 
      | Instr ins -> 
        (match ins with 
        | Return _ -> defines tl  
        | Mem_read (f0,_) -> (var_ref_traveler f0)::(defines tl)
        | Mem_write _ -> defines tl  
        | Assign (f0,_) -> (var_ref_traveler f0)::(defines tl)
        | Branch _ -> defines tl  
        | Goto _ -> defines tl  
        | Call ([],_,_) -> defines tl  
        | Call ([f0],_,_) -> (var_ref_traveler f0)::(defines tl)
        | Builtin ([],_,_) -> defines tl
        | Builtin ([f0],_,_) -> (var_ref_traveler f0)::(defines tl)
        | _ -> raise (Block_error "defines"))
      | Code_label _ -> defines tl 
      | _ -> raise (Block_error "defines2")
  in
  let rec uses instrs = 
    match instrs with 
    | [] -> [] 
    | hd::tl -> 
      match hd with 
      | Instr ins -> 
        (match ins with  
        | Return [] -> uses tl  
        | Return [f0] -> (operand_traveler f0)::(uses tl)
        | Mem_read (_,f1) -> (mem_ref_traveler f1)::(uses tl)
        | Mem_write (f0,f1) -> 
          [mem_ref_traveler f0; operand_traveler f1]@(uses tl) 
        | Assign (_,f1) -> (operand_traveler f1)::(uses tl)
        | Branch (_,f1,_) -> (operand_traveler f1)::(uses tl)
        | Goto f0 -> (code_addr_traveler f0)::(uses tl)
        | Call (_,f1,f2) -> 
          (func_addr_traveler f1)::((List.map (operand_traveler) f2)@(uses tl))
        | Builtin (_,_,f2) -> (List.map (operand_traveler) f2)@(uses tl)
        | _ -> raise (Block_error "defines"))
      | Code_label _ -> uses tl 
      | _ -> raise (Block_error "uses")
  in
    let u,d = (uses lis),(defines lis) in 
      let u_clean = List.fold_left plain_ident_only [] u in
      let d_clean = List.fold_left plain_ident_only [] d in
      
      Printf.printf "--USES \n";
      List.iter (Printf.printf "%s\n") u_clean;

      Printf.printf "--DEFINES \n";
      List.iter (Printf.printf "%s\n") d_clean;
      Bblock (lis,u_clean,d_clean)
    (*Bblock (lis,[],(defines lis))*)

let use_and_define func_lis =
  let use_def_func b = 
    match b with 
    | f_name,f_args,blks -> 
      Printf.printf "FUNC %s\n" f_name;
      let finished_blks = List.map use_def_single blks in
        f_name,f_args,finished_blks
  in
    Printf.printf "BEGIN\n";
    List.map use_def_func func_lis        

let rec find_label blks = 
  match blks with
  | [] -> raise (Block_error "label not defined")
  | hd::tl ->  
    match hd with 
    | Code_label x -> 

let cfg_create blk_lis = 
  match blk_lis with
  | [] -> 
  | Bblock (instrs,u,d)::tl -> 
    match (list_last instrs) 
    | Branch _ ->  
    | Goto _ -> 
    | Return _ -> 
    | Builtin (_,(Plain_ident "Error"),_) -> tl,(so_far@[hd])
    | _ ->  

| Code_label _ -> instrs,so_far
      | Instr i -> 
        (match i with 
        | Branch _ -> tl,(so_far@[hd])
        | Goto _ -> tl,(so_far@[hd])
        | Return _ -> tl,(so_far@[hd])
        | Builtin (_,(Plain_ident "Error"),_) -> tl,(so_far@[hd])
        | _ -> blockerize tl (so_far@[hd]) )
      | _ -> raise
type bblock =
 Bblock of (t_nodes list) * (string list) * (string list)   

let block_builder lis =
  use_and_define (iter_and_cons block_build_single_func lis)
     
