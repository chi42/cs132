open Vtypes
open Exceptions
open Debug_printers


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
        | Goto _ -> instrs,so_far 
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
        List.iter bb_print blk;
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
    




