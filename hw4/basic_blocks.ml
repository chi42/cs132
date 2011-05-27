open Vtypes
open Exceptions

type b_block = 
 | Block of t_nodes list   

let f_builder x = 
  let rec blockerize lis
  
  in 
    match x with name,vars,_,cl_and_i -> 
      name,vars,(blockerize cl_and_i)
    
let rec split_data_and_funcs funcs data_segs lis =  
  match lis with 
  | [] -> []
  | (Function (a,b,c,d))::tl -> 
    split_data_and_funcs (funcs@[(f_builder (a,b,c,d))]) data_segs tl
  | (Data_segment (a,b,c))::tl -> 
    split_data_and_funcs funcs (data_segs@[(a,b,c)]) tl 
  | _ -> raise (Block_error "bad token, split_data_and_funcs")


let block_builder prog = 
  match prog with  
  | Program lis -> split_data_and_funcs [] [] lis
  | _ -> raise (Block_error "bad token, block_builder") 
    

