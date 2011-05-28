open Vparser 
open Vlexar 
open Exceptions 
open Vtypes
open Basic_blocks

let main () = 
  (*try *)
    (* lex and parse, build AST *)
    let prog = (Vparser.program Vlexar.token (Lexing.from_channel stdin)) in
  
    (match prog with 
      | Program lis -> 
        (match List.map Basic_blocks.block_builder lis with _ ->  ())
      | _ -> ()
    );

      Printf.printf "Success!\n";
      exit 0
  (*with _ ->
    Printf.printf "FAIL!\n";
    exit 1*)
  
let _ = Printexc.print main ()

