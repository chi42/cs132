open Vparser 
open Vlexar 
open Exceptions 
open Vtypes
open Basic_blocks

let main () = 
  try 
    (* lex and parse, build AST *)
    match (Vparser.program Vlexar.token (Lexing.from_channel stdin)) with _ -> 
      Printf.printf "Success!\n";
      exit 0
  with _ ->
    Printf.printf "FAIL!\n";
    exit 1
  
let _ = Printexc.print main ()

