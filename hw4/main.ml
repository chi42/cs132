open Vparser 
open Vlexar 
open Exceptions 
open Vtypes

let main () = 
  try 
    (* lex and parse, build AST *)
    let tree = (Vparser.program Vlexar.token (Lexing.from_channel stdin)) in
      Printf.printf "Success!\n";
      exit 0
  with _ ->
    Printf.printf "FAIL!\n";
    exit 1
  
let _ = Printexc.print main ()

