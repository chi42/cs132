open Otree
open Table_visitor
open Checker_visitor
open Other_funcs

let main () = 
  try 
    (* lex and parse *)
    let tree = (Minijava.goal Minijava_lexar.token (Lexing.from_channel stdin)) in
      (* build tables of variables, methods, and classes, some basic checking *)
      (new table_building_visitor)#visit_g tree;
      (* check for cyclical inheritance and all valid inheritance *)
      inheritance_check ht_classes (Hashtbl.create 40);
      (* check everything else *)
      (new checker_visitor)#visit_g tree;

      Printf.printf "Program type checked successfully\n";
  with _ ->
    Printf.printf "Type error\n"
  
let _ = Printexc.print main ()

