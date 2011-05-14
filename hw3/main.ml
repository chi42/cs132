open Minijava 
open Minijava_lexar
open Tree_types
open Table_builder
open Printers
open Ir_gen

let main () = 
  try 
    (* lex and parse, build AST *)
    let tree = (Minijava.goal Minijava_lexar.token (Lexing.from_channel stdin)) in
      (* build tables of variables, methods, and classes, some basic checking *)
      match (Table_builder.goal_travel tree) with
      | classes_ht,l_classes -> 
      (* build the virtual method tables and print *)
        let vmt_l = vmts_maker classes_ht l_classes in
          List.iter Printers.print_vmt vmt_l;
          (* build tables of local class variables *)
          let var_l = vars_maker classes_ht l_classes in
            (* print array allocation function *)
            match Printers.array_alloc_func 0 with _ -> ();
            let new_ht = Hashtbl.create 20 in
              table_rebuild var_l vmt_l new_ht classes_ht;
              (* generate vapor *)
              match (Ir_gen.goal_travel new_ht tree) with _ ->  ();

      exit 0
  with _ ->
    exit 1
  
let _ = Printexc.print main ()

