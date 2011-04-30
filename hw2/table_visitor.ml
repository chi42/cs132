open Otree
open Other_funcs

let ht_classes = Hashtbl.create 20

class table_building_visitor =
  object  (self)
  inherit virt_visitor

  val mutable str = ""
  val mutable num = 0
  val mutable formal_params:(string list) = [] 
  val mutable cur_var_t = Hashtbl.create 10
  val mutable cur_meth_t = Hashtbl.create 10

  (* goal: main_class type_declaration-list *)
  method visit_g arg = 
    self#visit_mc arg#f0;
    loop self#visit_td arg#f1;
    ()
  (* main_class: identifier identifier var_declaration* statement* *)
  method visit_mc arg = 
    self#visit_i arg#f0;
    Hashtbl.add ht_classes str ("",cur_meth_t,(Hashtbl.create 10));
    self#visit_i arg#f1;
    Hashtbl.add cur_meth_t "main" ("String",cur_var_t,["String"]);
    Hashtbl.add cur_var_t str "#"; 
    loop self#visit_vd arg#f2;
    loop self#visit_s arg#f3;
    ()
  (* n_type_declaration: class_declaration OR class_extends_declaration *)
  method visit_td arg =
    match arg#f0 with 
    | ONE a -> self#visit_cd a
    | TWO a -> self#visit_ced a
    | _ -> raise (Error "visitor visit_d");
  (* n_class_declaration: identifier var_declaration* method_declaration* *)
  method visit_cd arg =
    cur_meth_t <- (Hashtbl.create 20);
    cur_var_t <- (Hashtbl.create 20);

    self#visit_i arg#f0;
    name_exists str ht_classes;
    Hashtbl.add ht_classes str ("",cur_meth_t,cur_var_t);
    (*print_string str;*)
    (*print_string " of class dec\n";*)
    
    (* variable declarations *) 
    loop self#visit_vd arg#f1;
    (* methods *)
    loop self#visit_md arg#f2;
    ()
  (* n_class_extends_declaration: identifier identifier var_declartion* method_declaration* *)
  method visit_ced arg =
    cur_meth_t <- (Hashtbl.create 20);
    cur_var_t <- (Hashtbl.create 20);

    self#visit_i arg#f0;
    name_exists str ht_classes;
    let c_name = str in 
      self#visit_i arg#f1;

      Hashtbl.add ht_classes c_name (str,cur_meth_t,cur_var_t);
      (*print_string str;*)
      (*print_string " of class extends dec\n";*)
      
      (* variable declarations *) 
      loop self#visit_vd arg#f2;
      (* methods *) 
      loop self#visit_vd arg#f2;
      loop self#visit_md arg#f3;
    ()
  (* n_var_declaration: type identifier*)
  method visit_vd arg =
    self#visit_t arg#f0;
   
    let temp = str in  
      self#visit_i arg#f1;
      name_exists str cur_var_t;
      Hashtbl.add cur_var_t str temp;
      (*print_string str;*)
      (*print_string " of var_dec\n";*)
    ()
  (* n_method_declaration: type identifier formal_parameter_list? var_declaration* statement* expression *)
  method visit_md arg =
    self#visit_t arg#f0;
    let method_type = str in 
      self#visit_i arg#f1;
      name_exists str cur_meth_t;
      let meth_name = str in

        cur_var_t <- Hashtbl.create 20;
        formal_params <- [];
        (match arg#f2 with
        | SYM a -> self#visit_fpl a
        | _ -> ());
        Hashtbl.add cur_meth_t meth_name (method_type,cur_var_t,formal_params);
        (*print_string meth_name; print_string " of method_dec\n";*)

        loop self#visit_vd arg#f3;

        loop self#visit_s arg#f4;
        self#visit_e arg#f5;
    ()

  (* n_formal_parameter_list: formal_parameter formal_parameter_rest* *)
  method visit_fpl arg =
    self#visit_fp arg#f0;
    loop self#visit_fpr arg#f1;
    ()
  (* n_formal_parameter: type identifier *)
  method visit_fp arg = 
    self#visit_t arg#f0;
    let temp = str in 
      self#visit_i arg#f1;

      Hashtbl.add cur_var_t str temp;
      formal_params <- formal_params@[temp];
      (*print_string str;*)
      (*print_string " of formal_param\n";*)
    ()
  (* n_formal_parameter_rest: formal_parameter *)
  method visit_fpr arg =
    self#visit_fp arg#f0;
    ()
  (* n_type: array_type OR boolean_type OR integer_type OR identifier *)
  method visit_t arg =
    match arg#f0 with
    | ONE a -> self#visit_at a
    | TWO a -> self#visit_bt a
    | THREE a -> self#visit_it a
    | FOUR a -> self#visit_i a
    | _ -> ();
    ()
  (* n_array_type *)
  method visit_at arg = 
    str <- "int[]";
    ()
  (* n_boolean_type *)
  method visit_bt arg = 
    str <- "boolean";
    ()
  (* n_integer_type *)
  method visit_it arg = 
    str <- "int";
    ()
  (* n_statement: block OR assignment_s OR array_assignment_s OR if_s OR while_s OR print_s *)
  method visit_s arg =
    match arg#f0 with
    | ONE a -> self#visit_b a 
    | TWO a -> self#visit_as a
    | THREE a -> self#visit_aas a
    | FOUR a -> self#visit_is a
    | FIVE a -> self#visit_ws a
    | SIX a -> self#visit_ps a   
    | _ -> ();
    ()
  (* n_block: statement* *)
  method visit_b arg =
    loop self#visit_s arg#f0;
    ()
  (* n_assignment_statement: identifier expression *)
  method visit_as arg =
    self#visit_i arg#f0;
    self#visit_e arg#f1;
    ()
  (* n_array_assignment_statement: identifier expression expression *)
  method visit_aas arg =
    self#visit_i arg#f0;
    self#visit_e arg#f1;
    self#visit_e arg#f2;
  (* n_if_statement: expression statement statement *)
  method visit_is arg =
    self#visit_e arg#f0;
    self#visit_s arg#f1;
    self#visit_s arg#f2;
    ()
  (* n_while_statement: expression statement *)
  method visit_ws arg =
    self#visit_e arg#f0;
    self#visit_s arg#f1;
    ()
  (* n_print_statement: expression *)
  method visit_ps arg =
    self#visit_e arg#f0;
    ()
  (* n_expression: and compare plus minus times array_lookup array_length message_send primary_expression *)
  method visit_e arg =
    match arg#f0 with
    | ONE a -> self#visit_ae a
    | TWO a -> self#visit_ce a
    | THREE a -> self#visit_pe a
    | FOUR a ->  self#visit_me a
    | FIVE a ->  self#visit_te a
    | SIX a ->   self#visit_al a
    | SEVEN a -> self#visit_alen a
    | EIGHT a -> self#visit_ms a
    | NINE a ->  self#visit_prie a
  (* n_and_expression: primary primary *)
  method visit_ae arg =
    self#visit_prie arg#f0;
    self#visit_prie arg#f1;
    ()
  (* n_compare_expression: primary primary *)
  method visit_ce arg =
    self#visit_prie arg#f0;
    self#visit_prie arg#f1;
    ()
  (* n_plus_expression: primary primary *)
  method visit_pe arg =
    self#visit_prie arg#f0;
    self#visit_prie arg#f1;
    ()
  (* n_minus_expression: primary primary *)
  method visit_me arg =
    self#visit_prie arg#f0;
    self#visit_prie arg#f1;
    ()
  (* n_times_expression: primary primary  *)
  method visit_te arg =
    self#visit_prie arg#f0;
    self#visit_prie arg#f1;
    ()
  (* n_array_lookup: primary primary *)
  method visit_al arg =
    self#visit_prie arg#f0;
    self#visit_prie arg#f1;
    ()
  (* n_array_length: primary *)
  method visit_alen arg =
    self#visit_prie arg#f0;
    ()
  (* n_message_send: primary identifier expression_list? *)
  method visit_ms arg =
    self#visit_prie arg#f0;
    self#visit_i arg#f1;
      match arg#f2 with
      | SYM a -> self#visit_el a
      | _ -> ();
    ()
  (* n_expression_list: expression expression_rest* *)
  method visit_el arg =
    self#visit_e arg#f0;
    loop self#visit_er arg#f1;
    ();
  (* n_expression_rest: expression *)
  method visit_er arg =
    self#visit_e arg#f0;
    ()
  (* n_primary_expression: integer true false identifier this_ array_allocation allocation not bracket *)
  method visit_prie arg =
    match arg#f0 with
    | ONE a -> self#visit_il a
    | TWO a -> self#visit_tl a
    | THREE a -> self#visit_fl a
    | FOUR a -> self#visit_i a 
    | FIVE a -> self#visit_thise a
    | SIX a -> self#visit_aae a
    | SEVEN a -> self#visit_ale a
    | EIGHT a -> self#visit_ne a 
    | NINE a -> self#visit_be a;
    ()
  (* n_integer_literal *)
  method visit_il arg =
    num <- (arg#f0);
    ()  
  (* n_true_literal *)
  method visit_tl arg =
    ()
  (* n_false_literal *)
  method visit_fl arg =
    ()
  (* n_identifier *)
  method visit_i arg =
    str <- arg#f0;
    ()
  (* n_this_expression *)
  method visit_thise arg =
    ()
  (* n_array_allocation_expression: expression  *)
  method visit_aae arg =
    self#visit_e arg#f0;
    ()
  (* n_allocation_expression: identifier *)
  method visit_ale arg =
    self#visit_i arg#f0;
    ()
  (* n_not_expression *)
  method visit_ne arg =
    self#visit_e arg#f0;
    ()
  (* n_bracket_expression: expression *)
  method visit_be arg =
    self#visit_e arg#f0;
    ()
end


