open Otree
open Other_funcs
open Table_visitor

class checker_visitor =
  object  (self)
  inherit virt_visitor

  val mutable str = ""
  val mutable expr_type = ""
  val mutable cur_class = ""
  val mutable num = 0
  val mutable in_var = false
  val mutable formal_params:(string list) =  []
  val mutable cur_var_t = Hashtbl.create 1
  val mutable cur_meth_t = Hashtbl.create 1
  val mutable ext_var_t = Hashtbl.create 1
  val mutable ready_vars_t = Hashtbl.create 40

  (* goal: main_class type_declaration-list *)
  method visit_g arg = 
    self#visit_mc arg#f0;
    loop self#visit_td arg#f1;
    ()
  (* main_class: identifier identifier var_declaration* statement* *)
  method visit_mc arg = 
    self#visit_i arg#f0;

    cur_class <- str;
    match (Hashtbl.find ht_classes str) with
      | _,b,c -> 
        cur_meth_t <- b;
        ext_var_t <- c;

    self#visit_i arg#f1;
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
    self#visit_i arg#f0;
    cur_class <- str;
    Hashtbl.clear ready_vars_t;
    Hashtbl.add ready_vars_t "*int[]" "int[]";
    update_ready ht_classes ready_vars_t cur_class;

    (*print_string str;*)
    (*print_string " of class declaration in CV\n";*)

    match (Hashtbl.find ht_classes str) with
      | _,b,c -> 
        cur_meth_t <- b;
        ext_var_t <- c;

    in_var <- true;
    loop self#visit_vd arg#f1;
    in_var <- false;
    loop self#visit_md arg#f2;
    () 
  (* n_class_extends_declaration: identifier identifier var_declartion* method_declaration* *)
  method visit_ced arg =
    self#visit_i arg#f0;
    cur_class <- str;
    Hashtbl.clear ready_vars_t;
    Hashtbl.add ready_vars_t "*int[]" "int[]";
    update_ready ht_classes ready_vars_t cur_class;

    (*print_string str; print_string " of class extends declaration in CV\n";*)

    match (Hashtbl.find ht_classes str) with
      | a,b,c -> 
        cur_meth_t <- b;
        ext_var_t <- c;

    (* self#visit_i arg#f1; already checked valid parent *)
    in_var <- true;
    loop self#visit_vd arg#f2;
    in_var <- false;
    loop self#visit_md arg#f3;
    ()
  (* n_var_declaration: type identifier*)
  (* check variables declared to be of valid types *)
  method visit_vd arg =
    self#visit_t arg#f0;
    (*print_string str;*)
    (*print_string " - type in var_dec CV...\n";*)
    let v_type = str in
      self#visit_i arg#f1;
      match v_type with 
        | "int" ->
          Hashtbl.add ready_vars_t str "int"
        | "int[]" -> 
          if in_var   
          then Hashtbl.add ready_vars_t str "int[]"
          else ()
        | "boolean" ->
          Hashtbl.add ready_vars_t str "boolean"
        | _ -> 
          (if in_var 
          then 
            match Hashtbl.find ht_classes v_type with  _ -> ();
            Hashtbl.add ready_vars_t str v_type
          else ())
        
  (* n_method_declaration: type identifier formal_parameter_list? var_declaration* statement* expression *)
  method visit_md arg =
    self#visit_t arg#f0;
    let ret_type = str in
      (match ret_type with 
        | "int" | "int[]" | "boolean" -> ()
        | _ -> 
          match Hashtbl.find ht_classes ret_type with | _ -> ());

      self#visit_i arg#f1;

      (*print_string str; print_string " method_dec in CV\n";*)
      (match Hashtbl.find cur_meth_t str with 
        | _,a,_ -> cur_var_t <- a);

      (match arg#f2 with
        | SYM a -> self#visit_fpl a
        | _ -> ());

      loop self#visit_vd arg#f3;
      loop self#visit_s arg#f4;

      self#visit_e arg#f5;
      (*Printf.printf "method should return %s, returns %s\n" ret_type expr_type;*)
      if expr_type = ret_type 
      then ()
      else raise (Syn_error "wrong return type")

  (* n_formal_parameter_list: formal_parameter formal_parameter_rest* *)
  method visit_fpl arg =
    self#visit_fp arg#f0;
    loop self#visit_fpr arg#f1;
    ()
  (* n_formal_parameter: type identifier *)
  method visit_fp arg = 
    self#visit_t arg#f0;
    let fp_type = str in
      self#visit_i arg#f1;
      match fp_type with 
        | "int" | "int[]" | "boolean" -> Hashtbl.add ready_vars_t str fp_type
        | _ -> 
          match Hashtbl.find ht_classes fp_type with _ -> ();
          Hashtbl.add ready_vars_t str fp_type;
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
    expr_type <- "int";
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
    let id_type,id = expr_type,str in 
      (* id does not exist *)
      if id_type = "" 
      then raise (Syn_error "undeclared variable used")  
      else
        self#visit_e arg#f1;
        (*Printf.printf "assign to type %s type %s CV\n" id_type expr_type;*)
        check_params ht_classes expr_type id_type; 
        Hashtbl.add ready_vars_t id id_type;

  (* n_array_assignment_statement: identifier expression expression *)
  method visit_aas arg =
    self#visit_i arg#f0;
    let id_type,id = expr_type,str in
      (* make sure that this array is actually allocated already *)
      match Hashtbl.find ready_vars_t id with _ -> ();

      self#visit_e arg#f1;
      let id_type2 = expr_type in 
        self#visit_e arg#f2;
        match id_type,id_type2,expr_type with
        | "int[]","int","int" -> ()
        | _ -> raise (Syn_error "bad values in array assignment")

  (* n_if_statement: expression statement statement *)
  method visit_is arg =
    (*Printf.printf "if statement\n";*)
    self#visit_e arg#f0;
    match expr_type with 
    | "boolean" -> 
      self#visit_s arg#f1;
      self#visit_s arg#f2;
    | _ -> raise (Syn_error "bad while state\n")

  (* n_while_statement: expression statement *)
  method visit_ws arg =
    (*Printf.printf "while statement\n";*)
    self#visit_e arg#f0;
    match expr_type with 
    | "boolean" -> self#visit_s arg#f1;
    | _ -> raise (Syn_error "bad while state\n")

  (* n_print_statement: expression *)
  method visit_ps arg =
    self#visit_e arg#f0;
    (*Printf.printf "trying to print type %s CV\n" expr_type;*)
    match expr_type with 
      | "int" -> ()
      | _ -> raise (Syn_error "bad print_statement")

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
    (match expr_type with 
    | "boolean" -> 
      (self#visit_prie arg#f1;
      match expr_type with 
      | "boolean" -> () 
      | _ -> raise (Syn_error "2nd primary in AND"));
    | _ -> raise (Syn_error "1st primary in AND"));
    expr_type <- "boolean";
    ()
  (* n_compare_expression: primary primary *)
  method visit_ce arg =
    self#visit_prie arg#f0;
    let a0_expr = expr_type in 
      self#visit_prie arg#f1;

      match expr_type,a0_expr with
      | "int","int" -> 
        expr_type <- "boolean";
        ();
      | _ -> raise (Syn_error "bad comparison types")
  (* n_plus_expression: primary primary *)
  method visit_pe arg =
    self#visit_prie arg#f0;
    self#visit_prie arg#f1;

    expr_type <- "int";
    ()
  (* n_minus_expression: primary primary *)
  method visit_me arg =
    self#visit_prie arg#f0;
    (match expr_type with
    | "int" -> ()
    | _ -> raise (Syn_error "minus expr operand 1"));

    self#visit_prie arg#f1;
    (match expr_type with
    | "int" -> ();
    | _ -> raise (Syn_error "minus expr operand 2"));
    expr_type <- "int";
    ()
  (* n_times_expression: primary primary  *)
  method visit_te arg =
    self#visit_prie arg#f0;
    (match expr_type with
    | "int" -> ();
    | _ -> raise (Syn_error "times expr operand 1"));

    self#visit_prie arg#f1;
    (match expr_type with
    | "int" -> ();
    | _ -> raise (Syn_error "times expr operand 2"));

    expr_type <- "int";
    ()
  (* n_array_lookup: primary primary *)
  method visit_al arg =
    self#visit_prie arg#f0;
    match expr_type with
    | "#int[]" | "int[]" -> 
      (match Hashtbl.find ready_vars_t str with _ -> ();
      self#visit_prie arg#f1;
      match expr_type with 
      | "int" -> 
        expr_type <- "int";
        ()
      | _ -> raise (Syn_error "bad array lookup"))
    | _ -> raise (Syn_error "bad array lookup 1")

  (* n_array_length: primary *)
  method visit_alen arg =
    self#visit_prie arg#f0;
    match expr_type with
    | "int[]" | "*int[]" -> 
      (match Hashtbl.find ready_vars_t str with _ -> ());
      expr_type <- "int";
      ()
    | _ -> raise (Syn_error "taking length of non-array type")

  (* n_message_send: primary identifier expression_list? *)
  method visit_ms arg =
    self#visit_prie arg#f0;
          (*Printf.printf "taking %s, %s\n" expr_type str;*)
    let c_name,id = expr_type,str in
      (match (String.sub id 0 1) with
      | "#" -> () 
      | _ -> match Hashtbl.find ready_vars_t id with _ -> ());

      self#visit_i arg#f1;
      (*Printf.printf "(class %s (%s)).%s of message_send CV\n" c_name id str;*)
      match method_find ht_classes c_name str with
      | r_type,_,fp -> 
        expr_type <- r_type;
        formal_params <- fp;
        (*Printf.printf "method call returns type %s\n" r_type; *)

       ( match arg#f2 with
          | SYM a -> self#visit_el a;()
          | _ -> 
            formal_params <- fp;
            match formal_params with
            | [] -> ()
            | _ -> raise (Syn_error "message expected formal parmeters CV\n"));
        match formal_params with
        | [] -> 
          expr_type <- r_type; 
          str <- (String.concat "" ["#";r_type]);
          (*Printf.printf "returning %s, %s\n" expr_type str;*)
          ()
        | _ -> raise (Syn_error "Expected more params CV\n") 

  (* n_expression_list: expression expression_rest* *)
  method visit_el arg =
    let fp =  formal_params in 
  (*Printf.printf "expressions_list!\n";*)
      self#visit_e arg#f0;
  (*Printf.printf "expressions_list!\n";*)
      match fp with 
      | [] -> raise (Syn_error "didn't expect any params")
      | hd::tl -> 
        (*Printf.printf "param %s, gets %s CV\n" hd expr_type;*)
        check_params ht_classes expr_type hd;
        formal_params <- tl;
        (loop self#visit_er arg#f1)
      (*| _ -> raise (Syn_error "failed first param check CV") *)

  (* n_expression_rest: expression *)
  method visit_er arg =
    let fp = formal_params in 
      match fp with
      | [] -> raise (Syn_error "too many params given CV\n")
      | hd::tl -> 
        self#visit_e arg#f0;
        (*Printf.printf "paramR %s, gets %s CV\n" hd expr_type;*)
        check_params ht_classes expr_type hd;
        formal_params <- tl;

  (* n_primary_expression: integer true false identifier this_ array_allocation allocation not bracket *)
  method visit_prie arg =
    match arg#f0 with
    | ONE a -> self#visit_il a
    | TWO a -> self#visit_tl a
    | THREE a -> self#visit_fl a
    | FOUR a -> 
      self#visit_i a
      (*match Hashtbl.find ready_vars_t str with _ -> ());
      () *)
    | FIVE a -> self#visit_thise a
    | SIX a -> self#visit_aae a
    | SEVEN a -> self#visit_ale a
    | EIGHT a -> self#visit_ne a 
    | NINE a -> self#visit_be a;
    ()
  (* n_integer_literal *)
  method visit_il arg =
    num <- arg#f0;
    expr_type <- "int";
    ()
  (* n_true_literal *)
  method visit_tl arg =
    str <- "true";
    expr_type <- "boolean";
    ()
  (* n_false_literal *)
  method visit_fl arg =
    str <- "false";
    expr_type <- "boolean";
    ()
  (* n_identifier *)
  method visit_i arg =
    str <- arg#f0;
    try 
      match Hashtbl.find cur_var_t str with
      | i_type -> expr_type <- i_type;
    with Not_found ->
      expr_type <- (var_find cur_class str ht_classes ext_var_t);

    (*Printf.printf "id %s has type %s CV\n" str expr_type;*)
    ()
  (* n_this_expression *)
  method visit_thise arg =
    expr_type <- cur_class;
    ()
  (* n_array_allocation_expression: expression  *)
  method visit_aae arg =
    self#visit_e arg#f0;
    str <- "int[]"; 
    match expr_type with 
    | "int" -> expr_type <- "#int[]"; 
    | _ ->  raise (Syn_error "aray allocated with non int size")

  (* n_allocation_expression: identifier *)
  method visit_ale arg =
    self#visit_i arg#f0;
    match Hashtbl.find ht_classes str with _ -> ();
    expr_type <- str; 
    str <- String.concat "" ["#";expr_type];
    (*Printf.printf "allocated of type %s CV\n" str;*)
    ()
  (* n_not_expression *)
  method visit_ne arg =
    self#visit_e arg#f0;
    match expr_type with 
    | "boolean" -> expr_type <- "boolean"
    | _ -> raise (Syn_error "bad args to not_expression")

  (* n_bracket_expression: expression *)
  method visit_be arg =
    self#visit_e arg#f0;
    ()
end
