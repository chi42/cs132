
(* FOR FUTURE REFERENCES:
    ocaml does not really allow for function overloading as in languages
    like Java. 
    
    this is somewhat problematic. 
    and is why this code looks like a**
 *)

type ('a) symbol = 
  | SYM of 'a
  | NULL

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) or_expr = 
  | ONE of 'a
  | TWO of 'b
  | THREE of 'c
  | FOUR of 'd
  | FIVE of 'e
  | SIX of 'f
  | SEVEN of 'g
  | EIGHT of 'h
  | NINE of 'i

 class virtual virt_visitor = 
   object 
     method virtual visit_g: n_goal -> unit
     method virtual visit_mc: n_main_class -> unit
     method virtual visit_td: n_type_declaration -> unit
     method virtual visit_cd: n_class_declaration -> unit
     method virtual visit_ced: n_class_extends_declaration -> unit
     method virtual visit_vd: n_var_declaration -> unit
     method virtual visit_md: n_method_declaration -> unit
     method virtual visit_fpl: n_formal_parameter_list -> unit
     method virtual visit_fp: n_formal_parameter -> unit
     method virtual visit_fpr: n_formal_parameter_rest -> unit
     method virtual visit_t: n_type -> unit
     method virtual visit_at: n_array_type -> unit
     method virtual visit_bt: n_boolean_type -> unit
     method virtual visit_it: n_integer_type -> unit
     method virtual visit_s: n_statement -> unit
     method virtual visit_b: n_block -> unit
     method virtual visit_as: n_assignment_statement -> unit
     method virtual visit_aas: n_array_assignment_statement -> unit
     method virtual visit_is: n_if_statement -> unit
     method virtual visit_ws: n_while_statement -> unit
     method virtual visit_ps: n_print_statement -> unit
     method virtual visit_e: n_expression -> unit
     method virtual visit_ae: n_and_expression -> unit
     method virtual visit_ce: n_compare_expression -> unit
     method virtual visit_pe: n_plus_expression -> unit
     method virtual visit_me: n_minus_expression -> unit
     method virtual visit_te: n_times_expression  -> unit
     method virtual visit_al: n_array_lookup -> unit
     method virtual visit_alen: n_array_length -> unit
     method virtual visit_ms: n_message_send -> unit
     method virtual visit_el: n_expression_list -> unit
     method virtual visit_er: n_expression_rest -> unit
     method virtual visit_prie: n_primary_expression -> unit
     method virtual visit_il: n_integer_literal -> unit
     method virtual visit_tl: n_true_literal -> unit
     method virtual visit_fl: n_false_literal -> unit
     method virtual visit_i: n_identifier -> unit
     method virtual visit_thise: n_this_expression -> unit
     method virtual visit_aae: n_array_allocation_expression -> unit
     method virtual visit_ale: n_allocation_expression -> unit
     method virtual visit_ne: n_not_expression -> unit
     method virtual visit_be: n_bracket_expression -> unit
 end
 (******************************************************************)

 and virtual node = 
   object 
     method virtual accept: virt_visitor -> unit
 end

 (******************************************************************)

 (* mainclass typedeclaration* *)
 and n_goal (a0:n_main_class) (a1:n_type_declaration list) = 
   object (self)
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v = v#visit_g (self :> n_goal)
 end

 (* identifier identifier vardeclaration* statement* *)
 and n_main_class (a0:n_identifier) (a1:n_identifier) 
    (a2:n_var_declaration list) (a3:n_statement list) = 
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method f2 = a2
     method f3 = a3
     method accept v = v#visit_mc (self :> n_main_class)
 end

 (* classdeclaration OR classextendsdeclaration STAR *) 
 and n_type_declaration a0 =
    object (self)
      inherit node
      method f0 =
        match a0 with 
        | ONE (a:n_class_declaration) -> a0 
        | TWO (a:n_class_extends_declaration) -> a0
          (****)
        | THREE (a:n_identifier)-> a0
        | FOUR (a:n_identifier)-> a0
        | FIVE (a:n_identifier)-> a0
        | SIX (a:n_identifier)-> a0
        | SEVEN (a:n_identifier)-> a0
        | EIGHT (a:n_identifier)-> a0
        | NINE (a:n_identifier)-> a0
     method accept v = v#visit_td (self :> n_type_declaration)
 end

 (* identifier vardeclaration* methoddeclaration *)
 and n_class_declaration 
    (a0:n_identifier) 
    (a1:n_var_declaration list) (a2:n_method_declaration list) =
   object  (self)
     inherit node
     method f0 = a0 
     method f1 = a1
     method f2 = a2
     method accept v = v#visit_cd (self :> n_class_declaration)
 end

 (* identifier identifier vardeclaration* method_declaration* *)
 and n_class_extends_declaration 
     (a0:n_identifier) (a1:n_identifier) 
     (a2:n_var_declaration list) (a3:n_method_declaration list) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method f2 = a2
     method f3 = a3
     method accept v = v#visit_ced (self :> n_class_extends_declaration)
 end

 (* type identifier STAR *)
 and n_var_declaration 
     (a0:n_type) (a1:n_identifier)  =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v = v#visit_vd (self :> n_var_declaration)
 end

 (* type identifier formal_parameter_list 
    var_declaration* statement* expression STAR *) 
 and n_method_declaration 
     (a0:n_type) (a1:n_identifier) a2 
     (a3:n_var_declaration list) (a4:n_statement list) (a5:n_expression) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method f2 =
      match a2 with 
      | SYM (v:n_formal_parameter_list) -> a2
      | _ -> a2
     method f3 = a3
     method f4 = a4
     method f5 = a5
     method accept v = v#visit_md (self :> n_method_declaration)
 end

 (* formal_parameter formal_parameter_rest* *)
 and n_formal_parameter_list
     (a0:n_formal_parameter) (a1:n_formal_parameter_rest list) =
   object (self) 
     inherit node
     method f0 = a0  
     method f1 = a1
     method accept v = v#visit_fpl (self :> n_formal_parameter_list)
 end
 
 (* type identifier *)
 and n_formal_parameter (a0:n_type) (a1:n_identifier) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v = v#visit_fp (self :> n_formal_parameter)
 end

 (* formal parameter STAR *)
 and n_formal_parameter_rest (a0:n_formal_parameter) =  
   object (self) 
     inherit node
     method f0 = a0
     method accept v = v#visit_fpr (self :> n_formal_parameter_rest)
 end

 (* int_array boolean integer identifier *)
 and n_type a0 =  
   object (self) 
     inherit node
     method f0 = 
      match a0 with
        | ONE (a:n_array_type) -> a0 
        | TWO (a:n_boolean_type) -> a0
        | THREE (a:n_integer_type)-> a0
        | FOUR (a:n_identifier)-> a0
          (****)
        | FIVE (a:n_identifier)-> a0
        | SIX (a:n_identifier)-> a0
        | SEVEN (a:n_identifier)-> a0
        | EIGHT (a:n_identifier)-> a0
        | NINE (a:n_identifier)-> a0
     method accept v = v#visit_t (self :> n_type)
 end

 and n_array_type =
   object (self) 
     inherit node
     method accept v = v#visit_at (self :> n_array_type)
 end

 and n_boolean_type =
   object (self) 
     inherit node
     method accept v = v#visit_bt (self :> n_boolean_type)
 end

 and n_integer_type  =
   object (self) 
     inherit node
     method accept v = v#visit_it (self :> n_integer_type)
 end
  
 (* block OR assignmentstatement OR arrayassignmentstatement OR 
     ifstatemetn OR while OR print  STAR*) 
 and n_statement a0 =
   object (self) 
     inherit node
     method f0 = 
      match a0 with
        | ONE (a:n_block) -> a0 
        | TWO (a:n_assignment_statement) -> a0
        | THREE (a:n_array_assignment_statement)-> a0
        | FOUR (a:n_if_statement)-> a0
        | FIVE (a:n_while_statement)-> a0
        | SIX (a:n_print_statement)-> a0
          (****)
        | SEVEN (a:n_identifier)-> a0
        | EIGHT (a:n_identifier)-> a0
        | NINE (a:n_identifier)-> a0
     method accept v = v#visit_s (self :> n_statement)
 end

 (* statement *)
 and n_block  (a0:n_statement list) =
   object (self) 
     inherit node
     method f0 = a0
     method accept v = v#visit_b (self :> n_block)
 end

 (* identifier expression *)
 and n_assignment_statement (a0:n_identifier) (a1:n_expression) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v = v#visit_as (self :> n_assignment_statement)
 end

 (* identifier expression expression *)
 and n_array_assignment_statement
     (a0:n_identifier) (a1:n_expression) (a2:n_expression) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method f2 = a2
     method accept v = v#visit_aas (self :> n_array_assignment_statement)
 end

 (* expression statement statement *)
 and n_if_statement
   (a0:n_expression) (a1:n_statement) (a2:n_statement) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method f2 = a2 
     method accept v = v#visit_is (self :> n_if_statement)
 end

 and n_while_statement (a0:n_expression) (a1:n_statement) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v = v#visit_ws (self :> n_while_statement)
 end

 and n_print_statement (a0:n_expression) =
   object (self) 
     inherit node
     method f0 = a0
     method accept v = v#visit_ps (self :> n_print_statement)
 end

 and n_expression a0 = 
   object (self) 
     inherit node
     method f0 = 
      match a0 with 
        | ONE (a:n_and_expression) -> a0 
        | TWO (a:n_compare_expression) -> a0
        | THREE (a:n_plus_expression)-> a0
        | FOUR (a:n_minus_expression)-> a0
        | FIVE (a:n_times_expression)-> a0
        | SIX (a:n_array_lookup)-> a0
        | SEVEN (a:n_array_length)-> a0
        | EIGHT (a:n_message_send)-> a0
        | NINE (a:n_primary_expression)-> a0
     method accept v = v#visit_e (self :> n_expression)
 end

 and n_and_expression 
     (a0:n_primary_expression) (a1:n_primary_expression) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v = v#visit_ae (self :> n_and_expression)
 end

 and n_compare_expression 
     (a0:n_primary_expression) (a1:n_primary_expression) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v = v#visit_ce (self :> n_compare_expression)
 end

 and n_plus_expression 
     (a0:n_primary_expression) (a1:n_primary_expression) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v = v#visit_pe (self :> n_plus_expression)
 end
  
 and n_minus_expression 
     (a0:n_primary_expression) (a1:n_primary_expression) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v = v#visit_me (self :> n_minus_expression)
 end

 and n_times_expression 
     (a0:n_primary_expression) (a1:n_primary_expression) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v = v#visit_te (self :> n_times_expression)
 end
  
 and n_array_lookup 
     (a0:n_primary_expression) (a1:n_primary_expression) =
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v = v#visit_al (self :> n_array_lookup)
 end

 and n_array_length (a0:n_primary_expression) = 
   object (self) 
     inherit node
     method f0 = a0
     method accept v = v#visit_alen (self :> n_array_length)
 end
  
 and n_message_send 
     (a0:n_primary_expression) (a1:n_identifier) a2 = 
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method f2 = 
      match a2 with
      | SYM (v:n_expression_list) -> a2
      | _ -> a2
     method accept v = v#visit_ms (self :> n_message_send)
 end

 and n_expression_list (a0:n_expression) (a1:n_expression_rest list) = 
   object (self) 
     inherit node
     method f0 = a0
     method f1 = a1
     method accept v  = v#visit_el (self :> n_expression_list)
 end
  
 and n_expression_rest (a0:n_expression) =
   object (self) 
     inherit node
     method f0 = a0
     method accept v = v#visit_er (self :> n_expression_rest)
 end

 and n_primary_expression a0 = 
   object (self) 
     inherit node
     method f0 = 
      match a0 with
        | ONE (a:n_integer_literal) -> a0 
        | TWO (a:n_true_literal) -> a0
        | THREE (a:n_false_literal)-> a0
        | FOUR (a:n_identifier)-> a0
        | FIVE (a:n_this_expression)-> a0
        | SIX (a:n_array_allocation_expression)-> a0
        | SEVEN (a:n_allocation_expression)-> a0
        | EIGHT (a:n_not_expression)-> a0
        | NINE (a:n_bracket_expression)-> a0
     method accept v = v#visit_prie (self :> n_primary_expression)
 end
  
 and n_integer_literal (a0:int) = 
   object (self) 
     inherit node
     method f0 = a0
     method accept v = v#visit_il (self :> n_integer_literal)
 end

 and n_true_literal = 
   object (self) 
     inherit node
     method f0 = true 
     method accept v = v#visit_tl (self :> n_true_literal)
 end

 and n_false_literal = 
   object (self) 
     inherit node
     method f0 = false
     method accept v = v#visit_fl (self :> n_false_literal)
 end

 and n_identifier (a0:string) = 
   object (self) 
     inherit node
     method f0 = a0 
     method accept v = v#visit_i (self :> n_identifier)
 end
  
 and n_this_expression = 
   object (self) 
     inherit node
     method accept v = v#visit_thise (self :> n_this_expression)
 end

 and n_array_allocation_expression (a0:n_expression) = 
   object (self) 
     inherit node
     method f0 = a0
     method accept v = v#visit_aae (self :> n_array_allocation_expression)
 end
  
 and n_allocation_expression (a0:n_identifier) = 
   object (self) 
     inherit node
     method f0 = a0
     method accept v = v#visit_ale (self :> n_allocation_expression)
 end

 and n_not_expression (a0:n_expression) = 
   object (self) 
     inherit node
     method f0 = a0
     method accept v = v#visit_ne (self :> n_not_expression)
 end
  
 and n_bracket_expression (a0:n_expression) = 
   object (self) 
     inherit node
     method f0 = a0
     method accept v = v#visit_be (self :> n_bracket_expression)
 end


