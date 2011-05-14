open Tree_types

let cat x = String.concat "" x
let reg_as_str i = cat ["t."; (string_of_int i)]

let rec m_map f lc vc lis = 
  match lis with 
  | [] -> [],lc,vc
  | hd::tl -> 
    match (f lc vc hd) with 
    | r,_,n_lc,n_vc -> 
      match (m_map f n_lc n_vc tl) with
      | so_far,r_lc,r_vc -> r::so_far,r_lc,r_vc

let rec s_map f lc vc lis = 
  match lis with 
  | [] -> [],lc,vc
  | hd::tl -> 
    match (f lc vc hd) with 
    | r,n_lc,n_vc -> 
      match (s_map f n_lc n_vc tl) with
      | so_far,r_lc,r_vc -> r::so_far,r_lc,r_vc

(* java zeros all values on init, therefore
 * we must zero out all local variables (which are storied in reg 
 * its often unecessary in most cases, but we do it here for all locals *)
let var_dec_travel node = 
  match node with
  | Var_dec (f0,(Identifier f1)) ->
      Printf.printf "%s = 0\n" f1

let goal_travel classes_ht node = 
  
let rec formal_param_travel node = 
  match node with
  | Formal_param (f0,(Identifier f1)) -> f1
    

and meth_dec_travel cur_cls l node = 
  match node with 
  | Method_dec (f0,(Identifier cur_meth),f2,f3,f4,f5) ->
    (* fetch all hashtables that might be needed *)
    match Hashtbl.find classes_ht cur_cls with
    | meths_ht,cls_vars_ht,_ -> 
      match Hashtbl.find meths_ht cur_meth with
      | _,_,_,vars_ht ->   

  let rec expr_travel lc vc n = 
    match n with 
    | Special a -> "","",lc,vc 
    | Expr node -> 

    match node with
    | And_expr (f0,f1) -> 
      (match (m_map (primary_expr_travel) lc vc [f0;f1]) with
      | [arg1;arg2],n_lc,n_vc ->
        Printf.printf "if %s goto :first_lbl%d\n" arg1 n_lc;
        Printf.printf "t.%d = 0\n" n_vc;
        Printf.printf "goto :fail_lbl%d\n" n_lc;
        Printf.printf "first_lbl%d:\n" n_lc;
        Printf.printf "if %s goto :second_lbl%d\n" arg2 n_lc;
        Printf.printf "t.%d = 0\n" n_vc;
        Printf.printf "goto :fail_lbl%d\n" n_lc;
        Printf.printf "second_lbl%d:\n" n_lc;
        Printf.printf "t.%d = 1\n" n_vc;
        Printf.printf "fail_lbl%d:\n" n_lc;
        (*Printf.printf "t.%d = And(%s %s)\n" n_vc arg1 arg2; *)
        (reg_as_str n_vc),"",(n_lc + 1),(n_vc + 1)
      | _ -> raise (Error "0"))
    | Compare_expr (f0,f1) ->
      (match (m_map (primary_expr_travel) lc vc [f0;f1]) with
      | [arg1;arg2],n_lc,n_vc ->
        Printf.printf "t.%d = LtS(%s %s)\n" n_vc arg1 arg2; 
        (reg_as_str n_vc),"",lc,(n_vc + 1)
      | _ -> raise (Error "0"))
    | Plus_expr (f0,f1) ->
      (match (m_map (primary_expr_travel) lc vc [f0;f1]) with
      | [arg1;arg2],n_lc,n_vc ->
        Printf.printf "t.%d = Add(%s %s)\n" n_vc arg1 arg2; 
        (reg_as_str n_vc),"",lc,(n_vc + 1)
      | _ -> raise (Error "0"))
    | Minus_expr (f0,f1) ->
      (match (m_map (primary_expr_travel) lc vc [f0;f1]) with
      | [arg1;arg2],n_lc,n_vc ->
        Printf.printf "t.%d = Sub(%s %s)\n" n_vc arg1 arg2; 
        (reg_as_str n_vc),"",lc,(n_vc + 1)
      | _ -> raise (Error "0"))
    | Times_expr (f0,f1) -> (match (m_map (primary_expr_travel) lc vc [f0;f1]) with
      | [arg1;arg2],n_lc,n_vc ->
        Printf.printf "t.%d = MulS(%s %s)\n" n_vc arg1 arg2; 
        (reg_as_str n_vc),"",lc,(n_vc + 1)
      | _ -> raise (Error "0"))
    | Array_lookup (f0,f1) ->
      (match (m_map (primary_expr_travel) lc vc [f0;f1]) with
      | [arg1;arg2],n_lc,n_vc ->
        Printf.printf "t.%d = [%s]\n" n_vc arg1;  
        Printf.printf "t.%d = Lt(%s t.%d)\n" n_vc arg2 n_vc;
        Printf.printf "if t.%d goto :lbl%d\n" n_vc n_lc;
        Printf.printf "  Error (\"array index out of bounds\")\n";
        Printf.printf "lbl%d:\n" n_lc;
        Printf.printf "t.%d = Add(%s 1)\n" n_vc arg2;
        Printf.printf "t.%d = MulS(t.%d 4)\n" n_vc n_vc;
        Printf.printf "t.%d = Add(t.%d %s)\n" n_vc n_vc arg1;
        Printf.printf "t.%d = [t.%d]\n" n_vc n_vc;
        (reg_as_str n_vc),"",(n_lc + 1),(n_vc + 1)
      | _ -> raise (Error "0"))
    | Array_length f0 ->
      (match (primary_expr_travel lc vc f0) with
      | reg,_,n_lc,n_vc -> 
        Printf.printf "t.%d = [%s]\n" n_vc reg;
        (reg_as_str n_vc),"",n_lc,(n_vc + 1))
    | Message_send _ ->
      mesg_send lc vc node 
    | Primary_expr_expr f0 -> primary_expr_travel lc vc f0

  and primary_expr_travel lc vc n = 
    match n with (Primary_expr node) ->
    match node with 
    | Int_literal f0 -> 
      Printf.printf "t.%d = %d\n" vc f0;
      (reg_as_str vc),"",lc,(vc + 1)
    | True_literal -> 
      Printf.printf "t.%d = 1\n" vc;
      (reg_as_str vc),"",lc,(vc + 1)
    | False_literal ->
      Printf.printf "t.%d = 0\n" vc;
      (reg_as_str vc),"",lc,(vc + 1)
    | Identifier_primary f0 -> 
      (try  
        (match Hashtbl.find vars_ht f0 with cls -> 
          f0,cls,lc,vc)
      with Not_found ->
        (match Hashtbl.find cls_vars_ht f0 with (obj,offset) -> 
          Printf.printf "t.%d = [this.this + %d]\n" vc offset;
          (reg_as_str vc),obj,lc,(vc + 1)))
    | This_expr -> "this.this",cur_cls,lc,vc
    | Array_alloc_expr f0 -> 
      let arg,trash,lc2,vc2 = expr_travel lc vc f0 in
        Printf.printf "t.%d = call :l_array_alloc(%s)\n" vc2 arg;
        (reg_as_str vc2),"",lc2,(vc2 + 1)
    | Alloc_expr (Identifier f0) ->
        (match Hashtbl.find classes_ht f0 with 
        | _,_,siz ->  
          Printf.printf "t.%d = HeapAllocZ(%d)\n" vc siz;
          Printf.printf "if t.%d goto :lbl%d\n" vc lc;
          Printf.printf "  Error(\"null pointer\")\n";
          Printf.printf "lbl%d:\n[t.%d] = %s\n" lc vc (cat [":vmt_";f0]);
          (reg_as_str vc),f0,(lc + 1),(vc + 1) )
    | Not_expr f0 -> 
      (match expr_travel lc vc f0 with
      | res,_,nlc,nvc -> 
        Printf.printf "if %s goto :lbl%d\n" res nlc;
        Printf.printf "t.%d = 1\n" nvc;
        Printf.printf "goto :lbl%d\n" (nlc + 1);
        Printf.printf "lbl%d:\n" nlc;
        Printf.printf "t.%d = 0\n" nvc;
        Printf.printf "lbl%d:\n" (nlc + 1);
        (reg_as_str nvc),"",(nlc + 2),(nvc + 1))
    | Bracket_expr f0 ->
      expr_travel lc vc f0 

  and state_travel lc vc n = 
    match n with (State node) ->
    match node with 
    | Block f0 -> 
      (match (s_map (state_travel) lc vc f0) with
      | _,n_lc,n_vc -> "",n_lc,n_vc)
    | Assign_state ((Identifier f0),f1) ->
      (match expr_travel lc vc f1 with
      | reg,_,n_lc,n_vc -> 
        (try  
          (* class variables are not in register, need to load 
           * the address and write back into memory *)
          match Hashtbl.find cls_vars_ht f0 with _,offset -> 
            Printf.printf "[this.this + %d] = %s\n" offset reg
        with Not_found ->
          (* else it must be local and therefore already in register *)  
          Printf.printf "%s = %s\n" f0 reg);
        (reg_as_str n_vc),n_lc,n_vc)
    | If_state (f0,f1,f2) -> 
      (match expr_travel lc vc f0 with reg,_,i_lc,i_vc -> 
        Printf.printf "if %s goto :if_lbl%d\n" reg i_lc;
        match state_travel (i_lc + 1) i_vc f2 with reg,lc2,vc2 -> 
          Printf.printf "goto :end_lbl%d\nif_lbl%d:\n" i_lc i_lc;
          match state_travel lc2 vc2 f1 with reg,lc3,vc3 -> 
            Printf.printf "end_lbl%d:\n" i_lc; 
            "",lc3,vc3)
    | While_state (f0,f1) ->  
      (* print the contents of the while loop, then evaluate the end *)
        (Printf.printf "goto :con_lbl%d\nwhile_lbl%d:\n" lc lc;
        match state_travel (lc + 1) vc f1 with _,lc2,vc2 -> 
          Printf.printf "con_lbl%d:\n" lc;
          match expr_travel lc2 vc2 f0 with res,_,lc3,vc3 -> 
            Printf.printf "if %s goto :while_lbl%d\n" res lc;
            "",lc3,vc3)
    | Print_state f0 ->
      (match expr_travel lc vc f0 with res,_,lc2,vc2 -> 
        Printf.printf "PrintIntS(%s)\n" res;
        "",lc2,vc2)
    | Array_assign_state ((Identifier f0),f1,f2) ->
      match expr_travel lc vc f1 with pos,_,lc2,vc2 ->
        match expr_travel lc2 vc2 f2 with dat,_,lc3,vc3 ->
          Printf.printf "t.%d = Add(%s 1)\n" vc3 pos;
          Printf.printf "t.%d = MulS(t.%d 4)\n" vc3 vc3;
          try 
            match Hashtbl.find cls_vars_ht f0 with _,offset ->
              Printf.printf "t.%d = [this.this + %d]\n" (vc3 + 1) offset;
              Printf.printf "t.%d = Add(t.%d t.%d)\n" vc3 vc3 (vc3 +1);
              Printf.printf "[t.%d] = %s\n" vc3 dat;
              "",lc3,(vc3 + 2)
          with Not_found ->
            (* add to reach offset within array *)
            Printf.printf "t.%d = Add(t.%d %s)\n" vc3 vc3 f0;
            Printf.printf "[t.%d] = %s\n" vc3 dat;
            "",lc3,(vc3 + 1)

  and mesg_send l v n = 
    let ms_expr_travel t_lc t_vc node = 
      (match node with
      | Message_send (f0,(Identifier f1),f2) ->
        (* we should explore the parameters list first *)
        let params,lc,vc = (m_map expr_travel t_lc t_vc f2) in
          (match primary_expr_travel lc vc f0 with
          | reg,cls,n_lc,n_vc ->  
            match Hashtbl.find classes_ht cls with
            | m_ht,_,_ -> 
              match Hashtbl.find m_ht f1 with
              | _,offset,ret_type,_ -> 
                (* so it appears that we can pretty easily do static method calls
                 * without having to check vmts at runtime, but it hits a problem
                 * when working with the overridden "visit" calls in TreeVisitor *)
                Printf.printf "t.%d = [%s]\n" n_vc reg;
                Printf.printf "t.%d = [t.%d + %d]\n" n_vc n_vc offset;
                Printf.printf "t.%d = call t.%d(%s " n_vc n_vc reg;
                (*Printf.printf "t.%d = call :%s.%s(%s" n_vc cls f1 reg;*)
                List.iter (Printf.printf " %s") params;
                Printf.printf " )\n";
                (reg_as_str n_vc),ret_type,n_lc,(n_vc + 1)) 
      | _ -> raise (Error "not an object, parse error"))
    in
      ms_expr_travel l v n 

  in
    (match cur_cls,cur_meth with 
    | "#","Main" -> 
      Printf.printf "func Main()\n"
    | _ -> 
      Printf.printf "func %s.%s(this.this" cur_cls cur_meth;  
      List.iter (Printf.printf " %s") (List.map (formal_param_travel) f2);
      Printf.printf ")\n");

    List.iter var_dec_travel f3;
    match (s_map state_travel l 0 f4) with  _,lc2,vc2 -> 
      match expr_travel lc2 vc2 f5 with reg,_,lc3,vc3 -> 
        Printf.printf "ret %s\n\n" reg;
        "",cur_cls,lc3
  

(* visits class_dec and class_dec_extends *)
and type_dec_travel lc vc node = 
  match node with 
  | Type_dec cl->
    match cl with 
    | Class_dec ((Identifier f0),f1,f2) ->
      (*List.map var_dec_traveler f1;*)
      (match s_map meth_dec_travel f0 lc f2 with
      | _,_,nlc -> [],nlc,0 )
    | Class_extends_dec ((Identifier f0),(Identifier f1),f2,f3) -> 
      (*List.map var_dec_traveler f2;*)
      (match s_map meth_dec_travel f0 lc f3 with
      | _,_,nlc -> [],nlc,0 )
in

  match node with 
  | Goal (f0,f1) -> 
    (* we turn the main class dec into a method dec...*) 
    match f0 with (Main_class (_,_,f2,f3)) -> 
      match meth_dec_travel "#" 0 (Method_dec 
              ((Type Integer_type),
              (Identifier "Main"),
              [],
              f2,
              f3,
              (Special 1))) with
      | _,_,lc -> s_map type_dec_travel lc 0 f1

