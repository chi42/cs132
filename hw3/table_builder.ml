open Tree_types

let finder x y = match y with c,_ -> c = x  
let sep x = match x with _,lis -> lis 

(* append a counter offset to each element in the list *)
let append_counter lis start = 
  let c = ref 0 in
    c := !c + start;
    List.map (fun v -> c := !c + 4; v,(!c - 4)) lis

(* result is child_lis, but with elements in child_lis and parent_lis
 * replaced, order in maintained *)
let rec uniq_join lis_hd lis_tl child_lis new_cls = 
  match lis_tl with 
  | [] _ -> lis_hd 
  | (ty,ele)::tl -> 
    (* see if ele exists in the child_lis *)
    if List.mem ele child_lis 
    then uniq_join (lis_hd@[new_cls,ele]) tl child_lis new_cls
    else uniq_join (lis_hd@[ty,ele]) tl child_lis new_cls

(* if element in child_l not in parent_l, add it to so_far *)
let rec join parent_l child_l so_far =
  let presence x lis = 
    if List.mem x lis then [] else [x] 
  in
    match child_l with
    | [] -> so_far
    | hd::tl -> join parent_l tl so_far@(presence hd parent_l)

(* generate virtual memory table for class "cls" *)
let generate_vmt a b =
  let rec maker classes_ht cls cur =
    match Hashtbl.find classes_ht cur with
    | parent,_,_,_,l_meth ->
      match parent with 
      | "" -> List.map (fun x -> cur,x) l_meth 
      | _ -> 
        (* update the class of each method call *)
        let upd = (uniq_join [] (maker classes_ht cls parent) l_meth cur) in
          (* add all methods in l_meth, that are not in upd *)
          upd@(List.map 
            (fun x -> cur,x) 
            (join (List.map (fun x -> match x with _,y -> y) upd) l_meth []))
  in 
    b,(append_counter (maker a b b) 0)

(* generate all virtual memory tables *)
let vmts_maker classes_ht l_class = 
  List.map (generate_vmt classes_ht) l_class

(* generate layout of local class variables for a class *)
let generate_vars a b =
  let rec maker classes_ht cur  = 
    match Hashtbl.find classes_ht cur with
    | parent,_,_,l_var,_ -> 
      match parent with  
      | "" -> l_var
      | _ -> (maker classes_ht parent)@l_var 
  in 
    b,(append_counter (maker a b) 4)
(* generate all variable tables *) 
let vars_maker classes_ht l_class = 
  List.map (generate_vars classes_ht) l_class

(* 
 *  for future efficency, rebuild tables to include vmts & offsets 
 *  new hashtable has form:
 *    (class_name, methods_ht * class_variables_ht * bytes_of_obj) Hashtable
 *  methods_ht: 
 *    (method_name, owner_class * offset * return type * vars_ht) Hashtable
 *  class_variables_h: 
 *    (variable_name, class * offset) Hashtable
*)

let table_rebuild varts vmts new_classes_ht old_classes_ht = 
  let create_vars_ht varts cls = 
    let iter_add ht x = 
      match x with (i_var,i_cls),i_offset ->
         (*Printf.printf "Adding variable: %s.%s in %s\n" i_cls i_var cls; *)
        (Hashtbl.add ht i_var (i_cls,i_offset))
    in
    let vt,var_lis = (Hashtbl.create 20),(sep (List.find (finder cls) varts)) in
      List.iter (iter_add vt) var_lis;
      (* Printf.printf "\tthis object has size: %d\n" ((List.length var_lis) * 4
       * + 4); *)
      vt,((List.length var_lis) * 4 + 4)
  in
  (* add arg to new hashtable, pull some data from old meth tables *)
  let iter_methods new_ht arg = 
    match arg with (cls,meth),offset ->  
      (* getting the old local method's variable table *) 
      match Hashtbl.find old_classes_ht cls with 
      | _,_,meth_ht,_,_ -> 
        match Hashtbl.find meth_ht meth with 
        | rt,_,lv_ht,_ ->
          (*Hashtbl.iter (Printf.printf "--Adding %s.%s\n") lv_ht;*)
          (* Printf.printf "Adding method: %s.%s\n" cls meth; *)
          Hashtbl.add new_ht meth (cls,offset,rt,lv_ht)
  in 
  let iter_classes new_ht varts arg = 
    match arg with (cls,lis) ->
      let nt = Hashtbl.create 20 in
        (* Printf.printf "Adding class: %s\n" cls; *)
        List.iter (iter_methods nt) lis;
        let vt,siz = create_vars_ht varts cls in
        (Hashtbl.add new_ht cls (nt,vt,siz))
  in
    List.iter (iter_classes new_classes_ht varts) vmts

(* because when iterating through a hashtable, there is no guarantee
 *  that we get the same list back everytime, & for convience, we 
 *  generate both lists and hashtables 
 *
 * format of output generated by goal node:
 *  mc,classes_hashtable
 *  where classes_hashtable has 
      key=name of class, value=parent,var_lis,meth_lis,var_ht,meth_ht
 *  where var_ht has key=identifier, val=type
 *  where meth_ht has 
      key=meth_name, val=return type,formal parameters,local_var_ht,vars
 *)

let type_as_str t = 
  match t with 
  | Type f0 ->
    match f0 with 
    | Array_type -> "int[]"
    | Integer_type -> "int"
    | Boolean_type -> "boolean"
    | Identifier_type ty -> ty

let var_dec_travel vars_ht node = 
  match node with
  | Var_dec (f0,(Identifier f1)) ->
     (*Printf.printf "add variable %s of type %s\n" f1 (type_as_str f0); *)
    Hashtbl.add vars_ht f1 (type_as_str f0);
    (f1,(type_as_str f0))

let formal_param_travel vars_ht node = 
  match node with
  | Formal_param (f0,(Identifier f1)) ->
    (* Printf.printf "add formal param %s\n" f1; *)
    Hashtbl.add vars_ht f1 (type_as_str f0); 
    (type_as_str f0),f1

let meth_dec_travel methods_ht node = 
    match node with 
    | Method_dec (f0,(Identifier f1),f2,f3,f4,f5) ->
      let vars_ht = Hashtbl.create 20 in 
        (* formal parameters are added to variable list, but they are also
           saved as a seperate list *)
        let fp = (List.map (formal_param_travel vars_ht) f2) in
        let vars = (List.map (var_dec_travel vars_ht) f3) in
          (* Printf.printf "add method %s\n" f1; *)
          Hashtbl.add methods_ht f1 ((type_as_str f0),fp,vars_ht,vars);
          f1 (* f1 is the name of method *)
    
(* visits class_dec and class_dec_extends *)
let type_dec_travel classes_ht node = 
  let global_vars_ht,methods_ht = (Hashtbl.create 20),(Hashtbl.create 20) in
    match node with 
    | Type_dec cl->
      match cl with 
      | Class_dec ((Identifier f0),f1,f2) ->
        let vars = (List.map (var_dec_travel global_vars_ht) f1) in
        let meths = (List.map (meth_dec_travel methods_ht) f2) in
          (* Printf.printf "add class %s\n" f0; *)
          Hashtbl.add classes_ht f0 ("",global_vars_ht,methods_ht,vars,meths);
          f0
      | Class_extends_dec ((Identifier f0),(Identifier f1),f2,f3) -> 
        let vars = (List.map (var_dec_travel global_vars_ht) f2) in
        let meths = (List.map (meth_dec_travel methods_ht) f3) in
          (* Printf.printf "add extends class %s\n" f0; *)
          Hashtbl.add classes_ht f0 (f1,global_vars_ht,methods_ht,vars,meths);
          f0

let main_class_travel node = 
  let vars_ht = Hashtbl.create 20 in 
    match node with 
    | Main_class ((Identifier f0),(Identifier f1),f2,_) ->
      let vars = List.map (var_dec_travel vars_ht) f2 in 
        f0,f1,vars_ht,vars
    
let goal_travel node = 
  let mc_ht,classes_ht = (Hashtbl.create 2),(Hashtbl.create 20) in
    match node with
    | Goal (f0,f1) -> 
      let _,_,mc_vars_ht,mc_vars = main_class_travel f0 in
        Hashtbl.add mc_ht "Main" ("void",[],mc_vars_ht,mc_vars);
        Hashtbl.add classes_ht "#" ("",(Hashtbl.create 1),mc_ht,[],["Main"]);
        let cls = (List.map (type_dec_travel classes_ht) f1) in 
          classes_ht,"#"::cls

