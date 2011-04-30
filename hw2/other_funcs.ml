
exception Error of string
exception Syn_error of string


(* class name *)
(* class parent  *)
(* method table (contains var table) *)
(* var table *)
(*let ht_classes:
  ((string, 
    (string * 
    ((string,string) Hashtbl.t) * 
    ((string,((string,string) Hashtbl.t)) Hashtbl.t))
  ) Hashtbl.t)
  = Hashtbl.create 20 
*)
let rec loop f lis = 
  match lis with
  | [] -> ()
  | a -> 
    f (List.hd a);
    loop f (List.tl a)

let rec method_find prog_t c m = 
  match c with 
  | "" -> raise (Syn_error "find func call") (* ; "" *)
  | _ ->
    match Hashtbl.find prog_t c with
    | parent,t_meth,_ ->
      try
        match Hashtbl.find t_meth m with
        | a -> a 
      with Not_found ->
        method_find prog_t parent m

let rec check_parent prog_t c desired_c = 
  if c = desired_c 
  then ()
  else 
    if c = ""
    then raise (Syn_error "could not coerce to parent")
    else
      match Hashtbl.find prog_t c with
      | parent,_,_ -> check_parent prog_t parent desired_c

(* coerce c up to the class of desired_c if possible *)
let check_params prog_t c desired_c = 
  let icheck ic idesired_c = 
    match ic with 
    | "int" | "int[]" | "boolean" -> 
      (if ic = idesired_c 
      then ()
      else 
        raise (Syn_error "mismatched param types"))
    | _ -> check_parent prog_t ic idesired_c
  in 
    match (String.sub c 0 1) with 
    | "#" -> icheck (String.sub c 1 ((String.length c) - 1)) desired_c
    | _ -> icheck c desired_c

(*
let printer k v =
  Printf.printf "key: %s, value: %s\n" k v
*)     

(* search upwards through parent classes for variable definition *)
let rec var_find current name prog_t ext_t = 
  try 
    match (Hashtbl.find ext_t name) with
    | a -> a
  with Not_found ->
    if current = "" 
    then ""
    else 
      match Hashtbl.find prog_t current with
      | parent,_,tt -> 
      (*  Hashtbl.iter printer tt; *)
          var_find parent name prog_t tt


(* add class variables of parent classes to "ready" (i.e. already assigned) *)
let rec update_ready prog_t ready_t c = 
  let add_to_t tt k _ = 
    Hashtbl.add tt (String.concat "" ["#";k]) k  
  in
    match c with 
    | "" -> ()
    | _ -> 
      match Hashtbl.find prog_t c with
      | parent,_,vt -> 
        update_ready prog_t ready_t parent;
        Hashtbl.iter (add_to_t ready_t) vt
  

(* check for valid inheritance and no cylical inheritance *)
let inheritance_check class_tbl t_tbl =
  let rec cylical_check _ (cname,_,_) =   
    match cname with 
    | ""  -> Hashtbl.clear t_tbl
    | _ -> 
      try 
        Hashtbl.find t_tbl cname;
        (* else, the parent was already in t_tbl, therefore cycle found *)
        raise (Syn_error "cylical inheritance found")
      with Not_found -> 
        (* this will raise Not_found if parent not in class_tbl *)
        match Hashtbl.find class_tbl cname with
        | a -> 
          Hashtbl.add t_tbl cname ();
          cylical_check "" a 
  in 
    Hashtbl.iter (cylical_check) class_tbl
   
(* check if element "name" exists in the hashtable already...*)
let name_exists name m_t = 
  try 
    Hashtbl.find m_t name;
    raise (Syn_error "name already declared in context")
  with Not_found ->
    () 

