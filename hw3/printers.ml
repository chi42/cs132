(* given a vmt, print it out nice and pretty *)
let print_vmt arg =  
  let printer l = 
    match l with tuple,offset_loc ->
      match tuple with a,b ->
      Printf.printf "  :%s.%s\n" a b 
  in 
    match arg with name,lis ->
      match name with 
      | "#" ->  ()
      | _ ->  
        Printf.printf "\nconst vmt_%s\n" name;
        List.iter printer lis 
 
let debug_print_var arg = 
  let printer l = 
    match l with (var_name,cls),offset_loc ->
      Printf.printf "%s (%s %d)\n" var_name cls offset_loc
  in
    match arg with clas,vars ->
      Printf.printf "vars for class: %s\n" clas;
      List.iter printer vars 

let array_alloc_func lc= 
(* we need to check for attempts to declare array to large 
 * which is anything > (2^32 - 1) / 4 
 * because we cannot address anything larger then that
 * also, then negative values get treated as large signed int *)
  Printf.printf 
"
func l_array_alloc(siz)
  t.0 = Lt(siz 1073741823)
  if t.0 goto :laa_lbl%d
    Error(\"array alloc out of bounds\")
  laa_lbl%d:
  t.1 = Add(siz 1)
  t.1 = MulS(t.1 4)
  t.0 = HeapAllocZ(t.1)
  if t.0 goto :laa_lbl%d
    Error(\"heap alloc fail\")
  laa_lbl%d: 
  [t.0] = siz
  ret t.0\n
" lc lc (lc + 1) (lc + 1);
  lc + 2
  
