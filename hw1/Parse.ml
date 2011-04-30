(* 
  Nicholas Wong
  UID: 403-600-605
  UCLA 
  CS 132
   SPRING 2011
*)

type terminal =
  | NUM 
  | BINOP
  | INCROP
  | LPAREN
  | RPAREN
  | DOLLAR
  | EOF

exception BadInput of char * int * int * int
exception ParseError of terminal * string * string * int


(* temp is a string 55 characters long *)
let temp = "********************************************************"


(* I like working with lists, more then I like strings, and
   converting adds fairly minimal overhead so no harm done :) *)

(* convert a string of characters of length "len" to a list of characters 
   in the revese of the original order *)
let string_to_list str len = 
  let rec string_to_list_c so_far pos m_len str =
    if pos = m_len then
      so_far
    else
      string_to_list_c (str.[pos]::so_far) (pos + 1) m_len str
  in 
    string_to_list_c [] 0 len str

(*  Read all input from stdin until EOF, return it as a list of characters *)
let rec read_all so_far =
  match input stdin temp 0 50 with
  | 0   -> so_far
  | len -> read_all so_far@(string_to_list temp len)


(*  This is rather ad-hoc, especially since it has to deal with algebraic
    operators on three different occassion 

    Its worth noting that if a comment falls between two '+' symbols, then
    those '+'s are seperate tokens, since a comment can only end when it hits
    a newline.

    take a list of characters and the current line number ln (initially 1),
    return the next token in the list and the list
    of chars with that token removed
*)
let get_token input_lst ln = 
  let rec get_token_c p lst ln_c = 
    if lst = [] then
      if p = '+' then
        BINOP,"+",[],ln_c
      else if p = '-' then
        BINOP,"-",[],ln_c
      else
        EOF,"E",[],ln_c
    else
      match p with 
        (* check to find end of comments *)
      | '#' ->
        if (List.hd lst) = '\n' then
          get_token_c ' ' (List.tl lst) (ln_c + 1)
        else
          get_token_c '#' (List.tl lst) ln_c
        (* algebraic operations *)
      | '+' ->
        if (List.hd lst) = '+' then
          INCROP,"++",(List.tl lst),ln_c
        else
          BINOP,"+",lst,ln_c
      | '-' ->
        if (List.hd lst) = '-' then
          INCROP,"--",(List.tl lst),ln_c
        else
          BINOP,"-",lst,ln_c
      | _ ->
        (* all other terminals *)
        match List.hd lst with
          (* beginning of comment *)
        | '#' -> get_token_c '#' (List.tl lst) ln_c
          (* whitespace *)
        | ' ' | '\t' -> get_token_c ' ' (List.tl lst) ln_c
        | '\n' -> get_token_c ' ' (List.tl lst) (ln_c + 1)
          (* algebraic operations *)
        | '+' -> get_token_c '+' (List.tl lst) ln_c
        | '-' -> get_token_c '-' (List.tl lst) ln_c
          (* parenthesis *)
        | '(' -> LPAREN,"(",(List.tl lst),ln_c
        | ')' -> RPAREN,")",(List.tl lst),ln_c    
          (* numbers *)
        | '0' -> NUM,"0",(List.tl lst),ln_c
        | '1' -> NUM,"1",(List.tl lst),ln_c
        | '2' -> NUM,"2",(List.tl lst),ln_c
        | '3' -> NUM,"3",(List.tl lst),ln_c
        | '4' -> NUM,"4",(List.tl lst),ln_c
        | '5' -> NUM,"5",(List.tl lst),ln_c
        | '6' -> NUM,"6",(List.tl lst),ln_c
        | '7' -> NUM,"7",(List.tl lst),ln_c
        | '8' -> NUM,"8",(List.tl lst),ln_c
        | '9' -> NUM,"9",(List.tl lst),ln_c
          (* field reference operator *)
        | '$' -> DOLLAR,"$",(List.tl lst),ln_c
        | a   -> raise (BadInput (a, ln_c, 0, 0)) 
  in 
    get_token_c ' ' input_lst ln

(* this was actually never really used *)
let num lst ln = 
  match get_token lst ln with
  | g_type,g_str,g_lst,g_ln ->
    match g_type with 
    | NUM -> g_lst,g_ln,g_str
    | _   -> raise (ParseError (g_type, g_str,"num",g_ln))

(* binop -> + | - 
    first: + -
    follow: INCROP, F, NUM, LPAREN, EOF *)
let binop lst ln expr_func = 
    match get_token lst ln with
    | g_type,g_str,g_lst,g_ln ->
      match g_type with 
      | BINOP -> g_lst,g_ln,[g_str]
      | DOLLAR | LPAREN | NUM | INCROP  -> lst,ln,[]
      | _ -> raise (ParseError (g_type, g_str,"binop", g_ln)) 
   
(* postinc -> _++ postinc | _-- postinc | epsilon 
    first: INCROP
    follow: INCROP, BINOP, LPAREN, RPAREN, EOF *)
let rec postinc lst ln expr_func =
  match get_token lst ln with
  | g_type,g_str,g_lst,g_ln ->
    match g_type with
    | INCROP  -> 
      (
        match postinc g_lst g_ln expr_func with
        | h_lst,h_ln,tree -> h_lst,h_ln,(String.concat "" ["_";g_str])::tree
      )
    | BINOP | DOLLAR | LPAREN | NUM | RPAREN | EOF -> lst,ln,[]
    (*| _ -> raise (ParseError (g_type, g_str, "postinc"))*)

(* preinc -> ++_ preinc | --_ princ | epsilon 
   first: INCROP
   follow: DOLLAR, NUM, LPAREN *)
let rec preinc lst ln expr_func = 
  match get_token lst ln with
  | g_type,g_str,g_lst,g_ln ->
    match g_type with
    | INCROP  -> 
      (
        match preinc g_lst g_ln expr_func with
        | h_lst,h_ln,tree -> h_lst,h_ln,tree@[(String.concat "" [g_str;"_"])]
      )
    | DOLLAR | LPAREN | NUM  -> lst,ln,[]
    | _ -> raise (ParseError (g_type, g_str, "preinc",g_ln))

(* e1 -> num | (expr)
   first: NUM, LPAREN
   NOT NULLABLE  *)
let e1 lst ln expr_func = 
  match get_token lst ln with
  | g_type,g_str,g_lst,g_ln ->
    match g_type with 
    | LPAREN  -> 
      (
        match expr_func g_lst g_ln with
        | l_lst,l_ln,tree ->
          match get_token l_lst l_ln with
          | h_type,h_str,h_lst,h_ln ->
            match h_type with
            | RPAREN  -> h_lst,h_ln,tree
            | _       -> raise (ParseError (h_type, h_str, "e1_rparen",h_ln))
      )
    | NUM -> g_lst,g_ln,[g_str]
    | _   -> raise (ParseError (g_type, g_str, "e1_catch",g_ln))

(* lvalue -> F lvalue | epsilon 
   first: DOLLAR 
   follow: LPAREN, NUM, INCROP *)
let rec lvalue lst ln expr_func =
  match get_token lst ln with
  | g_type,g_str,g_lst,g_ln ->
    match g_type with
    | DOLLAR -> 
      (
        match lvalue g_lst g_ln expr_func with
        | h_lst,h_ln,h_tree -> h_lst,h_ln,g_str::h_tree 
      )
    | INCROP | LPAREN | NUM -> lst,ln,[]
    | _ -> raise (ParseError (g_type, g_str, "lvalue",g_ln))

(* e2 -> preinc lvalue e2 | epsilon 
   first: DOLLAR, INCROP
   follow: LPAREN, NUM *)
let rec e2 lst ln expr_func = 
  match get_token lst ln with
  | g_type,g_str,g_lst,g_ln ->
    match g_type with
    | DOLLAR ->
      (
        (* seperating the parse results for "dollars" and "preincs"
           is purely so that we can more easily generate postfix notation *)
        match lvalue lst ln expr_func with
        | h_lst,h_ln,h_tree ->
          match e2 h_lst h_ln expr_func with
          | i_lst,i_ln,i_dols,i_incs -> i_lst,i_ln,h_tree@i_dols,i_incs
      )
    | INCROP ->
      (
        match preinc lst ln expr_func with
        | h_lst,h_ln,h_tree ->
          match e2 h_lst h_ln expr_func with
          | i_lst,i_ln,i_dols,i_incs -> i_lst,i_ln,i_dols,i_incs@h_tree
      )
    | LPAREN | NUM -> lst,ln,[],[]
    | _ -> raise (ParseError (g_type, g_str, "lvalue",g_ln))


(* e3 -> e2 e1 postinc 
   first: DOLLAR, LPAREN, NUM,INCROP
    NOT NULLABLE *)
let rec e3 lst ln expr_func = 
  match get_token lst ln with
  | g_type,g_str,g_lst,g_ln ->
    match g_type with 
    | DOLLAR | LPAREN | NUM | INCROP -> 
      (
        match e2 lst ln expr_func with 
        | h_lst,h_ln,h_dols,h_incs-> 
          match e1 h_lst h_ln expr_func with
          | i_lst,i_ln,i_tree ->
            match postinc i_lst i_ln expr_func with
            | j_lst,j_ln,j_tree -> j_lst,j_ln,i_tree@h_dols@j_tree@h_incs
      )
    | _ -> raise (ParseError (g_type, g_str, "e3",g_ln))

(* e4 -> binop expr | epsilon 
   first: BINOP, LPAREN, NUM, DOLLAR, INCROP
   follow: RPAREN, EOF *)
let rec e4 lst ln expr_func = 
  match get_token lst ln with
  | g_type,g_str,g_lst,g_ln ->
    match g_type with  
    | EOF -> g_lst,g_ln,[] 
    | BINOP | LPAREN | NUM | DOLLAR | INCROP -> 
      (
        match binop lst ln expr_func with 
        | h_lst,h_ln,h_tree ->
          match e3 h_lst h_ln expr_func with
          | i_lst,i_ln,i_tree ->
            match e4 i_lst i_ln expr_func with
            | j_lst,j_ln,j_tree -> j_lst,j_ln,i_tree@h_tree@j_tree
      )
    | RPAREN -> lst,ln,[]
    (*| _ -> raise (ParseError (g_type,g_str, "4")) *)

(* expr -> e3 e4 
   first: NUM, LPAREN, INCROP, DOLLAR 
   NOT NULLABLE *)
let rec expr lst ln = 
  match get_token lst ln with 
  | g_type,g_str,g_lst,g_ln ->
    match g_type with 
    | INCROP | DOLLAR | LPAREN | NUM -> 
      ( 
        match e3 lst ln expr with 
        | h_lst,h_ln,h_tree ->
          match e4 h_lst h_ln expr with
          | i_lst,i_ln,i_tree -> i_lst,i_ln,h_tree@i_tree
      )
    | EOF -> g_lst, g_ln, []
    | _ -> raise (ParseError (g_type, g_str, "expr", g_ln)) 


(* the start production rule, returns the parse tree only *)
let start lst ln = 
  match expr lst ln with
  | _,_,tree -> tree@["_"]

(* take the parse tree, and print it out in prefix *)
let rec print_tree = function
  | [] -> print_string "\n"
  | hd::tl -> 
    print_string (String.concat "" [hd;" "]);
    print_tree tl


(* RUN !!! *)
let run = 
  try 
    print_tree (start (List.rev (read_all [])) 1);
    print_string "Expression parsed successfully\n"
  with 
    | ParseError (_,_,_,err_ln) | BadInput (_,err_ln,_,_) -> 
      Printf.printf "Parse error in line %d\n" err_ln
    | _ ->
      Printf.printf "Something really (really) bad happened\n"
    
