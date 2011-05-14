type token =
  | LPAREN
  | RPAREN
  | LSQPAREN
  | RSQPAREN
  | LBRACE
  | RBRACE
  | SEMICOLON
  | DOT
  | ASSIGN
  | LT
  | PLUS
  | MINUS
  | TIMES
  | COMMA
  | AND
  | NOT
  | BOOLEAN
  | CLASS
  | INTERFACE
  | ELSE
  | EXTENDS
  | FALSE
  | IF
  | WHILE
  | INTEGER
  | LENGTH
  | MAIN
  | NEW
  | PUBLIC
  | RETURN
  | STATIC
  | STRING
  | THIS
  | TRUE
  | PRINT
  | VOID
  | ENDOFFILE
  | INTEGER_LITERAL of (int)
  | IDENTIFIER of (string)

open Parsing;;
# 2 "minijava.mly"
  open Minijava_lexar 
  open Tree_types

  let parse_error s = 
(*    print_endline s;*)
    flush stdout
# 51 "minijava.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LSQPAREN *);
  260 (* RSQPAREN *);
  261 (* LBRACE *);
  262 (* RBRACE *);
  263 (* SEMICOLON *);
  264 (* DOT *);
  265 (* ASSIGN *);
  266 (* LT *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMES *);
  270 (* COMMA *);
  271 (* AND *);
  272 (* NOT *);
  273 (* BOOLEAN *);
  274 (* CLASS *);
  275 (* INTERFACE *);
  276 (* ELSE *);
  277 (* EXTENDS *);
  278 (* FALSE *);
  279 (* IF *);
  280 (* WHILE *);
  281 (* INTEGER *);
  282 (* LENGTH *);
  283 (* MAIN *);
  284 (* NEW *);
  285 (* PUBLIC *);
  286 (* RETURN *);
  287 (* STATIC *);
  288 (* STRING *);
  289 (* THIS *);
  290 (* TRUE *);
  291 (* PRINT *);
  292 (* VOID *);
  293 (* ENDOFFILE *);
    0|]

let yytransl_block = [|
  294 (* INTEGER_LITERAL *);
  295 (* IDENTIFIER *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\006\000\006\000\003\000\003\000\015\000\015\000\
\016\000\017\000\018\000\018\000\019\000\019\000\021\000\021\000\
\023\000\024\000\024\000\020\000\020\000\020\000\025\000\025\000\
\007\000\008\000\010\000\010\000\026\000\026\000\026\000\026\000\
\026\000\009\000\014\000\014\000\027\000\028\000\011\000\012\000\
\013\000\022\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\037\000\037\000\038\000\039\000\040\000\040\000\041\000\041\000\
\029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
\042\000\043\000\044\000\004\000\045\000\049\000\050\000\046\000\
\046\000\047\000\048\000\000\000"

let yylen = "\002\000\
\003\000\017\000\002\000\004\000\004\000\002\000\002\000\002\000\
\002\000\000\000\003\000\002\000\004\000\000\000\001\000\001\000\
\004\000\006\000\004\000\000\000\013\000\000\000\002\000\000\000\
\002\000\003\000\000\000\001\000\001\000\001\000\002\000\000\000\
\001\000\002\000\002\000\000\000\001\000\002\000\001\000\001\000\
\001\000\003\000\001\000\001\000\003\000\006\000\007\000\005\000\
\005\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\000\000\002\000\002\000\002\000\002\000\002\000\003\000\
\001\000\001\000\001\000\004\000\002\000\000\000\003\000\000\000\
\001\000\001\000\001\000\001\000\001\000\002\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\004\000\003\000\001\000\
\001\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\092\000\000\000\084\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\015\000\016\000\000\000\033\000\000\000\030\000\028\000\029\000\
\000\000\000\000\000\000\013\000\000\000\000\000\034\000\000\000\
\000\000\000\000\000\000\000\000\031\000\000\000\017\000\000\000\
\000\000\000\000\000\000\019\000\000\000\000\000\000\000\018\000\
\000\000\000\000\000\000\000\000\000\000\025\000\000\000\000\000\
\023\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\000\000\000\000\037\000\000\000\039\000\040\000\
\041\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\003\000\000\000\043\000\044\000\000\000\000\000\000\000\006\000\
\007\000\008\000\009\000\000\000\038\000\042\000\035\000\000\000\
\000\000\083\000\000\000\085\000\082\000\081\000\076\000\000\000\
\000\000\073\000\074\000\075\000\077\000\079\000\080\000\000\000\
\000\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\000\000\000\000\090\000\000\000\000\000\078\000\088\000\089\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\000\000\
\000\000\000\000\045\000\011\000\000\000\004\000\005\000\002\000\
\091\000\000\000\000\000\000\000\000\000\067\000\000\000\057\000\
\065\000\066\000\060\000\061\000\062\000\063\000\059\000\048\000\
\049\000\000\000\000\000\000\000\087\000\000\000\064\000\000\000\
\000\000\021\000\086\000\047\000\000\000\000\000\046\000\000\000\
\069\000\068\000\000\000\071\000"

let yydgoto = "\002\000\
\004\000\005\000\009\000\111\000\067\000\089\000\023\000\024\000\
\077\000\078\000\079\000\080\000\081\000\090\000\016\000\017\000\
\018\000\025\000\033\000\026\000\051\000\112\000\052\000\057\000\
\031\000\082\000\091\000\092\000\113\000\145\000\146\000\147\000\
\148\000\149\000\150\000\151\000\168\000\169\000\170\000\190\000\
\193\000\114\000\115\000\116\000\117\000\134\000\118\000\119\000\
\135\000\136\000"

let yysindex = "\019\000\
\241\254\000\000\245\254\000\000\021\255\000\000\038\255\245\254\
\025\255\061\255\036\255\000\000\060\255\063\255\245\254\021\255\
\000\000\000\000\058\255\000\000\098\255\000\000\000\000\000\000\
\074\255\245\254\100\255\000\000\082\255\111\255\000\000\063\255\
\113\255\115\255\063\255\126\255\000\000\245\254\000\000\063\255\
\074\255\096\255\130\255\000\000\127\255\129\255\063\255\000\000\
\133\255\245\254\136\255\134\255\245\254\000\000\135\255\063\255\
\000\000\148\255\137\255\134\255\146\255\073\255\155\255\164\255\
\169\255\013\255\143\255\245\254\245\254\073\255\073\255\073\255\
\073\255\000\000\137\255\072\255\000\000\168\255\000\000\000\000\
\000\000\073\255\119\255\119\255\119\255\119\255\119\255\170\255\
\000\000\073\255\000\000\000\000\119\255\179\255\180\255\000\000\
\000\000\000\000\000\000\182\255\000\000\000\000\000\000\119\255\
\119\255\000\000\035\255\000\000\000\000\000\000\000\000\173\255\
\156\255\000\000\000\000\000\000\000\000\000\000\000\000\187\255\
\188\255\181\255\184\255\137\255\000\000\185\255\137\255\137\255\
\189\255\191\255\000\000\193\255\196\255\000\000\000\000\000\000\
\073\255\119\255\248\254\119\255\119\255\119\255\119\255\119\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\073\255\
\192\255\194\255\000\000\000\000\195\255\000\000\000\000\000\000\
\000\000\119\255\198\255\174\255\200\255\000\000\197\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\119\255\074\255\201\255\000\000\073\255\000\000\119\255\
\199\255\000\000\000\000\000\000\202\255\205\255\000\000\119\255\
\000\000\000\000\202\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\165\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\255\000\000\165\255\
\000\000\000\000\000\000\000\000\171\255\000\000\000\000\000\000\
\203\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\255\000\000\000\000\000\000\000\000\007\255\
\203\255\000\000\000\000\000\000\000\000\000\000\206\255\000\000\
\000\000\000\000\000\000\209\255\000\000\000\000\000\000\000\000\
\000\000\000\000\190\255\209\255\000\000\207\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\255\008\255\008\255\
\008\255\000\000\208\255\000\000\000\000\000\000\000\000\000\000\
\000\000\008\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\069\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\255\000\000\000\000\015\255\015\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\203\255\000\000\000\000\000\000\000\000\210\255\
\000\000\000\000\000\000\000\000\213\255\000\000\000\000\000\000\
\000\000\000\000\213\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\201\000\253\255\183\255\000\000\214\255\221\255\
\227\255\073\000\239\255\245\255\002\000\142\000\000\000\000\000\
\000\000\083\000\216\255\234\255\000\000\178\255\163\000\161\000\
\000\000\123\255\000\000\000\000\040\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yytablesize = 222
let yytable = "\007\000\
\045\000\100\000\003\000\164\000\011\000\120\000\121\000\122\000\
\123\000\038\000\022\000\027\000\020\000\036\000\126\000\086\000\
\068\000\166\000\176\000\001\000\010\000\087\000\034\000\069\000\
\050\000\130\000\131\000\006\000\022\000\070\000\006\000\022\000\
\068\000\050\000\043\000\020\000\022\000\036\000\008\000\069\000\
\014\000\071\000\010\000\022\000\010\000\070\000\054\000\072\000\
\188\000\058\000\156\000\006\000\022\000\158\000\159\000\066\000\
\015\000\071\000\076\000\132\000\073\000\012\000\088\000\072\000\
\094\000\095\000\076\000\076\000\076\000\076\000\058\000\066\000\
\058\000\006\000\086\000\058\000\073\000\062\000\076\000\020\000\
\087\000\068\000\058\000\180\000\068\000\068\000\076\000\021\000\
\069\000\013\000\019\000\069\000\069\000\029\000\070\000\063\000\
\064\000\070\000\070\000\185\000\030\000\006\000\032\000\133\000\
\035\000\189\000\071\000\065\000\036\000\071\000\071\000\006\000\
\072\000\195\000\037\000\072\000\072\000\041\000\039\000\104\000\
\066\000\040\000\044\000\066\000\066\000\073\000\042\000\046\000\
\073\000\073\000\047\000\049\000\048\000\076\000\105\000\167\000\
\053\000\055\000\186\000\059\000\106\000\062\000\096\000\097\000\
\098\000\099\000\107\000\056\000\076\000\061\000\075\000\108\000\
\109\000\020\000\103\000\083\000\110\000\006\000\138\000\063\000\
\064\000\021\000\125\000\139\000\084\000\140\000\141\000\142\000\
\143\000\085\000\144\000\065\000\093\000\102\000\137\000\006\000\
\124\000\165\000\076\000\171\000\172\000\173\000\174\000\175\000\
\154\000\127\000\128\000\129\000\152\000\153\000\155\000\157\000\
\161\000\182\000\160\000\162\000\163\000\184\000\177\000\181\000\
\179\000\014\000\178\000\183\000\187\000\191\000\194\000\024\000\
\022\000\032\000\027\000\070\000\036\000\010\000\072\000\192\000\
\028\000\101\000\060\000\010\000\074\000\196\000"

let yycheck = "\003\000\
\041\000\075\000\018\001\137\000\008\000\084\000\085\000\086\000\
\087\000\032\000\014\000\015\000\006\001\006\001\093\000\003\001\
\059\000\026\001\152\000\001\000\006\001\009\001\026\000\059\000\
\047\000\104\000\105\000\039\001\032\000\059\000\039\001\035\000\
\075\000\056\000\038\000\029\001\040\000\030\001\018\001\075\000\
\005\001\059\000\005\001\047\000\030\001\075\000\050\000\059\000\
\182\000\053\000\124\000\039\001\056\000\127\000\128\000\059\000\
\021\001\075\000\062\000\025\001\059\000\037\001\066\000\075\000\
\068\000\069\000\070\000\071\000\072\000\073\000\002\001\075\000\
\004\001\039\001\003\001\007\001\075\000\005\001\082\000\017\001\
\009\001\124\000\014\001\162\000\127\000\128\000\090\000\025\001\
\124\000\029\001\031\001\127\000\128\000\036\001\124\000\023\001\
\024\001\127\000\128\000\178\000\003\001\039\001\029\001\107\000\
\005\001\184\000\124\000\035\001\027\001\127\000\128\000\039\001\
\124\000\192\000\004\001\127\000\128\000\035\000\006\001\001\001\
\124\000\007\001\040\000\127\000\128\000\124\000\001\001\032\001\
\127\000\128\000\001\001\003\001\006\001\137\000\016\001\139\000\
\004\001\002\001\179\000\005\001\022\001\005\001\070\000\071\000\
\072\000\073\000\028\001\014\001\152\000\002\001\005\001\033\001\
\034\001\017\001\082\000\001\001\038\001\039\001\003\001\023\001\
\024\001\025\001\090\000\008\001\001\001\010\001\011\001\012\001\
\013\001\001\001\015\001\035\001\030\001\006\001\002\001\039\001\
\007\001\138\000\182\000\140\000\141\000\142\000\143\000\144\000\
\004\001\007\001\007\001\006\001\002\001\002\001\007\001\007\001\
\002\001\020\001\006\001\003\001\001\001\001\001\007\001\002\001\
\006\001\037\001\009\001\004\001\004\001\007\001\002\001\002\001\
\006\001\039\001\002\001\002\001\006\001\006\001\002\001\014\001\
\016\000\076\000\056\000\030\001\060\000\195\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LSQPAREN\000\
  RSQPAREN\000\
  LBRACE\000\
  RBRACE\000\
  SEMICOLON\000\
  DOT\000\
  ASSIGN\000\
  LT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  COMMA\000\
  AND\000\
  NOT\000\
  BOOLEAN\000\
  CLASS\000\
  INTERFACE\000\
  ELSE\000\
  EXTENDS\000\
  FALSE\000\
  IF\000\
  WHILE\000\
  INTEGER\000\
  LENGTH\000\
  MAIN\000\
  NEW\000\
  PUBLIC\000\
  RETURN\000\
  STATIC\000\
  STRING\000\
  THIS\000\
  TRUE\000\
  PRINT\000\
  VOID\000\
  ENDOFFILE\000\
  "

let yynames_block = "\
  INTEGER_LITERAL\000\
  IDENTIFIER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'main_class) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'type_declaration) in
    Obj.repr(
# 67 "minijava.mly"
  (
    Goal (_1,_2)
  )
# 340 "minijava.ml"
               : Tree_types.t_goal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 15 : 'identifier) in
    let _12 = (Parsing.peek_val __caml_parser_env 5 : 'identifier) in
    let _15 = (Parsing.peek_val __caml_parser_env 2 : 'var_dec_mul_state) in
    Obj.repr(
# 78 "minijava.mly"
  ( 
    match _15 with 
    | a, b -> Main_class (_2,_12,a,b) 
  )
# 352 "minijava.ml"
               : 'main_class))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'identifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_dec_mul_state_follow) in
    Obj.repr(
# 91 "minijava.mly"
    (
      match _2 with
      | ONE (a,b,c) -> 
        (match _1 with Identifier iden ->
           ((Var_dec (Type (Identifier_type iden),a))::b, c) )
      | TWO (a,b,c) -> (b, (State (Assign_state (_1,a))::c))
      | THREE (a,b,c) ->
        ( match a with 
          | ar1,ar2 ->
            (b, (State (Array_assign_state (_1,ar1,ar2))::c))
        )
      | _ -> raise (Error "mly var_decl_mul_state")
    )
# 372 "minijava.ml"
               : 'var_dec_mul_state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'boolean_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'var_dec_mul_state) in
    Obj.repr(
# 106 "minijava.mly"
    ( 
      match _4 with 
      | a,b -> ((Var_dec (Type _1,_2))::a, b)  
    )
# 384 "minijava.ml"
               : 'var_dec_mul_state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'integer_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'var_dec_mul_state) in
    Obj.repr(
# 111 "minijava.mly"
    ( 
      match _4 with
      | a,b -> ((Var_dec (Type _1,_2))::a, b)
    )
# 396 "minijava.ml"
               : 'var_dec_mul_state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mul_statement) in
    Obj.repr(
# 117 "minijava.mly"
    ( ([], (State _1)::_2) )
# 404 "minijava.ml"
               : 'var_dec_mul_state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'if_statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mul_statement) in
    Obj.repr(
# 119 "minijava.mly"
    ( ([], (State _1)::_2) )
# 412 "minijava.ml"
               : 'var_dec_mul_state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'while_statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mul_statement) in
    Obj.repr(
# 121 "minijava.mly"
    ( ([], (State _1)::_2) )
# 420 "minijava.ml"
               : 'var_dec_mul_state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'print_statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mul_statement) in
    Obj.repr(
# 123 "minijava.mly"
    ( ([], (State _1)::_2) )
# 428 "minijava.ml"
               : 'var_dec_mul_state))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "minijava.mly"
    ( [],[] )
# 434 "minijava.ml"
               : 'var_dec_mul_state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'var_dec_mul_state) in
    Obj.repr(
# 131 "minijava.mly"
    (
      (* $3 is a two-tuple *)
      match _3 with 
      | a,b -> ONE (_1,a,b)    
    )
# 446 "minijava.ml"
               : 'var_dec_mul_state_follow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pre_id_assignment_statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mul_statement) in
    Obj.repr(
# 138 "minijava.mly"
    (
      match _1 with 
      | ONE a -> TWO (a,[],_2)
      | TWO a -> THREE (a,[],_2) 
      | _ -> raise (Error "mly var_dec_mul_state_follow 2")
    )
# 459 "minijava.ml"
               : 'var_dec_mul_state_follow))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_declaration_follow) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_declaration) in
    Obj.repr(
# 148 "minijava.mly"
    ( 
      match _3 with
      | ONE (a,b) -> (Type_dec (Class_dec (_2,a,b)))::_4
      | TWO (a,b,c) -> (Type_dec (Class_extends_dec (_2,a,b,c)))::_4
      | _ -> raise (Error "mly type_declaration")
    )
# 473 "minijava.ml"
               : 'type_declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "minijava.mly"
    ( [] )
# 479 "minijava.ml"
               : 'type_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'class_declaration) in
    Obj.repr(
# 160 "minijava.mly"
  ( ONE _1 )
# 486 "minijava.ml"
               : 'type_declaration_follow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'class_extends_declaration) in
    Obj.repr(
# 162 "minijava.mly"
  ( TWO _1 )
# 493 "minijava.ml"
               : 'type_declaration_follow))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var_declaration) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'method_declaration) in
    Obj.repr(
# 168 "minijava.mly"
  ( _2,_3 )
# 501 "minijava.ml"
               : 'class_declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'var_declaration) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'method_declaration) in
    Obj.repr(
# 175 "minijava.mly"
  ( _2,_4,_5 )
# 510 "minijava.ml"
               : 'class_extends_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'mtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'var_declaration) in
    Obj.repr(
# 180 "minijava.mly"
    ( (Var_dec (_1,_2))::_4 )
# 519 "minijava.ml"
               : 'var_declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 182 "minijava.mly"
    ( [] )
# 525 "minijava.ml"
               : 'var_declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 11 : 'mtype) in
    let _3 = (Parsing.peek_val __caml_parser_env 10 : 'identifier) in
    let _5 = (Parsing.peek_val __caml_parser_env 8 : 'formal_parameter_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 5 : 'var_dec_mul_state) in
    let _10 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    let _13 = (Parsing.peek_val __caml_parser_env 0 : 'method_declaration) in
    Obj.repr(
# 192 "minijava.mly"
    ( 
      match _8 with
      | var_list,state_list ->
       (Method_dec (_2,_3,_5,var_list,state_list,_10))::_13 
    )
# 541 "minijava.ml"
               : 'method_declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 198 "minijava.mly"
    ( [] )
# 547 "minijava.ml"
               : 'method_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formal_parameter) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formal_parameter_rest) in
    Obj.repr(
# 203 "minijava.mly"
    ( _1::_2 )
# 555 "minijava.ml"
               : 'formal_parameter_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 205 "minijava.mly"
    ( [] )
# 561 "minijava.ml"
               : 'formal_parameter_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'mtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'identifier) in
    Obj.repr(
# 210 "minijava.mly"
    ( Formal_param (_1,_2) )
# 569 "minijava.ml"
               : 'formal_parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_parameter) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_parameter_rest) in
    Obj.repr(
# 215 "minijava.mly"
    ( _2::_3 )
# 577 "minijava.ml"
               : 'formal_parameter_rest))
; (fun __caml_parser_env ->
    Obj.repr(
# 217 "minijava.mly"
    ( [] )
# 583 "minijava.ml"
               : 'formal_parameter_rest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'boolean_type) in
    Obj.repr(
# 223 "minijava.mly"
    ( Type _1 )
# 590 "minijava.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'integer_type) in
    Obj.repr(
# 225 "minijava.mly"
    ( Type _1 )
# 597 "minijava.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'identifier) in
    Obj.repr(
# 227 "minijava.mly"
    ( match _1 with Identifier a -> Type (Identifier_type a) )
# 604 "minijava.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 232 "minijava.mly"
    ( Array_type )
# 610 "minijava.ml"
               : 'array_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 234 "minijava.mly"
    ( Integer_type )
# 616 "minijava.ml"
               : 'array_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 239 "minijava.mly"
  ( Boolean_type )
# 622 "minijava.ml"
               : 'boolean_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'array_type) in
    Obj.repr(
# 244 "minijava.mly"
  ( _2 )
# 629 "minijava.ml"
               : 'integer_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mul_statement) in
    Obj.repr(
# 249 "minijava.mly"
    ( 
      _1::_2 
    )
# 639 "minijava.ml"
               : 'mul_statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 253 "minijava.mly"
    ( [] )
# 645 "minijava.ml"
               : 'mul_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 258 "minijava.mly"
    ( 
      State _1
    )
# 654 "minijava.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'identifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pre_id_assignment_statement) in
    Obj.repr(
# 263 "minijava.mly"
    (
      match _2 with
      | ONE a -> State (Assign_state (_1,a))
      | TWO a ->
        (
          match a with
          | b,c -> State (Array_assign_state (_1,b,c))
        )
      | _ -> raise (Error "mly block2")
    )
# 671 "minijava.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'if_statement) in
    Obj.repr(
# 274 "minijava.mly"
    ( State _1 )
# 678 "minijava.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'while_statement) in
    Obj.repr(
# 276 "minijava.mly"
    ( State _1 )
# 685 "minijava.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'print_statement) in
    Obj.repr(
# 278 "minijava.mly"
    ( State _1 )
# 692 "minijava.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mul_statement) in
    Obj.repr(
# 283 "minijava.mly"
    ( Block _2 )
# 699 "minijava.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_statement) in
    Obj.repr(
# 288 "minijava.mly"
    ( ONE _1 )
# 706 "minijava.ml"
               : 'pre_id_assignment_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'array_assignment_statement) in
    Obj.repr(
# 290 "minijava.mly"
    ( TWO _1 )
# 713 "minijava.ml"
               : 'pre_id_assignment_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 295 "minijava.mly"
    ( _2 )
# 720 "minijava.ml"
               : 'assignment_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 300 "minijava.mly"
    ( _2, _5 )
# 728 "minijava.ml"
               : 'array_assignment_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 305 "minijava.mly"
  ( 
    If_state (_3,_5,_7)
  )
# 739 "minijava.ml"
               : 'if_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 312 "minijava.mly"
  (
    While_state (_3,_5)
  )
# 749 "minijava.ml"
               : 'while_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    Obj.repr(
# 319 "minijava.mly"
  (
    Print_state _3
  )
# 758 "minijava.ml"
               : 'print_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'primary_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression_follow) in
    Obj.repr(
# 327 "minijava.mly"
  ( 
    match _2 with 
      | ONE a -> Expr (And_expr (_1,a))
      | TWO a -> Expr (Compare_expr (_1,a))
      | THREE a -> Expr (Plus_expr (_1,a))
      | FOUR a -> Expr (Minus_expr (_1,a))
      | FIVE a -> Expr (Times_expr (_1,a))
      | SIX a -> Expr (Array_lookup (_1,a))
      | SEVEN a -> (
        match a with 
        | ONE b -> Expr (Array_length _1)
        | TWO (b,c) -> Expr (Message_send (_1,b,c))
        | _ -> raise (Error "mly expression") )
      | EIGHT a -> Expr (Primary_expr_expr _1)
      | _ -> raise (Error "mly expression2")
  )
# 781 "minijava.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 349 "minijava.mly"
  ( ONE _1 )
# 788 "minijava.ml"
               : 'expression_follow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compare_expression) in
    Obj.repr(
# 351 "minijava.mly"
  ( TWO _1 )
# 795 "minijava.ml"
               : 'expression_follow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plus_expression) in
    Obj.repr(
# 353 "minijava.mly"
  ( THREE _1 )
# 802 "minijava.ml"
               : 'expression_follow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'minus_expression) in
    Obj.repr(
# 355 "minijava.mly"
  ( FOUR _1 )
# 809 "minijava.ml"
               : 'expression_follow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'times_expression) in
    Obj.repr(
# 357 "minijava.mly"
  ( FIVE _1 )
# 816 "minijava.ml"
               : 'expression_follow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'array_lookup) in
    Obj.repr(
# 359 "minijava.mly"
  ( SIX _1 )
# 823 "minijava.ml"
               : 'expression_follow))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'array_length_message_send) in
    Obj.repr(
# 361 "minijava.mly"
  ( SEVEN _2 )
# 830 "minijava.ml"
               : 'expression_follow))
; (fun __caml_parser_env ->
    Obj.repr(
# 363 "minijava.mly"
  ( EIGHT () )
# 836 "minijava.ml"
               : 'expression_follow))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expression) in
    Obj.repr(
# 368 "minijava.mly"
  ( _2 )
# 843 "minijava.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expression) in
    Obj.repr(
# 373 "minijava.mly"
  ( _2 )
# 850 "minijava.ml"
               : 'compare_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expression) in
    Obj.repr(
# 378 "minijava.mly"
  ( _2 )
# 857 "minijava.ml"
               : 'plus_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expression) in
    Obj.repr(
# 383 "minijava.mly"
  ( _2 )
# 864 "minijava.ml"
               : 'minus_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expression) in
    Obj.repr(
# 388 "minijava.mly"
  ( _2 )
# 871 "minijava.ml"
               : 'times_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'primary_expression) in
    Obj.repr(
# 393 "minijava.mly"
  ( _2 )
# 878 "minijava.ml"
               : 'array_lookup))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'array_length) in
    Obj.repr(
# 398 "minijava.mly"
  ( ONE [] )
# 885 "minijava.ml"
               : 'array_length_message_send))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'message_send) in
    Obj.repr(
# 400 "minijava.mly"
  ( TWO _1 )
# 892 "minijava.ml"
               : 'array_length_message_send))
; (fun __caml_parser_env ->
    Obj.repr(
# 404 "minijava.mly"
  ( )
# 898 "minijava.ml"
               : 'array_length))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression_list) in
    Obj.repr(
# 409 "minijava.mly"
    ( _1,_3 )
# 906 "minijava.ml"
               : 'message_send))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression_rest) in
    Obj.repr(
# 414 "minijava.mly"
    ( _1::_2 )
# 914 "minijava.ml"
               : 'expression_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 416 "minijava.mly"
    ( [] )
# 920 "minijava.ml"
               : 'expression_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression_rest) in
    Obj.repr(
# 421 "minijava.mly"
    ( _2::_3 )
# 928 "minijava.ml"
               : 'expression_rest))
; (fun __caml_parser_env ->
    Obj.repr(
# 423 "minijava.mly"
    ( [] )
# 934 "minijava.ml"
               : 'expression_rest))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'integer_literal) in
    Obj.repr(
# 428 "minijava.mly"
    ( Primary_expr _1 )
# 941 "minijava.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'true_literal) in
    Obj.repr(
# 430 "minijava.mly"
    ( Primary_expr _1 )
# 948 "minijava.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'false_literal) in
    Obj.repr(
# 432 "minijava.mly"
    ( Primary_expr _1 )
# 955 "minijava.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'identifier) in
    Obj.repr(
# 434 "minijava.mly"
    ( 
      match _1 with Identifier a -> 
        Primary_expr (Identifier_primary a) 
    )
# 965 "minijava.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'this_expression) in
    Obj.repr(
# 439 "minijava.mly"
    ( Primary_expr _1 )
# 972 "minijava.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'allocation_expression) in
    Obj.repr(
# 441 "minijava.mly"
    ( Primary_expr _2 )
# 979 "minijava.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'not_expression) in
    Obj.repr(
# 443 "minijava.mly"
    ( Primary_expr _1 )
# 986 "minijava.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bracket_expression) in
    Obj.repr(
# 445 "minijava.mly"
    ( Primary_expr _1 )
# 993 "minijava.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 450 "minijava.mly"
  (
    Int_literal _1 
  )
# 1002 "minijava.ml"
               : 'integer_literal))
; (fun __caml_parser_env ->
    Obj.repr(
# 457 "minijava.mly"
  (
    True_literal 
  )
# 1010 "minijava.ml"
               : 'true_literal))
; (fun __caml_parser_env ->
    Obj.repr(
# 464 "minijava.mly"
  (
    False_literal 
  )
# 1018 "minijava.ml"
               : 'false_literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 471 "minijava.mly"
  (
    Identifier _1
  )
# 1027 "minijava.ml"
               : 'identifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 478 "minijava.mly"
  (
    This_expr 
  )
# 1035 "minijava.ml"
               : 'this_expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 485 "minijava.mly"
    ( Array_alloc_expr _3 )
# 1042 "minijava.ml"
               : 'array_allocation_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'identifier) in
    Obj.repr(
# 490 "minijava.mly"
    ( Alloc_expr _1 )
# 1049 "minijava.ml"
               : 'obj_allocation_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'array_allocation_expression) in
    Obj.repr(
# 495 "minijava.mly"
  ( _1 )
# 1056 "minijava.ml"
               : 'allocation_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'obj_allocation_expression) in
    Obj.repr(
# 497 "minijava.mly"
  ( _1 )
# 1063 "minijava.ml"
               : 'allocation_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 502 "minijava.mly"
  ( 
    Not_expr _2
  )
# 1072 "minijava.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 509 "minijava.mly"
  ( 
    Bracket_expr _2
  )
# 1081 "minijava.ml"
               : 'bracket_expression))
(* Entry goal *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let goal (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tree_types.t_goal)
;;
# 514 "minijava.mly"

# 1108 "minijava.ml"
