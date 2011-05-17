%{
  open Minijava_lexar 
  open Otree
  open Other_funcs
  let parse_error s = 
(*    print_endline s;*)
    flush stdout
%}

%token
  LPAREN     /* "(" */
  RPAREN     /* ")" */
  LSQPAREN   /* "[" */
  RSQPAREN   /* "]" */
  LBRACE     /* "{" */
  RBRACE     /* "}" */
  SEMICOLON  /* ";" */
  DOT        /* "." */
  ASSIGN     /* "=" */
  LT         /* "<" */
  PLUS       /* "+" */
  MINUS      /* "-" */
  TIMES      /* "*" */
  COMMA      /* "," */        
  AND        /* "&&"  */  
  NOT        /* "!"  */  
  BOOLEAN    /* "boolean"  */  
  CLASS      /* "class" */
  INTERFACE  /* "interface" */
  ELSE       /* "else" */
  EXTENDS    /* "extends" */
  FALSE      /* "false" */
  IF         /* "if" */
  WHILE      /* "while" */
  INTEGER    /* "int"  */  
  LENGTH     /* "length" */
  MAIN       /* "main" */
  NEW        /* "new" */
  PUBLIC     /* "public" */
  RETURN     /* "return" */
  STATIC     /* "static" */
  STRING     /* "String" */
  THIS       /* "this" */
  TRUE       /* "true" */
  PRINT      /* "System.out.println" */
  VOID       /* "void"  */        

%token ENDOFFILE
 
/* literals */
%token <int> INTEGER_LITERAL  /* ( ["1"-"9"] (["0"-"9"])* | "0" ) */

/* identifiers */
%token <string>  IDENTIFIER 


%start goal
%type <Otree.n_goal> goal 

/************************************
 * The MiniJava Grammar Starts Here *
 ************************************/
%%

goal: 
  main_class type_declaration ENDOFFILE
  {
    new n_goal $1 $2
  }
;

/* lookahead 2 */
main_class:
  CLASS identifier LBRACE 
    PUBLIC STATIC VOID MAIN LPAREN STRING LSQPAREN RSQPAREN identifier RPAREN 
    LBRACE var_dec_mul_state RBRACE
  RBRACE
  { 
    match $15 with 
    | a, b -> new n_main_class $2 $12 a b 
    (*| _ -> raise (Error "mly main_class") *)
  }
;

// var_dec -> m_type -> identifier
// mul_state -> statement -> identifier
var_dec_mul_state:
  | identifier var_dec_mul_state_follow 
    {
      match $2 with
      | ONE (a,b,c) -> ((new n_var_declaration (new n_type (FOUR $1)) a)::b, c)
      | TWO (a,b,c) -> 
        (b, (new n_statement (TWO (new n_assignment_statement $1 a))::c))
      | THREE (a,b,c) ->
        ( match a with 
        | ar1,ar2 ->
          (b, (new n_statement 
            (THREE (new n_array_assignment_statement $1 ar1 ar2))::c)) )
      | _ -> raise (Error "mly var_decl_mul_state")
    }
  /* var_declaration */
  | boolean_type identifier SEMICOLON var_dec_mul_state
    { 
      match $4 with 
      | a,b -> ((new n_var_declaration (new n_type (TWO $1)) $2)::a, b)  
      (*| _ -> raise (Error "nly var_dec_mul_state") *)
    }
  | integer_type identifier SEMICOLON var_dec_mul_state
    { 
      match $4 with
      | a,b -> ((new n_var_declaration (new n_type $1) $2)::a, b)
      (*| _ -> raise (Error "nly var_dec_mul_state 2") *)
    }
  /* mul_statement */
  | block mul_statement
    { ([], (new n_statement (ONE $1))::$2) }
  | if_statement mul_statement
    { ([], (new n_statement (FOUR $1))::$2) }
  | while_statement mul_statement
    { ([], (new n_statement (FIVE $1))::$2) }
  | print_statement mul_statement
    { ([], (new n_statement (SIX $1))::$2) }
  | 
    { [],[] }
;
 
var_dec_mul_state_follow:
  /* var_declaration */
  | identifier SEMICOLON var_dec_mul_state 
    {
      (* $3 is a two-tuple *)
      match $3 with 
      | a,b -> ONE ($1,a,b)    
      (*| _ -> raise (Error "mly var_dec_mul_state_follow") *)
    }
  /* mul_statement */
  | pre_id_assignment_statement mul_statement 
    {
      match $1 with 
      | ONE a -> TWO (a,[],$2)
      | TWO a -> THREE (a,[],$2) 
      | _ -> raise (Error "mly var_dec_mul_state_follow 2")
    }
;

type_declaration:
  CLASS identifier type_declaration_follow type_declaration
    { 
      let ne = new n_type_declaration in 
        match $3 with
        | ONE (a,b) -> (ne (ONE (new n_class_declaration $2 a b)))::$4
        | TWO (a,b,c) -> (ne (TWO (new n_class_extends_declaration $2 a b c)))::$4
        | _ -> raise (Error "mly type_declaration")
    }
  |
    { [] }
;

type_declaration_follow:
  class_declaration 
  { ONE $1 }
  | class_extends_declaration 
  { TWO $1 }
;

class_declaration:
  /* CLASS identifier  */
  LBRACE var_declaration method_declaration RBRACE
  { $2,$3 }
;

class_extends_declaration:
  /* CLASS identifier */
  EXTENDS identifier
    LBRACE var_declaration method_declaration RBRACE
  { $2,$4,$5 }
;

var_declaration: 
  mtype identifier SEMICOLON var_declaration
    { (new n_var_declaration $1 $2)::$4 }
  | 
    { [] }
;  

// lookahead 2
method_declaration:
  | PUBLIC mtype identifier 
  LPAREN formal_parameter_list RPAREN
  LBRACE var_dec_mul_state
    RETURN expression SEMICOLON 
  RBRACE method_declaration
    { 
      match $8 with
      | var_list,state_list ->
        (new n_method_declaration $2 $3 $5 var_list state_list $10)::$13 
      (*| _ -> raise (Error "mly method_declaration")*)
    }
  |    
    { [] }
;

formal_parameter_list:
  formal_parameter formal_parameter_rest
    { SYM (new n_formal_parameter_list $1 $2) }
  | 
    { NULL }
;

formal_parameter:
  mtype identifier
    { new n_formal_parameter $1 $2 }
;

formal_parameter_rest:
  COMMA formal_parameter formal_parameter_rest
    { (new n_formal_parameter_rest $2)::$3 }
  | 
    { [] }
;

mtype: 
  // lookahead 3
  | boolean_type 
    { new n_type (TWO $1) }
  | integer_type
    { 
      new n_type $1
      (*
      let ne = new n_type in 
        
        match $1 with
        | ONE a -> ne (ONE a)
        | THREE a -> ne (THREE a)
        | _  -> raise (Error "mly mtype") *)
    }
  | identifier
    { new n_type (FOUR $1) }
;

array_type:
  LSQPAREN RSQPAREN 
    { ONE (new n_array_type) }
  |
    { THREE (new n_integer_type) }
;

boolean_type:
  BOOLEAN
  { new n_boolean_type }
;
  
integer_type:
  INTEGER array_type
  { $2 }
;

mul_statement:
  statement mul_statement 
    { 
      $1::$2 
    }
  |
    { [] }
;

statement:
  | block
    { new n_statement (ONE $1) }
  /* lookahead 2 */
  | identifier pre_id_assignment_statement
    {
      let ne = new n_statement in
        match $2 with
        | ONE a -> ne (TWO (new n_assignment_statement $1 a))
        | TWO a ->
          (
            match a with
            | b,c -> ne (THREE (new n_array_assignment_statement $1 b c))
            (*| _ -> raise (Error "mly block") *)
          )
        | _ -> raise (Error "mly block2")
    }
  | if_statement
    { new n_statement (FOUR $1) }
  | while_statement
    { new n_statement (FIVE $1) }
  | print_statement
    { new n_statement (SIX $1) }
;

block:
  LBRACE mul_statement RBRACE
    { new n_block $2 }
;

pre_id_assignment_statement:
  assignment_statement
    { ONE $1 }
  | array_assignment_statement
    { TWO $1 }
;

assignment_statement:
  ASSIGN expression SEMICOLON
    { $2 }
;

array_assignment_statement:
  LSQPAREN expression RSQPAREN ASSIGN expression SEMICOLON
    { $2, $5 }
;

if_statement:
  IF LPAREN expression RPAREN statement ELSE statement 
  { 
    new n_if_statement $3 $5 $7
  }
;

while_statement:
  WHILE LPAREN expression RPAREN statement
  {
    new n_while_statement $3 $5
  }
;

print_statement:
  PRINT LPAREN expression RPAREN SEMICOLON
  {
    new n_print_statement $3
  }
; 

// weird lookaheads... going to follow the ebnf grammar instead
expression:
  primary_expression expression_follow
  { 
    let ne = new n_expression in
    match $2 with 
      | ONE a -> ne (ONE (new n_and_expression $1 a))
      | TWO a -> ne (TWO (new n_compare_expression $1 a))
      | THREE a -> ne (THREE (new n_plus_expression $1 a))
      | FOUR a -> ne (FOUR (new n_minus_expression $1 a))
      | FIVE a -> ne (FIVE (new n_times_expression $1 a))
      | SIX a -> ne (SIX (new n_array_lookup $1 a))
      | SEVEN a -> (
        match a with 
        | ONE b -> ne (SEVEN (new n_array_length $1))
        | TWO (b,c) -> ne (EIGHT (new n_message_send $1 b c))
        | _ -> raise (Error "mly expression")  )
      | EIGHT a -> ne (NINE $1)
      | _ -> raise (Error "mly expression2")
  }
;

/* ocamlyacc will complain, but note that the behaviour in a 
  shift/reduce conflict is to shift, which is what we want */
expression_follow:
  | and_expression 
  { ONE $1 }
  | compare_expression
  { TWO $1 }
  | plus_expression
  { THREE $1 }
  | minus_expression
  { FOUR $1 }
  | times_expression
  { FIVE $1 }
  | array_lookup
  { SIX $1 }
  | DOT array_length_message_send
  { SEVEN $2 }
  | 
  { EIGHT () }
;

and_expression:
  AND primary_expression
  { $2 }
;

compare_expression:
  LT primary_expression
  { $2 }
;

plus_expression:
  PLUS primary_expression
  { $2 }
;

minus_expression:
  MINUS primary_expression
  { $2 }
;

times_expression:
  TIMES primary_expression
  { $2 }
;

array_lookup:
  LSQPAREN primary_expression RSQPAREN
  { $2 }
;

array_length_message_send:
  array_length
  { ONE () }
  | message_send
  { TWO ($1) }
; 
array_length:
  LENGTH
    {}
;
 
message_send:
  identifier 
  LPAREN expression_list RPAREN
    { $1,$3 }
;

expression_list:
  expression expression_rest
    { SYM (new n_expression_list $1 $2) }
  |
    { NULL }
;

expression_rest:
  COMMA expression expression_rest
    { (new n_expression_rest $2)::$3 }
  |
    { [] }
;

primary_expression:
  integer_literal
    { new n_primary_expression (ONE $1) }
  | true_literal
    { new n_primary_expression (TWO $1) }
  | false_literal
    { new n_primary_expression (THREE $1) }
  | identifier
    { new n_primary_expression (FOUR $1) }
  | this_expression
    { new n_primary_expression (FIVE $1) }
  | NEW allocation_expression
    {
      let ne = new n_primary_expression in 
        match $2 with 
        | ONE a -> ne (SIX a)
        | TWO a -> ne (SEVEN a) 
        | _ -> raise (Error "mly Primary expression")
    }
  | not_expression
    { new n_primary_expression (EIGHT $1) }
  | bracket_expression
    { new n_primary_expression (NINE $1) }
;

integer_literal:
  INTEGER_LITERAL 
  {
    new n_integer_literal $1 
  }
;

true_literal:
  TRUE
  {
    new n_true_literal
  }
;

false_literal:
  FALSE
  {
    new n_false_literal
  }
;
  
identifier:
  IDENTIFIER
  {
    new n_identifier $1
  }
;

this_expression:
  THIS
  {
    new n_this_expression
  }
;

array_allocation_expression:
  INTEGER LSQPAREN expression RSQPAREN
    { (ONE (new n_array_allocation_expression $3)) }
;

obj_allocation_expression:
  identifier LPAREN RPAREN
    { (TWO (new n_allocation_expression $1)) }
;

allocation_expression:
  array_allocation_expression
  { $1 }
  | obj_allocation_expression
  { $1 }
;

not_expression:
  NOT expression
  { new n_not_expression $2 }
;

bracket_expression:
  LPAREN expression RPAREN
  { new n_bracket_expression $2 }
;
%%

