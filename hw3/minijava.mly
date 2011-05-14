%{
  open Minijava_lexar 
  open Tree_types

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
%type <Tree_types.t_goal> goal 

/************************************
 * The MiniJava Grammar Starts Here *
 ************************************/
%%

goal: 
  main_class type_declaration ENDOFFILE
  {
    Goal ($1,$2)
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
    | a, b -> Main_class ($2,$12,a,b) 
  }
;

// var_dec -> m_type -> identifier
// mul_state -> statement -> identifier
// some odd production rules are needed to make the grammer ll(1)
// however, this also means we need to piece parsed text back 
// together to create a useful abstract syntax tree
var_dec_mul_state:
  | identifier var_dec_mul_state_follow 
    {
      match $2 with
      | ONE (a,b,c) -> 
        (match $1 with Identifier iden ->
           ((Var_dec (Type (Identifier_type iden),a))::b, c) )
      | TWO (a,b,c) -> (b, (State (Assign_state ($1,a))::c))
      | THREE (a,b,c) ->
        ( match a with 
          | ar1,ar2 ->
            (b, (State (Array_assign_state ($1,ar1,ar2))::c))
        )
      | _ -> raise (Error "mly var_decl_mul_state")
    }
  /* var_declaration */
  | boolean_type identifier SEMICOLON var_dec_mul_state
    { 
      match $4 with 
      | a,b -> ((Var_dec (Type $1,$2))::a, b)  
    }
  | integer_type identifier SEMICOLON var_dec_mul_state
    { 
      match $4 with
      | a,b -> ((Var_dec (Type $1,$2))::a, b)
    }
  /* mul_statement */
  | block mul_statement
    { ([], (State $1)::$2) }
  | if_statement mul_statement
    { ([], (State $1)::$2) }
  | while_statement mul_statement
    { ([], (State $1)::$2) }
  | print_statement mul_statement
    { ([], (State $1)::$2) }
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
      match $3 with
      | ONE (a,b) -> (Type_dec (Class_dec ($2,a,b)))::$4
      | TWO (a,b,c) -> (Type_dec (Class_extends_dec ($2,a,b,c)))::$4
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
    { (Var_dec ($1,$2))::$4 }
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
       (Method_dec ($2,$3,$5,var_list,state_list,$10))::$13 
    }
  |    
    { [] }
;

formal_parameter_list:
  formal_parameter formal_parameter_rest
    { $1::$2 }
  | 
    { [] }
;

formal_parameter:
  mtype identifier
    { Formal_param ($1,$2) }
;

formal_parameter_rest:
  COMMA formal_parameter formal_parameter_rest
    { $2::$3 }
  | 
    { [] }
;

mtype: 
  // lookahead 3
  | boolean_type 
    { Type $1 }
  | integer_type
    { Type $1 }
  | identifier
    { match $1 with Identifier a -> Type (Identifier_type a) }
;

array_type:
  LSQPAREN RSQPAREN 
    { Array_type }
  |
    { Integer_type }
;

boolean_type:
  BOOLEAN
  { Boolean_type }
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
    { 
      State $1
    }
  /* lookahead 2 */
  | identifier pre_id_assignment_statement
    {
      match $2 with
      | ONE a -> State (Assign_state ($1,a))
      | TWO a ->
        (
          match a with
          | b,c -> State (Array_assign_state ($1,b,c))
        )
      | _ -> raise (Error "mly block2")
    }
  | if_statement
    { State $1 }
  | while_statement
    { State $1 }
  | print_statement
    { State $1 }
;

block:
  LBRACE mul_statement RBRACE
    { Block $2 }
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
    If_state ($3,$5,$7)
  }
;

while_statement:
  WHILE LPAREN expression RPAREN statement
  {
    While_state ($3,$5)
  }
;

print_statement:
  PRINT LPAREN expression RPAREN SEMICOLON
  {
    Print_state $3
  }
; 

// weird lookaheads... going to follow the bnf grammar instead
expression:
  primary_expression expression_follow
  { 
    match $2 with 
      | ONE a -> Expr (And_expr ($1,a))
      | TWO a -> Expr (Compare_expr ($1,a))
      | THREE a -> Expr (Plus_expr ($1,a))
      | FOUR a -> Expr (Minus_expr ($1,a))
      | FIVE a -> Expr (Times_expr ($1,a))
      | SIX a -> Expr (Array_lookup ($1,a))
      | SEVEN a -> (
        match a with 
        | ONE b -> Expr (Array_length $1)
        | TWO (b,c) -> Expr (Message_send ($1,b,c))
        | _ -> raise (Error "mly expression") )
      | EIGHT a -> Expr (Primary_expr_expr $1)
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
  { ONE [] }
  | message_send
  { TWO $1 }
; 
array_length:
  LENGTH
  { }
;
 
message_send:
  identifier LPAREN expression_list RPAREN
    { $1,$3 }
;

expression_list:
  expression expression_rest
    { $1::$2 }
  |
    { [] }
;

expression_rest:
  COMMA expression expression_rest
    { $2::$3 }
  |
    { [] }
;

primary_expression:
  integer_literal
    { Primary_expr $1 }
  | true_literal
    { Primary_expr $1 }
  | false_literal
    { Primary_expr $1 }
  | identifier
    { 
      match $1 with Identifier a -> 
        Primary_expr (Identifier_primary a) 
    }
  | this_expression
    { Primary_expr $1 }
  | NEW allocation_expression
    { Primary_expr $2 }
  | not_expression
    { Primary_expr $1 }
  | bracket_expression
    { Primary_expr $1 }
;

integer_literal:
  INTEGER_LITERAL 
  {
    Int_literal $1 
  }
;

true_literal:
  TRUE
  {
    True_literal 
  }
;

false_literal:
  FALSE
  {
    False_literal 
  }
;
  
identifier:
  IDENTIFIER
  {
    Identifier $1
  }
;

this_expression:
  THIS
  {
    This_expr 
  }
;

array_allocation_expression:
  INTEGER LSQPAREN expression RSQPAREN
    { Array_alloc_expr $3 }
;

obj_allocation_expression:
  identifier LPAREN RPAREN
    { Alloc_expr $1 }
;

allocation_expression:
  array_allocation_expression
  { $1 }
  | obj_allocation_expression
  { $1 }
;

not_expression:
  NOT expression
  { 
    Not_expr $2
  }
;

bracket_expression:
  LPAREN expression RPAREN
  { 
    Bracket_expr $2
  }
;
%%

