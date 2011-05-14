exception Error of string

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

type t_identifier = 
  | Identifier of string

type t_type_or = 
  | Array_type
  | Boolean_type
  | Integer_type
  | Identifier_type of string
and t_type = 
  | Type of t_type_or

type t_primary_expr_or = 
  | Int_literal of int
  | True_literal
  | False_literal
  | Identifier_primary of string
  | This_expr 
  | Array_alloc_expr of t_expr
  | Alloc_expr of t_identifier
  | Not_expr of t_expr
  | Bracket_expr of t_expr
and t_expr_or = 
  | And_expr of t_primary_expr * t_primary_expr 
  | Compare_expr of t_primary_expr * t_primary_expr
  | Plus_expr of t_primary_expr * t_primary_expr
  | Minus_expr of t_primary_expr * t_primary_expr
  | Times_expr of t_primary_expr * t_primary_expr
  | Array_lookup of t_primary_expr * t_primary_expr
  | Array_length of t_primary_expr 
  | Message_send of t_primary_expr * t_identifier * (t_expr list) 
  | Primary_expr_expr of t_primary_expr 
and t_expr = 
  | Expr of t_expr_or
  | Special of int  
and t_primary_expr =
  | Primary_expr of t_primary_expr_or


type t_state =
  | State of t_state_or
and t_state_or = 
  | Block of t_state list
  | Assign_state of t_identifier * t_expr
  | Array_assign_state of t_identifier * t_expr * t_expr
  | If_state of t_expr * t_state * t_state
  | While_state of t_expr * t_state
  | Print_state of t_expr

type t_formal_param =
  | Formal_param of t_type * t_identifier

type t_var_dec =
  | Var_dec of t_type * t_identifier

type t_method_dec = 
  | Method_dec of t_type * t_identifier * t_formal_param list * 
      t_var_dec list * t_state list * t_expr

type t_main_class = 
  | Main_class of t_identifier * t_identifier * t_var_dec list * t_state list

type t_type_dec_or = 
  | Class_dec of t_identifier * t_var_dec list * t_method_dec list
  | Class_extends_dec of t_identifier * t_identifier * t_var_dec list * 
      t_method_dec list
and t_type_dec = 
  | Type_dec of t_type_dec_or

type t_goal = 
  | Goal of t_main_class * (t_type_dec list)

