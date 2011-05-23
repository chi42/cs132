%{ 
  open Vtypes
  open Vlexar
  open Exceptions

  let parse_error s = 
    (* REMOVE 'FORE SUBMISSION *)
    print_endline s;
    flush stdout
%}

%token COMMA CONST VAR FUNC IN OUT LOCAL EQUAL PLUS MINUS IF EOF
%token IF_NOT GOTO CALL RET LPAREN RPAREN LBRACKET RBRACKET EOL 

%token <string> NEG_DIGITS
%token <string> DIGITS 
%token <string> PLAIN_IDENT 
%token <string> REG_IDENT 
%token <string> CODE_LABEL_IDENT 
%token <string> LABEL_REF_IDENT 
%token <string> LIT_STR 

%start program
%type <Vtypes.t_nodes> program

%%

//(* Program ::= ( Eol )? ( Function | DataSegment )* <EOF> *)
program: 
  | eol_question p_func_or_ds EOF
    { Program $2 }

p_func_or_ds:
  | func p_func_or_ds
    { $1::$2 }
  | data_segment p_func_or_ds
    { $1::$2 }
  | 
    { [] }

//(* DataSegment ::= ( "const" | "var" ) Ident Eol ( ( DataValue )+ Eol )* *)
data_segment:
  | ds_const_or_var ident eol dv_eol_star
    { Data_segment ($1,$2,$4)} 

ds_const_or_var:
  | CONST
    { T_const }
  | VAR
    { T_var }

dv_eol_star: 
  | dv_plus eol dv_eol_star 
    { $1::$3 }
  | 
    { [] } 

dv_plus: 
  | data_value dv_plus_follow
    { $1::$2 } 

dv_plus_follow:
  | dv_plus
    { $1 }  
  | 
    { [] }

//(* Function ::= "func" Ident ( "(" ( VarRefNoReg )* ")" )? ( "[" "in" <Digits>
// * "," "out" <Digits> "," "local" <Digits> "]" )? Eol ( ( CodeLabel ( Eol | Instr )
// * | Instr ) )* *)
func: 
  | FUNC ident func1_wrapper func2_wrapper eol func3_wrapper 
    { Function ($2,$3,$4,$6) }

func1_wrapper: 
  | LPAREN vrnr_star RPAREN 
    { $2 }
  | 
    { [] }

vrnr_star:
  | var_ref vrnr_star
    { $1::$2 }
  | 
    { [] } 

func2_wrapper: 
  | LBRACKET IN DIGITS COMMA OUT DIGITS COMMA LOCAL DIGITS RBRACKET
    { [((Digits $3),(Digits $6),(Digits $9))] }  
  | 
    { [] } 

func3_wrapper:
  | code_label func3_follow  func3_wrapper
    { match $2 with 
      | [] -> $1::$3
      | [a] -> (Cl_and_instr ($1,a))::$3
      | _ -> raise (My_parse_error "func3_wrapper")
    }
  | instr func3_wrapper
    { (Instr $1)::$2 } 
  | 
    { [] } 

func3_follow: 
  | eol  
    { [] } 
  | instr
    { [$1] } 

//(* Instr ::= ( Return | MemRead | MemWrite | Assign | Branch | Goto | Call | 
// * BuiltIn ) *)
instr:
  | GOTO code_addr eol 
    { Goto $2 } 
  | GOTO EQUAL mem_ref eol   
    { Mem_read ((Plain_ident "goto"),$3) } 
  | GOTO EQUAL operand eol 
    { Assign ((Plain_ident "goto"),$3) }   
  | GOTO EQUAL CALL func_addr call_2 eol 
    { Call ([(Plain_ident "goto")],$4,$5) }
  | GOTO EQUAL ident LPAREN builtin_2 RPAREN eol 
    { Builtin ([(Plain_ident "goto")],$3,$5) } 
  | GOTO LPAREN builtin_2 RPAREN eol 
    { Builtin ([],(Plain_ident "goto"),$3) } 

  | RET onr_question eol 
    { Return $2 } 
  | RET EQUAL mem_ref eol   
    { Mem_read ((Plain_ident "ret"),$3) } 
  | RET EQUAL operand eol 
    { Assign ((Plain_ident "ret"),$3) }   
  | RET EQUAL CALL func_addr call_2 eol 
    { Call ([Plain_ident "ret"],$4,$5) }
  | RET EQUAL ident LPAREN builtin_2 RPAREN eol 
    { Builtin ([Plain_ident "ret"],$3,$5) } 
  | RET LPAREN builtin_2 RPAREN eol 
    { Builtin ([],(Plain_ident "ret"),$3) } 

  | IN DIGITS EQUAL operand eol
    { Mem_write ((Mem_ref (Stack_mem_ref (T_in,(Digits $2)))),$4) } 
  | IN EQUAL mem_ref eol   
    { Mem_read ((Plain_ident "in"),$3) } 
  | IN EQUAL operand eol 
    { Assign ((Plain_ident "in"),$3) }   
  | IN EQUAL CALL func_addr call_2 eol 
    { Call ([Plain_ident "in"],$4,$5) }
  | IN EQUAL ident LPAREN builtin_2 RPAREN eol 
    { Builtin ([Plain_ident "in"],$3,$5) } 
  | IN LPAREN builtin_2 RPAREN eol 
    { Builtin ([],(Plain_ident "in"),$3) } 

  | OUT DIGITS EQUAL operand eol
    { Mem_write ((Mem_ref (Stack_mem_ref (T_out,(Digits $2)))),$4) } 
  | OUT EQUAL mem_ref eol   
    { Mem_read ((Plain_ident "out"),$3) } 
  | OUT EQUAL operand eol 
    { Assign ((Plain_ident "out"),$3) }   
  | OUT EQUAL CALL func_addr call_2 eol 
    { Call ([Plain_ident "out"],$4,$5) }
  | OUT EQUAL ident LPAREN builtin_2 RPAREN eol 
    { Builtin ([Plain_ident "out"],$3,$5) } 
  | OUT LPAREN builtin_2 RPAREN eol 
    { Builtin ([],(Plain_ident "out"),$3) } 

  | LOCAL DIGITS EQUAL operand eol
    { Mem_write ((Mem_ref (Stack_mem_ref (T_local,(Digits $2)))),$4) } 
  | LOCAL EQUAL mem_ref eol   
    { Mem_read ((Plain_ident "local"),$3) } 
  | LOCAL EQUAL operand eol 
    { Assign ((Plain_ident "local"),$3) }   
  | LOCAL EQUAL CALL func_addr call_2 eol 
    { Call ([Plain_ident "local"],$4,$5) }
  | LOCAL EQUAL ident LPAREN builtin_2 RPAREN eol 
    { Builtin ([Plain_ident "local"],$3,$5) } 
  | LOCAL LPAREN builtin_2 RPAREN eol 
    { Builtin ([],(Plain_ident "local"),$3) } 

  | IF operand GOTO code_label_ref eol 
    { Branch (T_if,$2,$4) }
  | IF EQUAL mem_ref eol   
    { Mem_read ((Plain_ident "if"),$3) } 
  | IF EQUAL operand eol 
    { Assign ((Plain_ident "if"),$3) }   
  | IF EQUAL CALL func_addr call_2 eol 
    { Call ([Plain_ident "if"],$4,$5) }
  | IF EQUAL ident LPAREN builtin_2 RPAREN eol 
    { Builtin ([Plain_ident "if"],$3,$5) } 
  | IF LPAREN builtin_2 RPAREN eol 
    { Builtin ([],(Plain_ident "if"),$3) } 

  | IF_NOT operand GOTO code_label_ref eol 
    { Branch (T_if_not,$2,$4) }
  | IF_NOT EQUAL mem_ref eol   
    { Mem_read ((Plain_ident "if0"),$3) } 
  | IF_NOT EQUAL operand eol 
    { Assign ((Plain_ident "if0"),$3) }   
  | IF_NOT EQUAL CALL func_addr call_2 eol 
    { Call ([Plain_ident "if0"],$4,$5) }
  | IF_NOT EQUAL ident LPAREN builtin_2 RPAREN eol 
    { Builtin ([Plain_ident "if0"],$3,$5) } 
  | IF_NOT LPAREN builtin_2 RPAREN eol 
    { Builtin ([],(Plain_ident "if0"),$3) } 

  | PLAIN_IDENT instr_ident_follow 

  | 
  
( <PlainIdent> | "func" | "const" | "var" | 

  | global_mem_ref EQUAL operand eol
    { Mem_write ((Mem_ref $1),$3) } 
  | CALL func_addr call_2 eol 
    { Call ([],$2,$3) }



//(* Assign ::= VarRef "=" Operand Eol *)
//(* MemRead ::= VarRef "=" MemRef Eol *)
//(* MemWrite ::= MemRef "=" Operand Eol *)

//(* MemRef ::= ( StackMemRef | GlobalMemRef ) *)
mem_ref:
  | stack_mem_ref 
    { Mem_ref $1 }
  | global_mem_ref 
    { Mem_ref $1 }

//(* StackMemRef ::= ( "in" | "out" | "local" ) "[" <Digits> "]" *)
stack_mem_ref:
  | IN LBRACKET DIGITS RBRACKET
    { Stack_mem_ref (T_in,(Digits $3)) } 
  | OUT LBRACKET DIGITS RBRACKET
    { Stack_mem_ref (T_out,(Digits $3)) }
  | LOCAL LBRACKET DIGITS RBRACKET
    { Stack_mem_ref (T_local,(Digits $3)) } 

//(* GlobalMemRef ::= "[" DataAddr ( ( "+" | "-" ) <Digits> )? "]" *)
global_mem_ref:
  | LBRACKET data_addr gmf_1 RBRACKET 
    { Global_mem_ref ($2,$3) } 

gmf_1:
  | PLUS DIGITS
    { [(T_plus,(Digits $2))] }
  | MINUS DIGITS
    { [(T_minus,(Digits $2))] }


//(* Call ::= ( VarRefNoReg "=" )? "call" FuncAddr ( "(" ( OperandNoReg )* ")"
// *              )? Eol *)
    
call_2:
  | LPAREN operand_no_reg RPAREN call_2
    { $2::$4 } 
  | 
    { [] }

//(* BuiltIn   ::=   ( VarRef "=" )? Ident "(" ( Operand )* ")" Eol *)


builtin_2:
  | operand builtin_2
    { $1::$2 }
  | 
    { [] }

//(* Return  ::=   "ret" ( OperandNoReg )? Eol *)

onr_question:
  | operand_no_reg 
    { [$1] } 
  | 
    { [] }    

//(* DataValue   ::=   ( LabelRef | LitInt ) *)
data_value:
  | label_ref 
    { Data_value $1 } 
  | lit_int
    { Data_value $1 } 

//(* Operand   ::=   ( LabelRef | LitInt | LitStr | VarRef ) *)
operand:
  | label_ref
    { Operand $1 } 
  | lit_int
    { Operand $1 } 
  | lit_str
    { Operand $1 } 
  | var_ref
    { Operand $1 } 

//(* LabelRef  ::=   <LabelRefIdent> *)
label_ref:
  | LABEL_REF_IDENT
    { Label_ref (Label_ref_ident $1) } 

//(* CodeAddr  ::=   ( CodeLabelRef | VarRef ) *)
code_addr:
  | code_label_ref
    { $1 } 
  | var_ref
    { $1 }  

//(* DataAddr  ::=   ( DataSegmentRef | VarRef ) *)
data_addr:
  | data_segment_ref
    { Data_addr $1 } 
  | var_ref
    { Data_addr $1 } 


//(* FuncAddr  ::=   ( FuncRef | VarRef ) *)
func_addr:
  | func_ref
    { Func_addr $1 } 
  | var_ref
    { Func_addr $1 }  

//(* CodeLabelRef  ::=   <LabelRefIdent> *)
code_label_ref:
  | LABEL_REF_IDENT
    { Code_label_ref (Label_ref_ident $1) } 

//(* DataSegmentRef  ::=   <LabelRefIdent> *)
data_segment_ref:
  | LABEL_REF_IDENT
    { Data_segment_ref (Label_ref_ident $1) }

//(* FuncRef   ::=   <LabelRefIdent> *)
func_ref:
  | LABEL_REF_IDENT
    { Func_ref (Label_ref_ident $1) }  

//(* VarRef  ::=   ( Ident | <RegIdent> ) *)
var_ref:
  | ident
    { Var_ref $1 }
  | REG_IDENT
    { Var_ref (Reg_ident $1) } 

//(* OperandNoReg  ::=   Operand *)
operand_no_reg:
  | operand
    { Operand_no_reg $1 }

//(* LitInt  ::=   ( <Digits> | <NegDigits> ) *) 
lit_int:
  | DIGITS
    { Lit_int (Digits $1) }
  | NEG_DIGITS
    { Lit_int (Neg_digits $1) } 

//(* LitStr  ::=   <LitStr> *)
lit_str:
  | LIT_STR
    { Lit_str $1 } 

//(* Ident ::= ( <PlainIdent> | "func" | "const" | "var" | "in" | "out" | "local"
// *         | "if" | "if0" | "goto" | "ret" | "call" ) *)
ident: 
  | PLAIN_IDENT 
    { (Plain_ident $1) } 
  | FUNC 
    { Plain_ident "func" }
  | CONST
    { Plain_ident "cons" }
  | VAR
    { Plain_ident "var" } 
  | IN 
    { Plain_ident "in" }
  | OUT
    { Plain_ident "out"} 
  | LOCAL 
    { Plain_ident "local" } 
  | IF 
    { Plain_ident "if" }
  | IF_NOT 
    { Plain_ident "if0" }
  | GOTO 
    { Plain_ident "goto" }
  | RET 
    { Plain_ident "ret" }
  | CALL
    { Plain_ident "call" }

//(* CodeLabel   ::=   <CodeLabelIdent> *)
code_label:
  | CODE_LABEL_IDENT 
    { Code_label $1 } 

//(* Eol ::= ( <Eol> )+ *)

eol: 
  | EOL eol_star
    {} 

eol_star:
  | eol  
    {}
  | 
    {} 

eol_question:
  | EOL  
    {}
  | 
    {} 


