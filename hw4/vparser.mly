%{ 
  open Vtypes
  open Vparser

  let parse_error s = 
    (* REMOVE 'FORE SUBMISSION *)
    print_endline s;
    flush stdout
%}

%token 
  COMMA
  CONST
  VAR
  FUNC
  IN
  OUT
  LOCAL
  EQUAL
  PLUS
  MINUS
  IF
  IF_NOT
  GOTO
  CALL
  RET
  LPAREN
  RPAREN
  LBRACKET
  RBRACKET
  EOL 
  EOF 

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
    { Data_segment $1 $2 $4 } 

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
    { Function $2 $3 $4 $6 }

func1_wrapper: 
  | LPAREN vrnr_star RPAREN 
    { $2 }
  | 
    { [] }

vrnr_star:
  | var_ref_no_reg vrnr_star
    { $1::$2 }
  | 
    { [] } 

func2_wrapper: 
  | LBRACKET IN DIGITS COMMA OUT DIGITS COMMA LOCAL DIGITS RBRACKET
    { [((Digits $3),(Digits $6),(Digits $9))] }  
  | 
    { [] } 

func3_wrapper:
  | code_label func3_follow 
    { match $2 with 
      | [] -> Code_label $1
      | [a] -> Cl_and_instr $1 a
    }
  | instr 
    { Instr $1 } 
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
  | return  
    { Instr $1 } 
  | mem_read 
    { Instr $1 } 
  | mem_write
    { Instr $1 } 
  | assign
    { Instr $1 } 
  | branch
    { Instr $1 } 
  | goto
    { Instr $1 } 
  | call
    { Instr $1 } 

//(* MemRead ::= VarRef "=" MemRef Eol *)
mem_read: 
  | var_ref EQUAL mem_ref eol  
    { Mem_read $1 $3 }

//(* MemWrite ::= MemRef "=" Operand Eol *)
mem_write:
  | mem_ref EQUAL operand eol 
    { Mem_write $1 $3 }

//(* MemRef ::= ( StackMemRef | GlobalMemRef ) *)
mem_ref:
  | stack_mem_ref 
    { Mem_ref $1 }
  | global_mem_ref 
    { Mem_ref $1 }

//(* StackMemRef ::= ( "in" | "out" | "local" ) "[" <Digits> "]" *)
stack_mem_ref:
  | IN LBRACKET DIGITS RBRACKET
    { Stack_mem_ref T_in (Digits $3) } 
  | OUT LBRACKET DIGITS RBRACKET
    { Stack_mem_ref T_out (Digits $3) }
  | LOCAL LBRACKET DIGITS RBRACKET
    { Stack_mem_ref T_local (Digits $3) } 

//(* GlobalMemRef ::= "[" DataAddr ( ( "+" | "-" ) <Digits> )? "]" *)
global_mem_ref:
  | LBRACKET data_addr gmf_1 RBRACKET 
    { Global_mem_ref $2 $3 } 

gmf_1:
  | PLUS DIGITS
    { [(Plus,(Digits $2))] }
  | MINUS DIGITS
    { [(Minus,(Digits $2))] }

//(* Assign ::= VarRef "=" Operand Eol *)
assign:
  | var_ref EQUAL operand eol 
    { Assign $1 $3 } 

//(* Branch ::= ( "if" | "if0" ) Operand "goto" CodeLabelRef Eol *)
branch: 
  | if_ifnot operand GOTO code_label_ref eol 
    { Branch $1 $2 $4 } 

if_ifnot:
  | IF
    { T_if } 
  | IF_NOT
    { T_if_not } 

//(* Goto ::= "goto" CodeAddr Eol *)
goto:
  | GOTO code_addr eol 
    { Goto $2 } 

//(* Call ::= ( VarRefNoReg "=" )? "call" FuncAddr ( "(" ( OperandNoReg )* ")"
// *              )? Eol *)
call:
  | call_1 CALL func_addr call_2 eol 
    { Call $1 $3 $4 }
    
call_1:
  | var_ref_no_reg EQUAL
    { [$1] }
  | 
    { [] }

call_2:
  | LPAREN operand_no_reg RPAREN call_2
    { $2::$4 } 
  | 
    { [] }
//(* BuiltIn   ::=   ( VarRef "=" )? Ident "(" ( Operand )* ")" Eol *)

//(* Return  ::=   "ret" ( OperandNoReg )? Eol *)
return: 
  | RET onr_question eol 
    { Return $2 } 

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
    { Labe_ref (Label_ref_ident $1) } 

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

//(* VarRefNoReg   ::=   VarRef *)
var_ref_no_reg:
  | var_ref
    { Var_ref_no_reg $1 } 

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
    { Ident (Plain_ident $1) } 
  | FUNC 
    { Ident T_func }
  | CONST
    { Ident T_const }
  | VAR
    { Ident T_var } 
  | IN 
    { Ident T_in }
  | OUT
    { Ident T_out } 
  | LOCAL 
    { Ident T_local } 
  | IF 
    { Ident T_if }
  | IF_NOT 
    { Ident T_if_not }
  | GOTO 
    { Ident T_goto }
  | RET 
    { Ident T_ret }
  | CALL
    { Ident T_call }

//(* CodeLabel   ::=   <CodeLabelIdent> *)
code_label:
  | CODE_LABEL_IDENT 
    { Code_label (Code_label_indent $1) } 

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


