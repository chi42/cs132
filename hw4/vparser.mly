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
%token <string> DIGITS of string
%token <string> PLAIN_IDENT 
%token <string> REG_IDENT 
%token <string> CODE_LABEL_IDENT 
%token <string> LABEL_REF_IDENT 
%token <string> STR 

%start program
%type <Vtokens.t_nodes> program

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
    { $1 }
  | 
    { [] }

vrnr_star:
  | var_ref_no_reg vrnr_star
    { $1::$2 }
  | 
    { [] } 

func2_wrapper: 
  | LBRACKET IN digits COMMA OUT digits COMMA LOCAL digits RBRACKET
    { [($3,$6,$9)] }  
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
    { $1 } 
  | memread 
    { $1 } 
  | memwrite
    { $1 } 
  | assign
    { $1 } 
  | branch
    { $1 } 
  | goto
    { $1 } 
  | call
    { $1 } 

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
    { $1 }
  | global_mem_ref 
    { $1 }

//(* StackMemRef ::= ( "in" | "out" | "local" ) "[" <Digits> "]" *)

//(* GlobalMemRef ::= "[" DataAddr ( ( "+" | "-" ) <Digits> )? "]" *)

//(* Assign ::= VarRef "=" Operand Eol *)

//(* Branch ::= ( "if" | "if0" ) Operand "goto" CodeLabelRef Eol *)

//(* Goto ::= "goto" CodeAddr Eol *)

//(* Call ::= ( VarRefNoReg "=" )? "call" FuncAddr ( "(" ( OperandNoReg )* ")"
// *              )? Eol *)

//(* BuiltIn   ::=   ( VarRef "=" )? Ident "(" ( Operand )* ")" Eol *)

//(* Return  ::=   "ret" ( OperandNoReg )? Eol *)

//(* DataValue   ::=   ( LabelRef | LitInt ) *)

//(* Operand   ::=   ( LabelRef | LitInt | LitStr | VarRef ) *)

//(* LabelRef  ::=   <LabelRefIdent> *)

//(* CodeAddr  ::=   ( CodeLabelRef | VarRef ) *)

//(* DataAddr  ::=   ( DataSegmentRef | VarRef ) *)

//(* FuncAddr  ::=   ( FuncRef | VarRef ) *)

//(* CodeLabelRef  ::=   <LabelRefIdent> *)

//(* DataSegmentRef  ::=   <LabelRefIdent> *)

//(* FuncRef   ::=   <LabelRefIdent> *)

//(* VarRef  ::=   ( Ident | <RegIdent> ) *)

//(* VarRefNoReg   ::=   VarRef *)

//(* OperandNoReg  ::=   Operand *)

//(* LitInt  ::=   ( <Digits> | <NegDigits> ) *) 

//(* LitStr  ::=   <LitStr> *)

//(* Ident ::= ( <PlainIdent> | "func" | "const" | "var" | "in" | "out" | "local"
// *         | "if" | "if0" | "goto" | "ret" | "call" ) *)

//(* CodeLabel   ::=   <CodeLabelIdent> *)

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


