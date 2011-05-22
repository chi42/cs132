%{ 
  open Vtokens
  open Vparser

  let parse_error s = 
    (* REMOVE 'FORE SUBMISSION *)
    print_endline s;
    flush stdout
%}

%start porgram
%type <Vtokens.t_nodes> program

%%
BNF for VaporParser.jj
NON-TERMINALS
(* Program ::= ( Eol )? ( Function | DataSegment )* <EOF> *)
program: 
  | eol_follow p_func_or_ds EOF
    { Program $2 }

p_func_or_ds:
  | func p_func_or_ds
    { $1::$2 }
  | data_segment p_func_or_ds
    { $1::$2 }
  | 
    { [] }

(* DataSegment ::= ( "const" | "var" ) Ident Eol ( ( DataValue )+ Eol )* *)
data_segment:
  | ds_const_or_var ident eol dv_eol_star

ds_const_or_var:
  | CONST
    { T_const }
  | VAR
    { T_var }

dv_eol_star: 
  | dv_plus eol dv_eol_star 
    {  }
  | 
    {} 

dv_plus: 
  | data_value dv_plus_follow
    { $1::$2 } 

dv_plus_follow:
  | dv_plus
    { $1 }  
  | 
    { [] }

(* Function ::= "func" Ident ( "(" ( VarRefNoReg )* ")" )? ( "[" "in" <Digits>
 * "," "out" <Digits> "," "local" <Digits> "]" )? Eol ( ( CodeLabel ( Eol | Instr )
 * | Instr ) )* *)

(* Instr ::= ( Return | MemRead | MemWrite | Assign | Branch | Goto | Call | 
 * BuiltIn ) *)

(* MemRead ::= VarRef "=" MemRef Eol *)

(* MemWrite ::= MemRef "=" Operand Eol *)

(* MemRef ::= ( StackMemRef | GlobalMemRef ) *)

(* StackMemRef ::= ( "in" | "out" | "local" ) "[" <Digits> "]" *)

(* GlobalMemRef ::= "[" DataAddr ( ( "+" | "-" ) <Digits> )? "]" *)

(* Assign ::= VarRef "=" Operand Eol *)

(* Branch ::= ( "if" | "if0" ) Operand "goto" CodeLabelRef Eol *)

(* Goto ::= "goto" CodeAddr Eol *)

(* Call ::= ( VarRefNoReg "=" )? "call" FuncAddr ( "(" ( OperandNoReg )* ")"
 *              )? Eol *)

(* BuiltIn   ::=   ( VarRef "=" )? Ident "(" ( Operand )* ")" Eol *)

(* Return  ::=   "ret" ( OperandNoReg )? Eol *)

(* DataValue   ::=   ( LabelRef | LitInt ) *)

(* Operand   ::=   ( LabelRef | LitInt | LitStr | VarRef ) *)

(* LabelRef  ::=   <LabelRefIdent> *)

(* CodeAddr  ::=   ( CodeLabelRef | VarRef ) *)

(* DataAddr  ::=   ( DataSegmentRef | VarRef ) *)

(* FuncAddr  ::=   ( FuncRef | VarRef ) *)

(* CodeLabelRef  ::=   <LabelRefIdent> *)

(* DataSegmentRef  ::=   <LabelRefIdent> *)

(* FuncRef   ::=   <LabelRefIdent> *)

(* VarRef  ::=   ( Ident | <RegIdent> ) *)

(* VarRefNoReg   ::=   VarRef *)

(* OperandNoReg  ::=   Operand *)

(* LitInt  ::=   ( <Digits> | <NegDigits> ) *) 

(* LitStr  ::=   <LitStr> *)

(* Ident ::= ( <PlainIdent> | "func" | "const" | "var" | "in" | "out" | "local"
 *         | "if" | "if0" | "goto" | "ret" | "call" ) *)

(* CodeLabel   ::=   <CodeLabelIdent> *)

(* Eol ::= ( <Eol> )+ *)
eol: 
  | EOL eol_follow
    {} 

eol_follow:
  | eol 
  | 
    {} 

   

