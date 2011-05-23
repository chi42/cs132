{
  open Vtokens
}
(* BNF for vaporparser.jj avaliable at: 
 * http://cs.ucla.edu/classes/spring11/cs132/kannan/vapor-grammar.html *)


let digit  = ['0'-'9']
let digits = (digit)+

let ident_head = ['A'-'Z' 'a'-'z' '_']
let ident_rest = (ident_head | digit | '.')
let ident = ident_head ident_rest*

rule token = parse
    (* non-newline spaces and whitespace *)
  | ' '
  | '\t' 
  | "//" [^ '\n' '\r']*  
  | "/*" [^ '*']* '*' ('*' | [^ '*' '/'] [^ '*']* '*')* '/' 
    (* end of line *)
  | (('\r')* '\n' ([' ' '\t'])*)+ 
    { EOL } 
    (* end of file *)
  | eof
  { EOF } 
    (* numbers, positive and negative *)
  | ("-" digits) as str
    { NEG_DIGITS str } 
  | digits as str 
    { DIGITS str } 
    (* strings of identifiers *)
  | ident as str
    { PLAIN_IDENT str } 
  | ("$" ident) as str
    { REG_IDENT str }
  | (ident ":") as str
    { CODE_LABEL_IDENT str } 
  | (":" ident) as str
    { LABEL_REF_IDENT str }
    (* strings *)
  | '"'("\\\"" | [' '-'~'] )* '"' as str
    { STR str }  
    (* reserved keywords *)
  | "const"     { CONST }
  | "var"       { VAR } 
  | "func"      { FUNC } 
  | "in"        { IN }
  | "out"       { OUT }
  | "local"     { LOCAL}
  | "="         { EQUAL }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "if"        { IF }
  | "if0"       { IF_NOT }
  | "goto"      { GOTO } 
  | "call"      { CALL } 
  | "ret"       { RET } 
  | "("         { LPAREN }
  | ")"         { RPAREN } 
  | "["         { LBRACKET }
  | "]"         { RBRACKET } 
  | ","         { COMMA } 


