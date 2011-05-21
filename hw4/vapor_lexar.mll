{
  open Vapor_parser (* Assumes the parser file is "vapor_parser.mly". *)
}
(* BNF for vaporparser.jj avaliable at: 
 * http://cs.ucla.edu/classes/spring11/cs132/kannan/vapor-grammar.html *)

let skip = 
  | '\t' 
    (* line comment *)
  | "//" [^ '\n' '\r']* 
    (* block comment *)
  | "/*" [^ '*']* '*' ('*' | [^ '*' '/'] [^ '*']* '*')* '/'   

let digit  = ['0'-'9']
let digits = (digit)+
let neg_digits = '-' digits
let ident_head = ['A'-'Z' 'a'-'z' '_']
let ident_rest = (ident_head | digit | '.')
let eol = (('\r')* '\n' ([' ' '\t'])*)+

rule token =  parse
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
  


