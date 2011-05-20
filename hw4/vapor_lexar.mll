{
  open Vapor (* Assumes the parser file is "vapor.mly". *)
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
  | 


