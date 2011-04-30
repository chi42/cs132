(* file: minijava_lexer.mll *)
{
  open Minijava (* Assumes the parser file is "minijava.mly". *)
}


let letter = ['$' 'A'-'Z' 'a'-'z' '_']
let digit  = ['0'-'9']

rule token = parse
  (* whitespace *)
  | [' ' '\t' '\r' '\n' '\x0C'] 
  (* comments (single line, formal comment, multi-line, respectively *)
  | "//" ([^ '\n' '\r'])* ('\n'|'\r'|"\r\n")
  | "/**" ([^ '*'])* '*' ('*' | ([^ '*' '/'] ([^ '*'])* '*'))* '/'
  | "/*" ([^ '*'])* '*' ('*' | ([^ '*' '/'] ([^ '*'])* '*'))* '/'
    { token lexbuf }
  (* tokens *)
  | "("         {LPAREN    }
  | "("         {LPAREN    } 
  | ")"         {RPAREN    } 
  | "["         {LSQPAREN  } 
  | "]"         {RSQPAREN  } 
  | "{"         {LBRACE    } 
  | "}"         {RBRACE    } 
  | ";"         {SEMICOLON } 
  | "."         {DOT       } 
  | "="         {ASSIGN    } 
  | "<"         {LT        } 
  | "+"         {PLUS      } 
  | "-"         {MINUS     } 
  | "*"         {TIMES     } 
  | "&&"        {AND       } 
  | "!"         {NOT       } 
  | "boolean"   {BOOLEAN   } 
  | "class"     {CLASS     } 
  | "interface" {INTERFACE } 
  | "else"      {ELSE      } 
  | "extends"   {EXTENDS   } 
  | "false"     {FALSE     } 
  | "if"        {IF        } 
  | "while"     {WHILE     } 
  | "int"       {INTEGER   } 
  | "length"    {LENGTH    } 
  | "main"      {MAIN      } 
  | "new"       {NEW       } 
  | "public"    {PUBLIC    } 
  | "return"    {RETURN    } 
  | "static"    {STATIC    } 
  | "String"    {STRING    } 
  | "this"      {THIS      } 
  | "true"      {TRUE      } 
  | "void"      {VOID      }
  | ","         {COMMA     }
  | eof         {ENDOFFILE }
  | "System.out.println" {PRINT} 
  (* integer literal *)
  | ( ['1'-'9'] (['0'-'9'])* | '0' ) as str 
    { INTEGER_LITERAL (int_of_string str) }
  (* identifier *)
  | letter (letter | digit)* as str
    { IDENTIFIER (str) }



