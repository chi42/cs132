%{ 
  open Vapor_tokens
  open Vapor_parser

  let parse_error s = 
    (* REMOVE 'FORE SUBMISSION *)
    print_endline s;
    flush stdout
%}

%start porgram
%type <Tree
