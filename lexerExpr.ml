let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]
let letter = [%sedlex.regexp? 'a'..'z'|'A'..'Z'] 

open ParserExpr        
exception Eof

let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \t\n") -> token buf
  (* | ['\n' ]        { EOL } *)
  | number -> INT (int_of_string (Sedlexing.Latin1.lexeme buf))
  | Plus letter ->  IDENT((Sedlexing.Latin1.lexeme buf)) 
  | ';'            -> SEMICOLON 
  | '('            -> LPAREN 
  | ')'            -> RPAREN 
  | '{'            -> LBRACE 
  | '}'            -> RBRACE 
  | "⊗"            -> OTIMES 
  | '@'            -> AT 
  | '?'            -> QUESTION 
  | eof            -> EOF 
  | _ -> failwith "Unexpected character"

let lexer = Sedlexing.with_tokenizer token

