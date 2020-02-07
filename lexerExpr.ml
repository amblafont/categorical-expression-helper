let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]
let letter = [%sedlex.regexp? 'a'..'z'|'A'..'Z'|
  945 | 961 | 955 ]
   (* "α"|"ρ"|"λ"] *)

(* |'λ'|'ρ']  *)

open ParserExpr        
exception Eof

let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \t\n") -> token buf
  (* | ['\n' ]        { EOL } *)
  | number -> INT (int_of_string (Sedlexing.Latin1.lexeme buf))
  | Plus (letter | "ρ") ->  IDENT((Sedlexing.Utf8.lexeme buf)) 
  | ';'            -> SEMICOLON 
  | '('            -> LPAREN 
  | ')'            -> RPAREN 
  | '{'            -> LBRACE 
  | '}'            -> RBRACE 
  | 8855 (* "⊗" *)            -> OTIMES 
  (* | "ρ"            -> failwith "c'est moi!"
  | "ρ"            -> failwith "c'est moi!" *)
  | '@'            -> AT 
  | '?'            -> QUESTION 
  | eof            -> EOF 
  | any -> 
     let str = (Sedlexing.Utf8.lexeme buf) in
       print_endline str ;
       failwith ("Unexpected character: " ^ str)
  | _ -> failwith "Unexpected character"

let lexer = Sedlexing.with_tokenizer token

