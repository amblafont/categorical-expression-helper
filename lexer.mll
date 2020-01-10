{
 open Parser        (* The type token is defined in parser.mli *)
 exception Eof
}
rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
  (* | ['\n' ]        { EOL } *)
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | ['a'-'z''A'-'Z']+ as lxm { IDENT(lxm) }
  | ';'            { SEMICOLON }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | '@'            { AT }
  | '?'            { QUESTION }
  | eof            { EOF }
