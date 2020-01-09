/* File parser.mly */
%token <string> IDENT
%token LPAREN RPAREN
%token SEMICOLON
%token EOF
%start main             /* the entry point */
%type <Base.stuffData> main
%%
main:
   listexpr EOF                { { Base.stTyp = Base.Other ; Base.stList = $1} }
    | listsemiexpr EOF { { Base.stTyp = Base.Composition ; Base.stList = $1} }
;
expr:
  | LPAREN listexpr RPAREN      { Base.mkListDatCursor $2 Base.Other}
    | LPAREN listsemiexpr RPAREN      { Base.mkListDatCursor $2 Base.Composition}
    | IDENT { Base.mkIdentDatCursor $1 }
    ;
listexpr:
    expr  { [ $1 ] }
  | expr listexpr { $1 :: $2 }
listsemiexpr:
    listexpr SEMICOLON listexpr
    { [ Base.mkListDatCursor $1 Base.Other ;
        Base.mkListDatCursor $3 Base.Other ] }
  | listexpr SEMICOLON listsemiexpr { Base.mkListDatCursor $1 Base.Other :: $3 }
 
