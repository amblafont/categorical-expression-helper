/* File parser.mly */
/*
  https://stackoverflow.com/questions/40754644/yacc-precedence-of-a-rule-with-no-operator
                                                                                   http://www.dabeaz.com/ply/ply.html#ply_nn27
*/
%token <string> IDENT 
%token <int> INT
%token LPAREN RPAREN
%token SEMICOLON
%token LBRACE RBRACE AT QUESTION
%token EOF
%start main             /* the entry point */
%type <Base.datCursor> main
%type <Base.cursor list> cursors
%type <Base.cursor> cursor
%type <Base.dat> dat
%type <Base.datCursor> expr
%%
main:
  expr EOF { $1 } ;

expr:
    | LBRACE cursors RBRACE AT dat { {Base.data = $5 ; Base.cursors = $2}}
      | cursor AT dat { {Base.data = $3 ; Base.cursors = [ $1 ]} }
      | dat { Base.newDatCursor $1 } ;
cursors:
   cursor { [ $1 ] }
  | cursor cursors { $1 :: $2 } ;

cursor :
  INT  { Base.Nb $1}
  | QUESTION IDENT { Base.CurMVar (Base.Name $2)} ;

dat:
  | LPAREN listexpr RPAREN      { Base.mkListDat $2 Base.Other } 
  | LPAREN listsemiexpr RPAREN    { Base.mkListDat $2 Base.Composition }
  | IDENT { Base.Ident (Name $1) }
  | QUESTION IDENT { Base.MVar (Name $2)}
    ;
listexpr:
    expr  { [ $1 ] }
  | expr listexpr { $1 :: $2 } ;
listsemiexpr:
    expr SEMICOLON expr
      { [ $1 ; $3 ] }
  | expr SEMICOLON listsemiexpr { $1 :: $3 }
 
