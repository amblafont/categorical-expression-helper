
open Base ;;
open Stringstuff ;;
open Aideur;;
open Lib;;
open Toplib;;
let cursorMode : mode =
  { commands = 
  [ 
     'j' , ("move down", j) ;
    'k' , ("move up", k) ;
     'h' , ("move left", h );
     'l' , ("move right", l) ;
     (* The advantage of h and l is that they don't care about the type of
     composition. But we could also ignore this! *)
     'H' , equation_to_command( Equation.equation_swap MoreEquations.eq_move_cursor );
     'L' , equation_to_command  MoreEquations.eq_move_cursor ;
    'a' , ("activate cursor" , iactiveCursor) ;
    'd' , ("desactivate cursor" , idesactiveCursor) ;
    'n' , ("create new cursors at identifiers" , icursorsAtIdentifiers) ;
     statusCmd 
  ] ; prompt = "Cursor mode" };;

let expressionMode =
  { commands = 
      [  's' , ("set expression" , isetExpression)  ;
         (* 'n' , ("natural transformations mode" , wrapMode ntMode) ; *)
           statusCmd 
      ] ; prompt = "Expression mode" };;

let mainMode : mode =
  { commands =[
    'c' , ("cursor mode", wrapMode cursorMode) ;
    'e' , ("expression mode", wrapMode expressionMode) ;
    'n' , ("natural transformation mode (experimental)", wrapMode (equation_to_mode MoreEquations.eq_nat_trans "Natural Transformations")) ;
    statusCmd 
  ] ; prompt = "Main menu"
  }


let startLoop () =
  (
    (* print_menu "Press '?' for help, 'q' for quit" ; *)
    status () ; ) ;
  mode mainMode ;;
