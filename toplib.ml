
open Base ;;
open Stringstuff ;;
open Aideur;;


let mainExpr = ref (string_to_stuffData "F j ; n y ") ;;
let mainCur = ref (Nb 1);;

let setCur (n : int) = mainCur := Nb n;;

let printExpr (stuff : stuffData)  = 
  print_endline (stuffDataToString true stuff) ;;

let print () = printExpr (! mainExpr);;

let print_menu (str : string) =
  Printf.printf
    "-------------
%s
-------------\n" str

let status () =
    print();
  print_endline ("Current cursor: " ^ cursorToString (! mainCur)) ;;

let status_with_menu () =
  print_menu "Status" ;
  status () ;;

let stepDat (f : datCursor list -> datCursor list) =
  mainExpr := { ! mainExpr with stList = f (! mainExpr).stList} ;
  status() ;;

(* put the cursor on 'j' *)
stepDat (cursorsForStrl "j") ;;

let stepDatCursor (f : cursor -> datCursor list -> datCursor list) =
  mainExpr := { ! mainExpr with stList = f (! mainCur) (! mainExpr).stList} ;
  status()
  

(* vim-like commands *)
let l () = stepDatCursor nextCursorl ;;
let h () = stepDatCursor prevCursorl ;;
let k () = stepDatCursor upCursorl ;;
let j () = stepDatCursor downCursorl ;;

let curAtStr (s : string) = stepDat (cursorsForStrl s) ;;

let print_flush str = print_string str ; flush stdout;;

(*
https://stackoverflow.com/questions/13410159/how-to-read-a-character-in-ocaml-without-a-return-key

   breaks portability
*)
let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false ; Unix.c_echo = true } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

let get1charn () =
  let res = get1char () in
  print_newline ();
  res;;

let print_prompt (menu : string) =
  print_newline () ;
  print_flush (menu ^ "> ")


let isetCursor () =
  Printf.printf "Enter new value for cursor (currently %s): " (cursorToString (! mainCur)) ;
  setCur (read_int ()) ;;

let isetExpression () =
  print_string "Enter new expression: "  ;
  mainExpr := (string_to_stuffData (read_line ())) ;
  status ();;


let icursorsAtIdentifiers () =
  print_endline "
----
Choose an identifier where new cursors will be created in the expression
" ;
  print ();
  curAtStr (read_line ()) ;
  isetCursor () ;;


let quitMenu () = raise Exit ;;

(* char: character to activate the option,
   the string describes the mode, and the function does the stuff
*)
type mode = { commands : (char * (string * (unit -> unit))) list; prompt : string } ;;

let helpMode m =
  print_menu m.prompt ;
  print_endline "?: help\nq: quit mode\n" ;
  List.iter (function (c, (s, _)) -> Printf.printf "%c: %s\n" c s) m.commands ;;


let mode (l : mode)  =
  helpMode l;
  try
    while true do
      print_prompt l.prompt;
      let c = get1charn () in
      match c with
      | 'q' -> raise Exit
      | '?' -> helpMode l
      | c ->
        match List.assoc_opt c l.commands with
          Some (_, x) -> x ()
        | None -> print_endline "Invalid key (press ? for help)"
    done
  with
    Exit -> ();;

let wrapMode (l : mode) () = mode l 

let statusCmd =  ' ' , ("print expression and current cursor" , status)


let cursorMode : mode =
  { commands = 
  [  'h' , ("move left", h );
    'j' , ("move down", j) ;
    'k' , ("move up", k) ;
    'l' , ("move right", l) ;
    's' , ("set cursor" , isetCursor) ;
    'n' , ("erase cursors and create new ones at identifiers" , icursorsAtIdentifiers) ;
     statusCmd 
  ] ; prompt = "Cursor mode" };;

let expressionMode =
  { commands = 
      [  's' , ("set expression" , isetExpression) 
      ] ; prompt = "Expression mode" };;

let mainMode : mode =
  { commands =[
    'c' , ("cursor mode", wrapMode cursorMode) ;
    'e' , ("expression mode", wrapMode expressionMode) ;
    statusCmd 
  ] ; prompt = "Main menu"
  }

let startLoop () =
  (
    (* print_menu "Press '?' for help, 'q' for quit" ; *)
   status () ; ) ;
  mode mainMode ;;
