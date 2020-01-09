
open Base ;;
open Stringstuff ;;
open Aideur;;


let mainExpr = ref Stringstuff.test ;;
let mainCur = ref (Nb 0);;

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
  status()

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

let iset () =
  print_menu "Set menu" ;
  print_endline "c: set cursor\ne : set expression\nq : cancel\n" ;
  let rec local_loop () =
    print_prompt "Set menu" ;
    match get1charn () with
    'q' -> ()
    | 'c' -> isetCursor ()
    | 'e' -> isetExpression ()
    | _ -> print_endline "Invalid key" ;local_loop ()
  in
    local_loop()

let icursorsAtIdentifiers () =
  print_endline "
----
Choose an identifier where new cursors will be created in the expression
" ;
  print ();
  curAtStr (read_line ()) ;
  isetCursor () ;;

let printHelp () =
   print_menu "Help" ;
   print_endline 
    "q : Quit
? : This help

' ' : status

h,j,k,l : vim-like cursor movements

sc : set cursor
se : set expression

c : cursors at identifiers" ;;

let startLoop () =
  (print_menu "Main menu: Press '?' for help" ;
   status () ; ) ;
  (* let shouldPrint = ref true in *)
    try
      while true do
        print_prompt "Main menu" ;
        (* (if ! shouldPrint then 
         *    print_flush "> "
         *  else shouldPrint := true); *)
        match get1charn () with
        | 'q' -> raise Exit
        | '?' -> printHelp ()
        | 'h' -> h ()
        | 'j' -> j ()
        | 'k' -> k ()
        | 'l' -> l ()
        | 's' -> iset ()
        | ' ' -> status_with_menu ()
        | 'c' -> icursorsAtIdentifiers ()
        | _ -> print_endline "Invalid key (press ? for help)"
      done
    with
      Exit -> ();;
