
open Base ;;
open Stringstuff ;;
open Aideur;;
open Lib;;

(* TODO: make isActive a property of cursors (then, I can display it specially)
   a quick pass should allow to recover the active cursor

   allow multiple isActive
   then keepOnlyActive
   setActive
   marqué par un !
*)


let mainExpr = ref (string_to_datCursor "((F 0@j) ; (n y)) ") ;;
let mainEnv = ref { activeCursors = [Nb 0] ; printCursors = true ; outerPrec = 0.} ;;
(* let mainCur = ref (Nb 1);; *)

(* let setCur (n : int) = mainCur := Nb n;; *)


let statusStr () = 
  (datCursorToString (! mainEnv) (! mainExpr )) ;;

let print () = 
  print_endline (statusStr ()) ;;


let print_menu (str : string) =
  Printf.printf
    "-------------
%s
-------------\n" str

let status () =
    print()
  (* print_endline ("Current cursor: " ^ cursorToString (! mainCur)) ;; *)

let status_with_menu () =
  print_menu "Status" ;
  status () ;;

(* let stepDat (f : datCursor list -> datCursor list) =
 *   mainExpr := { ! mainExpr with stList = f (! mainExpr).stList} ;
 *   status() ;; *)

(* put the cursor on 'j' *)
(* stepDat (cursorsForStrl "j") ;; *)

(* let stepDatCursor (f : cursor -> datCursor list -> datCursor list) =
 *   mainExpr := { ! mainExpr with stList = f (! mainCur) (! mainExpr).stList} ;
 *   status() *)

exception NeedsExactlyOneActiveCursor ;;

let getTheCursor () = 
  match listActiveCursors (! mainEnv.activeCursors) (! mainExpr) with
    [ t ] -> t
  | _ -> raise NeedsExactlyOneActiveCursor ;;

let stepDatCursor (f : cursor -> datCursor -> datCursor) =
   mainExpr := f (getTheCursor ()) (! mainExpr) ; status ()
           (* print_endline "Exactly one cursor must be active"  ;; *)
  

(* vim-like commands *)
let l () = stepDatCursor nextCursor ;;
let h () = stepDatCursor prevCursor ;;
let k () = stepDatCursor upCursor ;;
let j () = stepDatCursor downCursor ;;

let curAtStr (s : string) = mainExpr := cursorsAtStr s (! mainExpr)

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

let get1int () =
  int_of_string (string_of_char (get1char ())) ;;

let get1intn () =
  int_of_string (string_of_char (get1charn ())) ;;

let get1intMultiFiguren () =
  print_flush "(press space for many-digits number) ";
  match get1char () with
    ' ' -> read_int ()
  | c -> print_newline (); int_of_string (string_of_char c)

let print_prompt (menu : string) =
  print_newline () ;
  print_flush (menu ^ "> ")


(* let isetCursor () =
 *   Printf.printf "Enter new value for cursor (currently %s): " (cursorToString (! mainCur)) ;
 *   setCur (read_int ()) ;; *)

(* let input_datCursor () = string_to_datCursor (read_line ());; *)

let getDatCursor () =
  let aux () =
    let s = read_line () in
    if s = "cancel" then raise Exit else
      try
        let d = string_to_datCursor s in
        let l = Lib.listListDup (listCursors d) in
        if l <> [] then
          (print_endline ("Invalid expression: the following cursors appear many times: " ^ join " " (List.map (cursorToString (!mainEnv)) l));
           None)
        else if not (datCursorIsValidInEnv (!mainEnv).activeCursors d) then
          (print_endline "Invalid expression" ; None)
        else if not (noMVars d) then
          (print_endline "Metavariables not allowed" ; None)
        else
          Some d
      with 
        Parsing.Parse_error -> print_flush "Parse error. "; None
  in
  let rec loop () = 
    match aux () with
      None -> print_endline " Please try again (write 'cancel' to cancel)" ; 
       loop()
    | Some x -> x
  in
  loop() ;;

let mayGetDatCursor () =
  try getDatCursor () with
  Exit -> ! mainExpr ;;


let isetExpression () =
  print_string "Enter new expression: "  ;
  mainExpr := mayGetDatCursor () ;
  status ();;


let icursorsAtIdentifiers () =
  print_endline "
----
Choose an identifier where new cursors will be created in the expression
" ;
  print ();
  curAtStr (read_line ()) ;;


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
  print_menu ( statusStr ())  ;
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

(* let ntInferToLeftToString (n : ntInfer) : string =
 *   let s = datCursorToString false in
 *   Printf.sprintf "(natural transformation) %s : %s => ?1\n%(morphism)s : ?2 -> %s"
 *     (s n.nat) (s n.funct) (s n.mor) (s n.obj) *)

open Equation ;;

  

let equation_rewrite (e : equation)() =
  let (inf, _) = findMatchingDatCursor (! mainEnv).activeCursors e.lhs [] (! mainExpr)
  in
  let e' = equationSubst inf e in
  print_endline (equationToString e') ;
  print_endline "-----------" ;
  let la = dclListDatMVars e'.rhs.stList in
  let lc = dclListCursorMVars e'.rhs.stList in
  if la <> [] then 
  ( print_string "Enter the following unspecified meta-variables: " ;
  print_endline (join " " (List.map mvarToString la)) ;
  print_endline "(type cancel to cancel)" ;)  ;
  let la' = List.map
      (fun m ->
         print_flush (mvarToString m ^" ") ;
         (m , (getDatCursor ()).data)
      )
      la
  in
  if lc <> [] then 
    (print_string "Enter the following unspecified cursors:" ;
   print_endline (join " " (List.map mvarToString lc)) ;);
  let lc' = List.map
      (fun m ->
         print_flush (mvarToString m ^" ") ;
         (m , Nb (get1intMultiFiguren ()))
      )
      lc
  in
  let rhs' = stuffDataSubstMVars true { dcMVars = la' ; cursorMVars = lc'} e'.rhs in
  let (_, e') = findMatchingDatCursor (! mainEnv).activeCursors e.lhs (rhs'.stList)
      (! mainExpr)
  in
  mainExpr := e' ;;

let equation_to_command
    (e : equation) : string * (unit -> unit)
  = (equationToShortString e, equation_rewrite e)

let equation_to_mode (e : equation)(prompt : string) : 
  mode =
  (* (char * (string * (unit -> unit))) list = *)
  let e2 = equation_swap e in
  {commands = 
  [ 'h', equation_to_command e2 ;
    'l', equation_to_command e
    
  ] ; prompt = prompt }

  (*
let ntMode : mode =
  { commands = [


        statusCmd 
      ] 
  ; prompt = "Natural transformations"
  }
*)

let printActivatedCursors () =
  print_endline ("Activated cursors: [" ^ (join " " (List.map (cursorToString (!mainEnv)) (listActiveCursors (! mainEnv).activeCursors(! mainExpr)))) ^ "]") ;;

let printTheoreticalActivatedCursors () =
  print_endline ("Theoretically activated cursors: [" ^ (join " " (List.map (cursorToString (!mainEnv)) (! mainEnv).activeCursors)) ^ "]") ;;

let printCursors () =
  print_endline ("Cursors: [" ^ (join " " (List.map (cursorToString (!mainEnv)) (listCursors (! mainExpr)))) ^ "]") ;;

let iactiveCursor () =
  print_flush "Enter a cursor to activate " ;
  mainEnv := {! mainEnv with activeCursors = Nb (get1intMultiFiguren ()) :: (! mainEnv).activeCursors} ;
  (* printCursors () ;
   * printTheoreticalActivatedCursors () ; *)
  printActivatedCursors () ;
  status () ;;

let idesactiveCursor () =
  print_flush "Enter a cursor to desactivate " ;
  mainEnv := {! mainEnv with activeCursors = listRemove (Nb (get1intMultiFiguren ())) (! mainEnv).activeCursors} ;
  status () ;;



let cursorMode : mode =
  { commands = 
  [ 
     'j' , ("move down", j) ;
    'k' , ("move up", k) ;
     'h' , ("move left", h );
     'l' , ("move right", l) ;
     (* The advantage of h and l is that they don't care about the type of
     composition. But we could also ignore this! *)
     'H' , equation_to_command( equation_swap eq_move_cursor );
     'L' , equation_to_command  eq_move_cursor ;
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
    'n' , ("natural transformation mode (experimental)", wrapMode (equation_to_mode eq_nat_trans "Natural Transformations")) ;
    statusCmd 
  ] ; prompt = "Main menu"
  }


let startLoop () =
  (
    (* print_menu "Press '?' for help, 'q' for quit" ; *)
   status () ; ) ;
  mode mainMode ;;
