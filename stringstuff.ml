(* type class for ocaml ? *)
(* on peut envisager une version encore moins typée mor : ident_mor of String | Stuff of (mor list) list *)

open Base

let stuffTypeInfixStr  = function
   Composition -> " ; "
 | Other -> " ";;


let rec join (ch : string) = function
   | [] -> ""
   | [ t ] -> t
   | t :: q -> t ^ ch ^ join ch q ;;

let joinAround (before : string)(after : string)(alone : bool)(ch : string) = function  
   | [] -> ""
   | [ t ] when alone -> t
   | l -> before ^ join ch l ^ after ;;

 
let cursorToString (c : cursor) = match c with
  Nb n -> string_of_int n ;;

let identToString (c : ident) = match c with
  Name n -> n;;

let rec datToString (printCursors : bool) (d : dat) =
  match d with
    Ident i -> identToString i
  | Stuff s -> stuffDataToString printCursors s
and datCursorToString (printCursors : bool) (d : datCursor) =
   let s = datToString printCursors d.data in
   if printCursors && d.cursors <> [] then
     joinAround "{" "}@" false " " (List.map cursorToString d.cursors) ^ s
   else s
   
(* and datCursorToStringl (printCursors : bool) (dl : datCursor list) = join " ∘ " (List.map datCursorToString dl) *)
and stuffDataToString (printCursors : bool) (s : stuffData) =
  let sl = (List.map (datCursorToString printCursors) s.stList) in
  joinAround "(" ")" false
    (* (match s.stList with [ {data = Ident _} ] -> true | _ -> false ) *)
  (stuffTypeInfixStr s.stTyp) sl
;;

let string_to_stuffData s =
            let lexbuf = Lexing.from_string s in
            let result = Parser.main Lexer.token lexbuf in
     result ;;

let test = string_to_stuffData "F j ; n y " ;;


