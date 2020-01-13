(* type class for ocaml ? *)
(* on peut envisager une version encore moins typée mor : ident_mor of String | Stuff of (mor list) list *)

open Base


let levelStuffType : stuffType -> level  = function
    Composition -> 3.0
  | Other -> 4.0
let precPrefixCursors : level = 5.0

let stuffTypeInfixStr = function
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

 

let identToString (c : ident) = match c with
  Name n -> n;;

let mvarToString (c : mvar) = "?" ^ identToString c

let cursorToString (env : env)(c : cursor) = match c with
  | CurMVar m -> mvarToString m
  | Nb n ->
    (if List.mem c env.activeCursors then "!" else "") ^ string_of_int n ;;

let mayEnclose outerPrec prec s =
  if outerPrec >= prec then
    "(" ^ s ^ ")"
  else
    s


let rec datToString (env : env) (d : dat) =
  match d with
    Ident i -> identToString i
  | MVar m -> mvarToString m
  | Stuff s -> stuffDataToString env s
and datCursorToString env (d : datCursor) =
   if env.printCursors && d.cursors <> [] then
     joinAround "{" "}@" false " " (List.map (cursorToString env) d.cursors) ^
     datToString {env with outerPrec = precPrefixCursors} d.data 
   else 
       datToString env d.data 
   
(* and datCursorToStringl (printCursors : bool) (dl : datCursor list) = join " ∘ " (List.map datCursorToString dl) *)
and stuffDataToString (env : env) (s : stuffData) =
  let prec = levelStuffType s.stTyp in
  let sl = (List.map (datCursorToString {env with outerPrec = prec}) s.stList) in
  mayEnclose env.outerPrec prec
    (join
    (* (match s.stList with [ {data = Ident _} ] -> true | _ -> false ) *)
       (stuffTypeInfixStr s.stTyp) sl)
;;

let string_to_datCursor (s : string) : datCursor =
            let lexbuf = Lexing.from_string s in
            let result = ParserExpr.main LexerExpr.token lexbuf in
     result ;;



