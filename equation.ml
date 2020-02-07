
(* a module for equations *)
open Base ;;
open Stringstuff ;;
open Lib ;;

(* équation entre listes (comme ça on n'a pas le pb d'associativité ) *)
type handside = stuffData

exception CastingError;;

let cast_dat_to_stuffData (d : dat) : stuffData =
  match d with
  |  Stuff l -> l
  | _ -> raise CastingError
    
let string_to_stuffData (s : string) : stuffData =
  cast_dat_to_stuffData (string_to_datCursor s).data ;;


type inferMVar =
  {
    dcMVars : (ident * dat) list ;
    cursorMVars : (ident * cursor) list
  }

exception Incoherent;;

(* raise Incoherent is same key has multiple values *)
let rec removeDup (l : ('a * 'b) list) : ('a * 'b) list =
  match l with
    [] -> []
  | (a , b) :: q ->
    (match List.assoc_opt a q with
       None -> (a, b) :: removeDup q
    | Some z -> if b = z then removeDup q else raise Incoherent)


(* let checkCompatible (m1 : inferMVar)(m2 : inferMVar) =
 *   agreeEveryWhere (m1.dcMVars @ m2.dcMVars ) 
 *   && 
 *     agreeEveryWhere (m1.cursorMVars @ m2.cursorMVars )  *)

(* and checkes compatibility*)
(* let concatInferMVar (m1 : inferMVar) (m2 : inferMVar) : inferMVar =
 *   { dcMVars = m1.dcMVars @ m2.dcMVars ;
 *     cursorMVars = m1.cursorMVars @ m2.cursorMVars} *)

let checkAndIncInferMVar (m1 : inferMVar)(m2 : inferMVar) : inferMVar =
  (* usually, m2 is shorter than m1 *)
  { dcMVars = removeDup (m1.dcMVars @ m2.dcMVars) ;
    cursorMVars = removeDup (m1.cursorMVars @ m2.cursorMVars) } 

exception MVar_not_found of ident ;;


(* let listo_to_list (l : ('a list) option) : 'a list =
 *   match l with
 *   Some l -> l
 *   | None -> [] *)

let emptyInferMVar = { dcMVars = [] ; cursorMVars = []} ;;
(* c'est un travail de parsing qu'on fait ici. Todo: réfléchir à comment
réutilsier le truc

   Does two things: try to match the pattern and replace the stuff if
   the pattern is replaced with replaceWith, if provided.
   If provided, then it ignores the fact the tail is not consumed completely,
   and also returns the updated cursor list
   (in the other cases, the updated cursor list is meaningless)

   This hackish definition is to avoid copy and paste
   the second projection is meaningful only in case of PrefixMatchReplaceWith
*)
(* type matchDatCursorMode = PrefixMatch | PrefixMatchReplaceWith of (datCursor list) | ExactMatch ;; *)
(* returns what was missing *)
let rec matchDatCursorlHeadWith 
    (activeCursors : cursor list) (hand : datCursor list)(dl : datCursor list) :
  (* list associative *)
  inferMVar * datCursor list
   =
  match hand,dl with
  | [] , l -> emptyInferMVar, l
  | t :: q , t' :: q' ->
    let m1 = matchDatCursorHeadWith activeCursors t t' in
    let m2,q'2 = matchDatCursorlHeadWith activeCursors q q' in
    (try
      checkAndIncInferMVar m1 m2
     with Incoherent -> raise Not_found) , q'2
  | _, _ -> raise Not_found
and matchExactDatCursorlHeadWith 
    (activeCursors : cursor list) (hand : datCursor list)(dl : datCursor list)
  : inferMVar =
  match matchDatCursorlHeadWith activeCursors hand dl with
    (inf , []) -> inf
  | _ -> raise Not_found

and matchDatCursorHeadWith (activeCursors : cursor list)(d : datCursor)(d' : datCursor) :
    inferMVar =
  let infCursors = matchCursorsHeadWith activeCursors d.cursors d'.cursors in
  let infDat = matchDatHeadWith activeCursors d.data d'.data in
  checkAndIncInferMVar infCursors infDat
and matchCursorsHeadWith (activeCursors : cursor list)(c : cursor list)(c' : cursor list) : inferMVar
  =
  List.fold_left
    checkAndIncInferMVar
    emptyInferMVar (List.map (fun x -> matchCursorHeadWith activeCursors x c') c)
and matchCursorHeadWith (activeCursors : cursor list)(c : cursor )(c' : cursor list) : inferMVar
  =
  match c with
    Nb _ -> if List.mem c c' then emptyInferMVar else raise Not_found
  | CurMVar x ->
    match List.filter (fun x -> List.mem x activeCursors) c' with
      t :: _ -> { dcMVars = [] ; cursorMVars = [ (x , t) ]}
    | [] -> raise Not_found
and matchDatHeadWith (activeCursors : cursor list)(d : dat)(d' : dat) :
  inferMVar =
  match d, d' with
    MVar v , d' ->
    { dcMVars = [(v , d')] ; cursorMVars = []}
                     (* vérifier que c'est dedans *)
    | Ident x , Ident x' when x = x' -> emptyInferMVar
    | Stuff st , Stuff st' ->
      matchStuffDataHeadWith activeCursors st st'
    | _,_ -> raise Not_found
and matchStuffDataHeadWith (activeCursors : cursor list)(st : stuffData) (st' : stuffData) : inferMVar =
  if st.stTyp = st'.stTyp then
    matchExactDatCursorlHeadWith activeCursors st.stList st'.stList
  else
    raise Not_found

    (* and matchCursorsWith _ _ = true *)


(* the returned second projection says what happens if replacing the matching [h] with [replaceWith] *)
let rec findMatchingDatCursor (activeCursors : cursor list)(h : stuffData)(replaceWith : datCursor list)(d : datCursor) : inferMVar * datCursor =
  (match d.data with
   | Stuff st ->
     let inf , st' = findMatchingStuffData activeCursors h replaceWith st in
     (inf , {d with data = Stuff st'})
   | _ -> raise Not_found )
and findMatchingStuffData (activeCursors : cursor list)(st : stuffData) (replaceWith : datCursor list)(st' : stuffData) : inferMVar * stuffData =
    if st.stTyp = st'.stTyp then
      let inf, l = findMatchingDatCursorl activeCursors st replaceWith st'.stList in
      inf , {st' with stList = l}
    else
      raise Not_found
and  findMatchingDatCursorl (activeCursors : cursor list)(h : stuffData)(replaceWith : (datCursor list))(d : datCursor list) : inferMVar * datCursor list =
  let h' : datCursor list = h.stList in
  try
    let (inf, dl) = matchDatCursorlHeadWith activeCursors h' d in
    inf , replaceWith @ dl
  with
    Not_found ->
    match d with
      [] -> raise Not_found
    | t :: q ->
      try
        let inf, d = findMatchingDatCursor activeCursors h replaceWith t in
        inf , d :: q
      with
      Not_found -> findMatchingDatCursorl activeCursors h replaceWith q

let cursorSubstMVars (raiseExc : bool)
    (l : (ident * cursor) list) (c : cursor) : cursor =
  match c with | Nb _ -> c | CurMVar x ->
  try List.assoc x l with
    Not_found -> 
                   if raiseExc then
                     raise (MVar_not_found x)
                   else
                     CurMVar x

let cursorsSubstMVars (raiseExc : bool)(l : (ident * cursor) list) (cl : cursor list) : cursor list = List.map (cursorSubstMVars raiseExc l) cl

let dcMVarSubstMVars (raiseExc : bool)(env : inferMVar) (x : mvar) : dat =
  try List.assoc x env.dcMVars with
    Not_found ->
    if raiseExc then
      raise (MVar_not_found x)
    else
      MVar x

let rec datSubstMVars (raiseExc : bool)(env : inferMVar) (d : dat) : dat =
  match d with 
    Ident _ -> d
  | Stuff st ->
    Stuff (stuffDataSubstMVars raiseExc env st)
  | MVar x -> dcMVarSubstMVars raiseExc env x
and stuffDataSubstMVars (raiseExc : bool)(env : inferMVar) (s : stuffData) : stuffData =
  { s with stList = datCursorlSubstMVars raiseExc env s.stList }
and datCursorSubstMVars (raiseExc : bool) (env : inferMVar) (d : datCursor) : datCursor =
  { cursors = cursorsSubstMVars raiseExc env.cursorMVars d.cursors ;
    data = datSubstMVars raiseExc env d.data}
  and datCursorlSubstMVars (raiseExc : bool)(env : inferMVar) (l : datCursor list) : datCursor list =
    List.map (datCursorSubstMVars raiseExc env) l


type strToken = Str of string
              (* | CurMVar of mvar *)
              | MVar of mvar ;;

let tokenToString (t : strToken) : string =
  match t with Str s -> s | MVar x -> mvarToString x

(* let mvarMaySubstToString (env : inferMVar) (m : mvar) : string =
 *   (match List.assoc_opt m env.dcMVars with
 *      Some d -> datToString emptyEnv d
 *    | None -> mvarToString m
 *   ) *)

let strTokenSubst (env : inferMVar)(t : strToken) : strToken =
  match t with
    Str _ -> t
  | MVar x ->
    try Str (datToString emptyEnv (dcMVarSubstMVars true env x)) with
      MVar_not_found _ -> t

(* let strToken (env : inferMVar) (s : strToken) : string =
 *     match s with
 *       Str s -> s
 *       (\* | CurMVar x ->
 *        *   (match List.assoc_opt x env.cursorMVars with
 *        *      Some d -> cursorToString emptyEnv d
 *        *    | None -> mvarToString x
 *        *   ) *\)
 *       | MVar x -> mvarMaySubstToString env x *)

let tokensToString (l : strToken list) : string =
  join "" (List.map tokenToString  l) ;;
  
(* ?x for an expression metavariable
   !x for a cursor metavariable
   no cursor metavariable
*)
let rec str_charl_to_tokenl (s : char list)(prev : string) : strToken list =
  match s with
  [] -> [ Str prev ]
  | '?' :: q -> Str prev :: dcMVar_charl_to_tokenl q ""
  (* | '!' :: q -> Str prev :: curMVar_charl_to_tokenl q "" *)
  | c :: q -> str_charl_to_tokenl q (prev ^ (string_of_char c))
and dcMVar_charl_to_tokenl (s : char list)(prev : string) : strToken list =
  match s with
    [] -> [MVar (Name prev)]
  | ('a'..'z' as c) :: q
  | ('A'..'Z' as c) :: q
    ->
    print_endline "ici" ;
    dcMVar_charl_to_tokenl q (prev ^ (string_of_char c))
  | c :: q -> MVar (Name prev) :: str_charl_to_tokenl q (string_of_char c) ;;


let string_to_strToken (s : string) : strToken list =
  str_charl_to_tokenl (explode s) ""

type equation = { lhs : datCursor list ; rhs : datCursor list ;
                  typ : stuffType ; str : strToken list} ;;

let lhs_stData (e : equation) : stuffData = { stTyp = e.typ ; stList = e.lhs }
let rhs_stData (e : equation) : stuffData = { stTyp = e.typ ; stList = e.rhs }

let equationSubst (env : inferMVar) (e : equation) : equation =
  { 
    lhs = datCursorlSubstMVars false env e.lhs ;
    rhs = datCursorlSubstMVars false env e.rhs ;
    typ = e.typ ;
    str = List.map (strTokenSubst env) e.str ;
  }

(*
    tokensToString (string_to_strToken "(natural transformation): ?n : ?F ⇒ ?G (morphism) ?f : ?x → ?y")
   match 'b' with 'a' .. 'z' -> "oui" | _ -> "non" ;;
   findMatchingDatCursor [ Nb 0 ] equation_nt.rhs None (! mainExpr)
   findMatchingDatCursor [ Nb 0 ] equation_nt.lhs None (! mainExpr)
*)

(* type equation = handside * handside;; *)

let equationToShortString (e : equation) : string =
  stuffDataToString emptyEnv (lhs_stData e) ^ "  =  " ^ stuffDataToString emptyEnv (rhs_stData e) ;;

let equationToString (e : equation) : string =
  equationToShortString e ^ "\n\n" ^
  tokensToString e.str ;;


let equation_swap (e : equation) : equation =
  { e with lhs = e.rhs; rhs = e.lhs }


