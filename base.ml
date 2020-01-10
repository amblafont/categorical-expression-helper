open Lib ;;

type ident = Name of string ;;
type mvar = ident
type cursor = Nb of int | CurMVar of mvar

let cursorToInt = function
    Nb n -> n
  | CurMVar _ -> 0 ;;

type stuffType = Composition | Other ;;
type stuffData = { stTyp : stuffType ; stList : datCursor list}
  and dat = Ident of ident | Stuff of stuffData | MVar of mvar
  and datCursor = { data : dat ; cursors : cursor list}

(* let updateMapStuffData (f : datCursor -> datCursor)(d : stuffData) : stuffData =
 *   { d with stList = List.map f d.stList } *)

(* let dcSetDataFromStuffList (d : datCursor)(st : stuffData)(l : datCursor list) =
 *   { d with data = Stuff {st with stList = l}} *)

type level = float;;
type env = {activeCursors : cursor list ; printCursors : bool ;
            (* precedence à afficher *)
           outerPrec : level};;


let emptyEnv = {activeCursors = [] ; printCursors = true ; outerPrec = 0.} ;;

let emptyStuffData = { stTyp = Composition ; stList = []}
let newDatCursor (d : dat) : datCursor = { data = d ; cursors = []};;
let mkIdentDatCursor (s : string) : datCursor = newDatCursor (Ident (Name s)) ;;
let mkListDat (d : datCursor list)(st : stuffType ) : dat =
   (Stuff {stTyp = st ; stList = d}) ;;
let mkListDatCursor (d : datCursor list)(st : stuffType ) : datCursor =
   newDatCursor (Stuff {stTyp = st ; stList = d}) ;;

let rec listCursors (d : datCursor) : cursor list =
  match d.data with
  | Ident _ | MVar _ -> d.cursors 
  | Stuff st -> d.cursors @ List.flatten (List.map listCursors st.stList)

let rec listActiveCursors (env : env)(d : datCursor) : cursor list =
  List.filter (fun x -> List.mem x env.activeCursors) (listCursors d)

let rec dcListDatMVars (d : datCursor) : mvar list =
  match d.data with
  | Ident _ -> []
  | MVar x -> [ x ]
  | Stuff st -> List.flatten (List.map dcListDatMVars st.stList)

let rec dcListCursorMVars (d : datCursor) : mvar list =
  List.flatten (List.map (function CurMVar x -> [x] | _ -> []) d.cursors) @
  (match d.data with
   | Ident _ | MVar _ -> []
   | Stuff st ->  List.flatten (List.map dcListCursorMVars st.stList))

let rec dcListDatMVars (d : datCursor) : mvar list =
  match d.data with
  | Ident _ -> []
  | MVar x -> [ x ]
  | Stuff st -> List.flatten (List.map dcListDatMVars st.stList)


let rec removeAllCursors (d : datCursor) : datCursor =
  {
    (* d with *)
    cursors = [] ;
    data = 
      (match d.data with
        | Ident _ | MVar _ -> d.data
        | Stuff st -> Stuff {st with stList = List.map removeAllCursors st.stList})
                        (* (updateMapStuffData removeAllCursors st)) *)
  }

let rec dcMaxCursor (d : datCursor) : int  =
  let m = maxList 0 (List.map cursorToInt d.cursors) in
  match d.data with
  | Ident _ | MVar _-> m
  | Stuff l -> maxList m (List.map dcMaxCursor l.stList) 


let cursorsAreUnique (d : datCursor) : bool =
  listNoDup (listCursors d)


let datCursorIsValid (d : datCursor) : bool =
  cursorsAreUnique d ;;

let noMVars (d : datCursor) : bool =
  dcListCursorMVars d = [] && dcListDatMVars d = []

(** permet de comparer vraiment les structures *)
let dcEraseFioritures (d : datCursor) : datCursor =
  removeAllCursors d

let dcEq (d : datCursor)(d2 : datCursor) : bool =
  dcEraseFioritures d = dcEraseFioritures d2

let dEq (d : dat) (d2 : dat) : bool =
  dcEq (newDatCursor d)(newDatCursor d2)




