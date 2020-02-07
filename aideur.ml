(* type class for ocaml ? *)
(* on peut envisager une version encore moins typée mor : ident_mor of String | Stuff of (mor list) list *)

open Base ;;

open Lib ;;



(* un truc qui assigne à chaque identifiant un type *)

(* type ntTrans_data = { foncteurSource : dat; foncteurTarget : dat } ;;
 * type mor_data = { objectSource : dat; objectTarget : dat } ;;
 * 
 * type type_ident = 
 *   NtTrans of ntTrans_data 
 * | Mor of mor_data
 * 
 * type env = Hash.Tbl ident type_ident *)


(* On pourrait créer des curseurs pour chaque occurence ! *)
let dcRemoveCursor (c: cursor)(d : datCursor)  =
  {d with cursors = listRemove c d.cursors  } ;;
let dcMemCursor (c : cursor)(d : datCursor) = List.mem c d.cursors;;
let dcAddCursor (c: cursor)(d : datCursor)  =
  {d with cursors = c :: d.cursors  } ;;
let dcFilterCursor (f : cursor -> bool) (d : datCursor)  =
  {d with cursors = List.filter f d.cursors } ;;

let rec filterCursor_rec (f : cursor -> bool) (d : datCursor) =
  dcFilterCursor f 
  (match d.data with
    Ident _ | MVar _ -> d
  | Stuff l -> {d with data = Stuff {l with stList = filterCursor_recl f l.stList}})
and filterCursor_recl(f : cursor -> bool) (l : datCursor list) =
  List.map (filterCursor_rec f) l;;


let removeOtherCursors (c : cursor)(l : datCursor list) : datCursor list =
  filterCursor_recl (fun c2 -> c = c2) l;;



let cursorsAtDat (x : dat) (d : datCursor) =
  let n = ref (dcMaxCursor d) in
  let rec aux (d : datCursor) : datCursor =
    if dEq x d.data then
      (incr n ;
       dcAddCursor (Nb (! n)) d )
    else
      match d.data with
      | Stuff l ->
        { d with data = Stuff {l with stList = List.map aux l.stList}}
      | Ident _ | MVar _ -> d
  in
    aux d

let cursorsAtStr (x : string) (d : datCursor) :
     datCursor  =
  cursorsAtDat (Ident (Name x)) d ;;

type retUpCursor = NotFound | Found | Done

(* NotFound: il n'a pas trouvé, Found, il a besoin d'être remonté *)
let rec upCursorWithInfo(cc : cursor) (d : datCursor) : datCursor * retUpCursor =
  if dcMemCursor cc d then
    dcRemoveCursor cc d , Found
  else
    match d.data with
      Ident _ | MVar _ -> d , NotFound
    | Stuff st -> 
      let (dl2 , r) = upCursorlWithInfo cc st.stList in
      (
        match r with
          Done | NotFound -> { d with data = Stuff {st with stList = dl2} } , r
        | Found -> {  cursors = cc :: d.cursors ;  data = Stuff {st with stList = dl2} } , Done
      ) 

 and upCursorlWithInfo (cc : cursor) : datCursor list -> datCursor list * retUpCursor  = function
  t :: q ->
    let (t2 , r2) = upCursorWithInfo cc t in
    (match r2 with
          Done | Found -> (t2 :: q) , r2
        | NotFound ->
            let (q2 , s2) = upCursorlWithInfo cc q in
            (t2 :: q2) , s2)
| [] -> [] , NotFound

let upCursor cc d = fst (upCursorWithInfo cc d)


let rec downCursor (cc : cursor) (d : datCursor) : datCursor =
  if dcMemCursor cc d then
   (
    match d.data with
    Ident _ | MVar _ -> d
  | Stuff ({stList = (t :: q) ; _} as dd) ->
       dcRemoveCursor cc { d with data =
        Stuff {dd with stList = dcAddCursor cc t :: q }} 
  | Stuff {stList = [] ; _}  -> raise Not_found)
  else
    match d.data with
    | Ident _ | MVar _ -> d
    | Stuff l -> {d with data = Stuff {l with stList = List.map (downCursor cc) l.stList}}

(* could be defined as equations *)
let rec nextCursor (cc : cursor) (d : datCursor) : datCursor =
  match d.data with
    Ident _ | MVar _ -> d
  | Stuff l -> {d with data = Stuff {l with stList = nextCursorl cc l.stList}}
and nextCursorl (cc : cursor) = function
  t1 :: t2 :: q ->
     if dcMemCursor cc t1 then
         dcRemoveCursor cc t1 :: dcAddCursor cc t2 :: q
     else
(* En principe, au plus l'un des deux est nécessaire*)
        nextCursor cc t1 :: nextCursorl cc (t2 :: q)
  | [ t ] -> [ nextCursor cc t]
  | [] -> [];;

let rec prevCursor (cc : cursor) (d : datCursor) : datCursor =
  match d.data with
    Ident _ | MVar _ -> d
  | Stuff l -> {d with data = Stuff
      {l with stList = (prevCursorl cc l.stList )}}
and prevCursorl (cc : cursor) = function
  t1 :: t2 :: q ->
     if dcMemCursor cc t2 then
         dcAddCursor cc t1 :: dcRemoveCursor cc t2  :: q
     else
(* En principe, au plus l'un des deux est nécessaire*)
        prevCursor cc t1 :: prevCursorl cc (t2 :: q)
  | [ t ] -> [ prevCursor cc t]
  | [] -> [];;

(*
(* Put the cursor at the compteur-th occurence of x (typically, x is an identifier) *)
let newCursorAtDatl (cc : cursor)(x : dat)(compteur : int)(dl : datCursor list) : datCursor list =
  let n = ref compteur in
   let rec aux d =
      if n.val = 0 then d else
        if x = d.data then
              n.val = n.val - 1 ;
              if n.val = 0 then
                dcAddCursor d cc
              else
                d
        else
           match d.data with
               | Stuff l -> { d with data = Stuff (List.map aux l)}
               | Ident _ -> d
   in
      List.map aux dl ;;

let newCursorAtDat (cc : cursor)(x : ident)(compteur : int)(d : datCursor) : datCursor =
  List.head (newCursorAtDatl cc x cmpteur [ d ]) ;;
*)
   
(* returns a function that need additional data: what is the other sides of j and n ?
 *)
