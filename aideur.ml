(* type class for ocaml ? *)
(* on peut envisager une version encore moins typée mor : ident_mor of String | Stuff of (mor list) list *)

open Base ;;
open Stringstuff ;;
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
  | Stuff ({stList = (t :: q)} as dd) ->
       dcRemoveCursor cc { d with data =
        Stuff {dd with stList = dcAddCursor cc t :: q }} 
  | Stuff {stList = []}  -> raise Not_found)
  else
    match d.data with
    | Ident _ | MVar _ -> d
    | Stuff l -> {d with data = Stuff {l with stList = List.map (downCursor cc) l.stList}}

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
type ntInfer =
   { nat : datCursor ;
     funct : datCursor ;
     mor : datCursor ;
     obj : datCursor ;
   }
type ntRequire = { funct : datCursor ; obj : datCursor } ;;
(* does not check anything *)
(* F j ; {c}@n k) = @{c}n i ; G j *)
let rec natTransToLeftInferl (c : cursor) (l : datCursor list)
                 (* n *)     (* k *)     (* j *)
  : ntInfer =
  match l with
    { data = Stuff { stList = [ f ; j]} } ::
    { data = Stuff { stList = [ n ; k]} } ::
     q when dcMemCursor c n -> 
     { nat = n ; funct = f ; mor = j ; obj = k }
   | t :: q -> (try natTransToLeftInfer c t with
         Not_found -> natTransToLeftInferl c q)
   | [] -> raise Not_found
and natTransToLeftInfer (c : cursor) (d : datCursor) =
  match d.data with
     Ident _ | MVar _ -> raise Not_found
   | Stuff st -> natTransToLeftInferl c st.stList

let rec natTransToLeftl  
    (r : ntRequire)(c : cursor)(l : datCursor list)
  : datCursor list =
  match l with
    ({ data = Stuff { stList = [ f ; j]} } as ff) ::
    ({ data = Stuff { stList = [ n ; k]}} as nn) ::
     q when dcMemCursor c n -> 
      {nn with data = Stuff { stTyp = Other; stList = [ n ; r.obj]}}
      ::
      {ff with data = Stuff { stTyp = Other; stList = [ r.funct ; j]}}
      :: q
   | t :: q -> (try natTransToLeft c r t :: q with
        Not_found -> natTransToLeftl r c  q)
   | [] -> raise Not_found
and natTransToLeft  (c : cursor)(r : ntRequire)(d : datCursor) =
  match d.data with
     Ident _ | MVar _ -> raise Not_found
   | Stuff st -> { d with data =
            Stuff { st with stList = natTransToLeftl r c st.stList}}

    

   
 (*
  sprintf
   datCursorToString false n.nat 
   
   

    
(* faire les deux en même temps *)
type request = { id : int ; question: String; answer : dat option } ;;
type request = String list;;
type answer = 

let rec nat_trans_last_swap (foutre après ou avant) (c : cursor) (l : mors) =
  match l with
   funct_mor (F, j):: cursor_mor c2 :: nt_mor (n, x)  :: r when c = c2 -> 
     Some  question , (fun k G -> nt_mor n k :: cursor_mor c :: funct_mor (G , j)

type request = String list;;
type answer = 

let rec nat_trans_last_swap (foutre après ou avant) (c : cursor) (l : mors) =
  match l with
   funct_mor (F, j):: cursor_mor c2 :: nt_mor (n, x)  :: r when c = c2 -> 
     Some  question , (fun k G -> nt_mor n k :: cursor_mor c :: funct_mor (G , j)
  | t :: q -> nat_trans_last_swap t
(* 
let rec newCursorAtIdent (cc : cursor)(x : ident)(d : datCursor) : datCursor =
  match d.data with
   | Ident n -> if x = n then dcAddCursor d cc else d
   | Stuff l -> Stuff (newCursorAtIdentl cc x l)
and newCursorAtIdentl cc x l = List.map (newCursorAtIdentl cc x) l

  



data mor = ident of String | cursor of cursor | Stuff of (mor list);;
data mor = ident of String | cursor of cursor | Stuff of (mor list);;
exception invalidOperation;;

let rec max_cursor = function
  cursor n -> n
 | Stuff l -> max (map max_cursor l)
 | ident n -> 0 ;;

let max_cursor_mors = max (map max_cursor l);;

let fresh_cursor (mors : mor list) : cursor  = nb (max_cursor_mors l + 1);;
let rec rename_cursor_mor (c : cursor) (n : cursor) =
 | cursor c2 when c = c2 -> cursor n
 | Stuff l -> Stuff (map (remove_cursor_mor c n) l)
 | m -> m

let rename_cursor_mors c n = map (remove_cursor_mor c n)

  
  
let rec remove_cursor_mor (c : cursor) = 
   ident name -> [ ident name ]
 | cursor c2 -> if c = c2 then [] else [ cursor c2 ]
 | Stuff l -> Stuff (remove_cursor_mors c l)

and  remove_cursor_mors (c : cursor) = function
  t :: q -> remove_cursor_mor c t @ remove_cursor_mors c q
 [] -> []

    
let cursor_after_mor (c : cursor)(id : ident) = function
   ident name when name = id -> [ ident name ; cursor c ]
 | Stuff l -> 
    (Stuff (cursor_after_id_mors c id l))
 | _ -> raise Not_found
with cursor_after_id_mors (c : cursor) (id : ident) = function
 | [] -> raise Not_found
 | t :: q -> try (cursor_after_id_mor c id t :: q) with
      Not_found -> t :: (cursor_after_id_mor c id q) ;;

(** peut raise Found: il a trouvé le curseur, Not_found il ne l'a pas trouvé, et normal il a fait
ce qu'il fallait *)
let cursor_after_cursor_and_id_mor (c : cursor)(f : cursor)(id : ident) = function
 | cursor c2 when c2 = f -> raise Found
 | Stuff l -> 
    (Stuff (cursor_after_id_mors c id l))
with cursor_after_cursor_and_id_mors (c : cursor)(f : cursor)(id : ident) = function
 | [] -> raise Not_found
 | t :: q -> try (cursor_after_cursor_and_id_mors c id t :: q) with
   Not_found -> t :: (cursor_after_cursor_and_id_mors c id q)
   Found -> t :: (cursor_after_id_mor id q)
 ;;

let move_cursor_after_id (c : cursor)(id : ident) (mors : mor list) = 
  let fc = fresh_cursor mors in
  cursor_after_cursor_and_id fc id c >>=
  remove_cursor_mors c >>=
  rename_cursor_mors fc c ;;

  
  



(* return None if id not found, or the updated
let cursor_after_id_mors (c : cursor) (id : ident) mors = 
   lo_ol (map (cursor_after_id_mor c id) l mors)


data cursor_after_ret = AfterCursorNotFound | AfterCursorFound | Updated of mor list;;

let rec cursor_after_id_mors (c : cursor)(id : ident) = function
  t :: q -> match cursor_after_id_mor id t with
              Some l -> l @ q
              None -> t :: (cursor_after_id_mors q)
 | [] -> []
and

let rec cursor_after_cursor_and_id (c : cursor) (id : ident) = function
  t :: q -> match cursor_after_mor c id t with
              AfterCursorFound
   cursorNotFound -> t :: cursor_after_mors c id q
  [] -> []
let rec cursor_after_mor (c : cursor) (id : ident) = function
   ident name when name = id -> [ ident name ]
 | cursor c2 -> if c = c2 then [] else [ cursor c2 ]
 | Stuff l -> Stuff (remove_cursor_mors c l)


let rec cursor_after_mor (c : cursor) (n : int) (id : ident) = function
   ident name when name = id -> [ ident name ]
 | cursor c2 -> if c = c2 then [] else [ cursor c2 ]
 | Stuff l -> Stuff (remove_cursor_mors c l)
and cursor_after_mors (c : cursor) (id : ident)kkllkk

let safe_cursor_after (c : cursor) (n : int) l = 
  cursor_after c n (remove_cursor_mor)

(* the cursor is at the end *)
exception cursorEnd;;
(* the cursor is not found *)
exception cursorNotFound;;


let rec forward_mors (c : cursor) := function
  | cursor c2 :: t :: q when c = c2 -> t :: cursor c2 :: q
 | t :: q -> forward_mors 
				      
  match mor

let rec forward_into_mors (c : cursor) := function
  | cursor c2 :: t :: q when c = c2 -> 
  match mor


data functor = ident_functor of String;;
(* fonction remove curseur enlève toutes les occurences du curseur *)
(* fonction move curseur( le crée s'il n'existe pas, le détruit) *)

data objet = ident_ob of String | funct_ob : functor * objet ;; 

data nattrans = ident_nt of String;;
data cursor = ident_cur of String ;;

data mor = ident_mor of String | 
           cursor_mor of cursor |
          funct_mor  of functor * mors |
          nt_mor of nattrans * objet 
with mors = mor list;;

(*
data mor_x = ident_mor_x of String |
    funct_mor_x1 of functor_x * mor list |
    funct_mor_x2 of functor * mors_x |
    nt_mor_x1 of nattrans_x * objet |
    nt_mor_x2 of nattrans_x

with 
    mors_x = mor list * mor_x * mor list 

data mors = mor list;;
*)

(* let us assume that there is only one cursor of each type.
This function checks that there is only one cursor of each type in the stream *)
let rec check_integrity :


(* TODO: the same with backward: make the cursor progresses by 1 step *)
let rec forward_mor (c : cursor) = function
   cursor_mor c2 -> raise  (if c = c2 then cursorEnd else cursorNotFound)
 | funct_mor (F,l) -> 
     (try funct_mor ( forward_functor c F, l) 
     with
       cursorEnd -> functor_mor (F , cursor_mor c :: l)
      | cursorNotFound -> funct_mor (F , forward_mors c l))
 | nt_mor (nt, ob) -> nt_mor (forward_nt c nt, forward_ob c ob)
and
 forward_mors (c : cursor) (l : mors) :=
  match l with
 | t :: q -> 
    try forward_mor c t :: q with
       CursorEnd -> t :: cursor_mor c :: q
     | CursorNotFound -> t :: forward_mors c q
 | [] -> []
and 
forward_funct (c : cursor) = function
  ident_functor s => ident_functor s
and
forward_nt (c : cursor) = function
  ident_nt s => ident_nt s;;
and
forward_ob (c : cursor) = function
 ident_ob s => ident_ob s
| funct_ob (F , o)  => funct_ob (forward_funct c F, forward_ob c o);;
  
let funct_propagate_mors (f : mors -> 'a) = match


(* returns a function that need additional data: what is the other sides of j and n ?
 *)
(* F j ∘ c ∘ n k = n i ∘ c ∘ G j *)
(* faire les deux en même temps *)
type request = { id : int ; question: String; answer : dat option } ;;
type request = String list;;
type answer = 

let rec nat_trans_last_swap (foutre après ou avant) (c : cursor) (l : mors) =
  match l with
   funct_mor (F, j):: cursor_mor c2 :: nt_mor (n, x)  :: r when c = c2 -> 
     Some  question , (fun k G -> nt_mor n k :: cursor_mor c :: funct_mor (G , j)
  | t :: q -> nat_trans_last_swap t

let rec functor_comp (c : cursor) l =
  match 
   
  
    
let rec lo_ol (l : ('a option) list) : ('a list) option = 
  match l with 
    None :: q -> None
  | Some t :: q -> 
    ( match lo_ol l with
      None -> None
    | Some l2 -> t @ l2
   );;
*)

*)

*)
