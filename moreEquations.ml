
open Base;;
open Equation;;
let string_to_dcList (s : string) : datCursor list =
  (* (string_to_stuffData "(?c@(?n ?x) ; (?G ?f))" ).stList *)
  (string_to_stuffData s).stList
(* ********************
   Examples of equations
 ********************** *)
let eq_nat_trans : equation =
  { lhs = string_to_dcList "(?c@(?n ?x) ; (?G ?f))" ;
    rhs = string_to_dcList "((?F ?f) ; ?c@(?n ?y))" ;
    typ = Composition ;
    str = string_to_strToken "(natural transformation): ?n : ?F ⇒ ?G\n(morphism) ?f : ?x → ?y"
  }

(* ?x *)
let one_guy = {data = MVar (Name "x"); cursors = []} 

(* ?c@?x *)
let one_guy_one_cursor = Aideur.dcAddCursor (CurMVar (Name "c")) one_guy


let id_neutral : equation =
  { lhs = [ one_guy_one_cursor ];
    rhs = [];
    typ = Composition ;
    str = string_to_strToken "neutrality of identity (for composition)"}

(* could be used to move the cursor, but then it would nly work with compositions *)
let eq_move_cursor : equation =
  { lhs = string_to_dcList "(?c@?x ; ?y)" ;
    rhs = string_to_dcList "(?x ; ?c@?y)" ;
    typ = Composition ;
    str = string_to_strToken "Move cursor"}

(* remove/add cursor (what about look for multiple matches) *)
let eq_add_cursor : equation =
  {
    (* lhs = string_to_stuffData "?c@?x" ; *)
    lhs = [ one_guy_one_cursor ];
    (* rhs = string_to_stuffData "?x" *)
    rhs = [ one_guy ];
    typ = Composition ;
    str = string_to_strToken "Remove/add cursor"} ;;

(* Monoidal product *)

let rho_lambda_eq =
  { lhs = string_to_dcList "?c@(id I)" ;
    rhs = string_to_dcList "((rho I) ; ?c@(lambda I))" ;
    typ = Composition ;
    str = string_to_strToken "rho lambda"}

let triangle_eq =
  { lhs = string_to_dcList "(id (?a (x) ?b))" ;
    rhs = string_to_dcList "(((rho ?a) (x) ?b) ; (alpha ?a I ?b) ; (?a (x) (lambda ?b)))" ;
    typ = Composition ;
    str = string_to_strToken "triangle"}

let alpha_lambda_eq =
  {
    lhs = string_to_dcList "?c@(((lambda ?a) (x) ?b))" ;
    rhs = string_to_dcList "((alpha I ?a ?b) ; ?c@(lambda (?a (x) ?b)))" ;
    typ = Composition ;
    str = string_to_strToken "alpha lambda"}
(* let rho_alpha_eq =
 *   {
 *     lhs = string_to_stuffData "?c@(a ; ?b)" ;
 *     rhs = string_to_stuffData "((alpha I ?a ?b) ; ?c@(lambda (?a (x) ?b)))" ;
 *     str = string_to_strToken "alpha lambda"} *)
