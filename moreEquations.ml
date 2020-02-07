
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
    rhs = string_to_dcList "((ρ I) ; ?c@(λ I))" ;
    typ = Composition ;
    str = string_to_strToken "ρ λ"}

let triangle_eq =
  { lhs = string_to_dcList "(id (?a ⊗ ?b))" ;
    rhs = string_to_dcList "(((ρ ?a) ⊗ ?b) ; (α ?a I ?b) ; (?a ⊗ (λ ?b)))" ;
    typ = Composition ;
    str = string_to_strToken "triangle"}

let alpha_lambda_eq =
  {
    lhs = string_to_dcList "?c@(((λ ?a) ⊗ ?b))" ;
    rhs = string_to_dcList "((α I ?a ?b) ; ?c@(λ (?a ⊗ ?b)))" ;
    typ = Composition ;
    str = string_to_strToken "α λ"}
(* let ρ_α_eq =
 *   {
 *     lhs = string_to_stuffData "?c@(a ; ?b)" ;
 *     rhs = string_to_stuffData "((α I ?a ?b) ; ?c@(λ (?a ⊗ ?b)))" ;
 *     str = string_to_strToken "α λ"} *)
