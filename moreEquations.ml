
open Base;;
open Equation;;
open Stringstuff;;
(* ********************
   Examples of equations
 ********************** *)
let eq_nat_trans : equation =
  { lhs = string_to_stuffData "(?c@(?n ?x) ; (?G ?f))" ;
    rhs = string_to_stuffData "((?F ?f) ; ?c@(?n ?y))" ;
    str = string_to_strToken "(natural transformation): ?n : ?F ⇒ ?G\n(morphism) ?f : ?x → ?y"
  }

(* could be used to move the cursor, but then it would nly work with compositions *)
let eq_move_cursor : equation =
  { lhs = string_to_stuffData "(?c@?x ; ?y)" ;
    rhs = string_to_stuffData "(?x ; ?c@?y)" ;
    str = string_to_strToken "Move cursor"}

(* remove/add cursor (what about look for multiple matches) *)
let eq_move_cursor : equation =
  {
    (* lhs = string_to_stuffData "?c@?x" ; *)
    lhs = {stTyp = Other ; stList = [ {data = MVar (Name "x"); cursors = [ CurMVar (Name "c")]} ]};
    (* rhs = string_to_stuffData "?x" *)
    rhs = {stTyp = Other ; stList = [ {data = MVar (Name "x"); cursors = []} ]};
    str = string_to_strToken "Remove/add cursor"}
