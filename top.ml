(* Tip: to remove an active cursor, go up until it disappears *)
(* Equation.coucou ();; *)
Toplib.startLoop() ;;
(*

# # # # # # # # mainCur := test ;;
- : unit = ()
# mainLoop (cursorsForStrl "n") ;;
((F j) ; ({1}@n y))
- : unit = ()
# let infer = natTransToLeftInferl (Nb 1) (! mainCur) ;;
Characters 40-51:
  let infer = natTransToLeftInferl (Nb 1) (! mainCur) ;;
                                          ^^^^^^^^^^^
Error: This expression has type Base.stuffData
       but an expression was expected of type Base.datCursor list
# let infer = natTransToLeftInferl (Nb 1) (! mainCur).stLis

                
                t ;
              
              
              ;
              ;;;;;
Characters 140-141:
                ;
                ^
Error: Syntax error
# let infer = natTransToLeftInferl (Nb 1)(! mainCur).stLis;;
Characters 51-56:
  let infer = natTransToLeftInferl (Nb 1)(! mainCur).stLis;;
                                                     ^^^^^
Error: This expression has type Base.stuffData
       The field stLis does not belong to type Base.stuffData
Hint: Did you mean stList?
# let infer = natTransToLeftInferl (Nb 1)(! mainCur).stList;;
val infer : Aideur.ntInfer =
  {nat = {data = Ident (Name "n"); cursors = [Nb 1]};
   funct = {data = Ident (Name "F"); cursors = []};
   mor = {data = Ident (Name "j"); cursors = []};
   obj = {data = Ident (Name "y"); cursors = []}}
# ntInferToLeftToString infer;;
- : string = "n : F => ?\nj : ? -> y"
# mainLoop (natTransToLeftl (Nb 1){funct = G ; obj = x}) ;;
Characters 41-42:
  mainLoop (natTransToLeftl (Nb 1){funct = G ; obj = x}) ;;
                                           ^
Error: Unbound constructor G
# mainLoop (natTransToLeftl (Nb 1){funct = mkIdentDatStruct "G" ; obj = mkIdentDatStruct "x"}) ;;
Characters 41-57:
  mainLoop (natTransToLeftl (Nb 1){funct = mkIdentDatStruct "G" ; obj = mkIdentDatStruct "x"}) ;;
                                           ^^^^^^^^^^^^^^^^
Error: Unbound value mkIdentDatStruct
# open Base;;
# mainLoop (natTransToLeftl (Nb 1){funct = mkIdentDatStruct "G" ; obj = mkIdentDatStruct "x"}) ;;
Characters 41-57:
  mainLoop (natTransToLeftl (Nb 1){funct = mkIdentDatStruct "G" ; obj = mkIdentDatStruct "x"}) ;;
                                           ^^^^^^^^^^^^^^^^
Error: Unbound value mkIdentDatStruct
# mainLoop (natTransToLeftl (Nb 1){funct = mkIdentDatCursor "G" ; obj = mkIdentDatStruct "x"}) ;;
Characters 70-86:
  mainLoop (natTransToLeftl (Nb 1){funct = mkIdentDatCursor "G" ; obj = mkIdentDatStruct "x"}) ;;
                                                                        ^^^^^^^^^^^^^^^^
Error: Unbound value mkIdentDatStruct
# mainLoop (natTransToLeftl (Nb 1){funct = mkIdentDatCursor "G" ; obj = mkIdentDatCursor "x"}) ;;
(({1}@n x) ; (G j))
- : unit = ()
# 

*)
