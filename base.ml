type cursor = Nb of int 

type ident = Name of string ;;
type stuffType = Composition | Other ;;
type stuffData = { stTyp : stuffType ; stList : datCursor list}
  and dat = Ident of ident | Stuff of stuffData 
  and datCursor = { data : dat ; cursors : cursor list}


let emptyStuffData = { stTyp = Composition ; stList = []}
let newDatCursor (d : dat) : datCursor = { data = d ; cursors = []};;
let mkIdentDatCursor (s : string) : datCursor = newDatCursor (Ident (Name s)) ;;
let mkListDatCursor (d : datCursor list)(st : stuffType ) =
   newDatCursor (Stuff {stTyp = st ; stList = d}) ;;
