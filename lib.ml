
let listRemove (cc : 'a)(l : 'a list) = List.filter (fun b -> b <> cc) l

let rec listNoDup (l : 'a list) : bool = 
  match l with
  | [] -> true
  | t :: q -> not (List.mem t q) && listNoDup q

(* list duplicates *)
let rec listListDup (l : 'a list) : 'a list = 
  match l with
  | [] -> []
  | t :: q -> if (List.mem t q) then t :: listListDup q else listListDup q

let maxList (smallest : 'a)(l : 'a list) : 'a =
  List.fold_left max smallest l


let string_of_char = String.make 1 


