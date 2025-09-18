let rec append l1 l2=
    if l1=[] then l2
    else List.hd l1 :: append (List.tl l1) l2;;


let rec append = function
    [] -> function (l -> l)
  | h::t -> f(unction l -> h :: append t l);;


let rec append l1 l2=
    match l1 with
        [] -> l2
      | h::t -> h :: append t l2;;


let rec compare_lengths l1 l2=
    if l1=[] && l2 <> [] then (-1)
    else if l1<>[] && l2 = [] then 1
    else if l1=[] && l2=[] then 0
    else compare_lengths (List.tl l1) (List.tl l2);;


let rec compare_lengths l1 l2=
    match (l1,l2) with
        [], [] -> 0
      | [], _ -> (-1)
      | _, [] -> 1
      | _::t1, _::t2 -> compare_lengths t1 t2;;



let rec lmax =function
    [] -> raise (Failure "lista vacia")
  | h::[] -> h
  | h::t -> let max = (lmax t) in if h > max then h else max;;


let rec lmax l=
    if l=[] then raise (Failure "lista vacia")
    else if List.tl l=[] then List.hd l
    else let max = (lmax (List.tl l)) in if List.hd l > max then List.hd l
        else max;;




