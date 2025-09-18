let hd l=
    match l with
        [] -> raise (Failure "hd")
      | h::t -> h;;

let tl l=
    match l with
        [] -> raise (Failure "tl")
      | h::t -> t;;


let rec last = function
        [] -> raise (Failure "last")
      | h::[] -> h
      | _::t -> last t;;


let rec length =function
        [] -> 0
      | h::t -> 1 + (length t);;


let length' l=
    let rec aux l counter=
        match l with
            [] -> counter
        | h::t -> aux t (counter+1)
    in aux l 0;;


let rec compare_lengths l1 l2=
    match l1,l2 with
        [],[] -> 0
      | _,[] -> 1
      | [],_ -> (-1)
      | _::t1,_::t2 -> compare_lengths t1 t2;;


let rec append l1 l2=
    match l1 with
        [] -> l2
      | h1::t1 -> h1::append t1 l2;;


let rec rev_append l1 l2 = 
    match l1,l2 with
        [],_ -> l2
      | h1::t1,_ -> rev_append t1 (h1::l2);;


let rev l= rev_append l [];;


let concat l = 
    let rec aux l1 l2=
    match l2 with
        []-> l1
      | h2::t2 -> aux (append l1 h2) t2
    in aux [] l;;


let flatten = concat;;


let init n f=
    let rec aux n1 count f l=
        if count = n1 then l else (aux n1 (count+1) f (append l [(f count)]))
    in aux n 0 f [];;


let rec nth l n= 
    if n <0 then raise (Invalid_argument "nth posicion negativa") else
    match l with
            [] -> raise (Failure "nth lista demasiado corta")
          | h::t -> if n=0 then h else nth t (n -1);;


let map f l=
    let rec aux f1 l1 l2=
        match l1 with
            [] -> l2
          | h::t -> (f h)::aux f1 t l2
    in aux f l [];;


let rev_map f l=
    let rec aux f1 l1 l2=
        match l1 with
            [] -> l2
          | h::t -> aux f1 t ((f h)::l2)
    in aux f l [];;


let map2 f l1 l2=
    let rec aux f1 l11 l22 l33=
        match l11,l22 with
            [],[] -> l33
          | [],_ -> raise(Invalid_argument "las listas no tienen la misma longitud")
          | _,[] -> raise(Invalid_argument "las listas no tienen la misma longitud")
          | h1::t1,h2::t2 -> (f h1 h2)::aux f1 t1 t2  l33
    in aux f l1 l2 [];;


let rec combine l1 l2=
    match l1,l2 with
        [],[] -> []
      | [],_ -> raise(Invalid_argument "las listas no tienen la misma longitud")
      | _,[] -> raise(Invalid_argument "las listas no tienen la misma longitud")
      | h1::t1,h2::t2 -> (h1, h2)::combine t1 t2;;


let rec split =function
    [] -> ([],[])
  | (a,b)::t -> let (l1,l2)=split t in (a::l1,b::l2);;
    

let rec find f l=
    match l with
        [] -> raise (Not_found)
      | h::t -> if (f h) then h else find f t;;


let rec filter f l=
    match l with
        [] -> []
      | h::t -> if (f h) then h:: filter f t else filter f t;;


let filter' f l=
    let rec aux f1 l1 l2=
        match l1 with
            [] -> rev l2
          | h1::t1 -> if (f1 h1) then aux f1 t1 (h1::l2) else aux f1 t1 l2
    in aux f l [];;


let rec partition f l=
    match l with
        [] -> ([],[])
      | h::t -> let (l1,l2)=partition f t in if f h then (h::l1,l2) else (l1,h::l2);;


let partition' f l=
    let rec aux f1 l1 l2 l3=
        match l1 with
            [] -> (rev l2,rev l3)
          | h::t -> if f1 h then aux f1 t (h::l2) l3 else aux f1 t l2 (h::l3)
    in aux f l [] [];;


let rec for_all f l=
    match l with
        [] -> true
      | h::t -> if f h then for_all f t else false;;
    

let rec exists f l=
    match l with
        [] -> false
      | h::t -> if f h then true else exists f t;;
    

let rec mem element l=
    match l with
        [] -> false
      | h::t -> if element = h then true else mem element t;;


let rec fold_left f acc l=
    match l with
        [] -> acc
      | h::t -> fold_left f (f acc h) t;;


let rec fold_right f l acc=
    match l with
        [] -> acc
      | h::t -> f h (fold_right f t acc);;