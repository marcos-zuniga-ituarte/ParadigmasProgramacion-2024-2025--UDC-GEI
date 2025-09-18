let concat l =
  let rec aux acc = function
    [] -> acc
  | h::t -> aux (List.append acc h) t
  in (aux [] l)
;;

let front l=
    let rec aux acc=function
        [] -> raise (Failure "front")
      | h::[] -> acc
      | h::t -> aux (h::acc) t
    in List.rev (aux [] l)
;;


let rec compress = function
    h1::h2::t -> if h1 = h2 then compress (h2::t)
                else h1 :: compress (h2::t)
  | l -> l

let compress l= 
    let rec aux acc=function
        h1::h2::t -> if h1 = h2 then aux acc (h2::t)
                    else aux (h1::acc) (h2::t)
      | l -> l@acc
    in List.rev(aux [] l)
;;

let rec ofo = function
    [] -> []
  | h::t -> h :: List.filter ((<>) h) (ofo t)
;;

let ofo l=
    let rec aux acc =function
        [] -> acc
      | h::t -> aux (h::acc) (List.filter ((<>) h) t)
    in List.rev (aux [] l)
;;

let rec fold_right' f l acc=
    match l with
        [] -> acc
      | h::t -> f h (fold_right f t acc);;

let existe elem lista =
  List.exists (fun x -> x = elem) lista
;;

let rec iterar l1 n l3= 
    match l1 with
        [] -> List.rev l3
      | h::t -> iterar t n ([n::h]@l3)
;;

let sublists l=
    if l=[] then [] else
    let rec aux acc revl = let iterado=iterar acc (List.hd revl) [] in if existe l iterado then acc@iterado else aux (acc@iterado) (List.tl revl)
    in aux [[]] (List.rev l)
;;
