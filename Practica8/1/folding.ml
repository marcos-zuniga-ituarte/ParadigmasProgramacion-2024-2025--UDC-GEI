let i_prod = function
    [] -> 1
  | h::t -> List.fold_left (fun n h -> n*h) h t;;


let f_prod = function
    [] -> 1.
  | h::t -> List.fold_left (fun n h -> n *. h) h t;;


let lmin = function
    [] -> raise (Failure "lmin")
  | h::t -> List.fold_left min h t;;


let min_max = function
    [] -> raise (Failure "min_max")
  | h::t -> List.fold_left (fun (a,b) h -> (min a h, max b h)) (h,h) t;;


let rev l = List.fold_left (fun l1 h -> h::l1) [] l;;


let rev_append l1 l2 = List.fold_left (fun l1 h -> h::l1) l2 l1;;


let rev_map f l = List.fold_left ((fun f -> fun l1 h -> (f h)::l1)(f)) [] l;;


let concat l = List.fold_left (fun s h -> s ^ h) "" l;;
