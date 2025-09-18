let from0to n= if n<0 then raise (Failure "from0to") else List.init (n+1) (fun e -> e);;


let to0from n= if n<0 then raise (Failure "to0from") else  List.init (n+1) ((fun n1 -> fun n2 -> n-n2)(n));;


let pair x l = List.map (fun y -> (x, y)) l;;


let remove_first x l = List.rev ((fun (lista,_) -> lista)(List.fold_left ((fun x1 -> fun (l,b) h -> if h = x1 && not b then (l,true) else (h::l,b))(x)) ([],false) l));;


let remove_all x l = List.rev (List.fold_left ((fun x1 -> fun l h -> if h = x1 then l else h::l)(x)) [] l);;


let ldif l1 l2 = List.rev (List.fold_left ((fun l22 -> fun l h -> if List.mem h l22 then l else h::l)(l2)) [] l1);;