type move = LtoC | LtoR | CtoL | CtoR | RtoL | RtoC;;
let movimientos=[LtoC;LtoR;CtoL;CtoR;RtoL;RtoC];;

let rec correct_list count= function
    [] -> true
  | h::t -> if count<>h then false else correct_list (count+1) (t)
;;

let existe elem lista =
  List.exists (fun x -> x = elem) lista
;;

let rec update_state_list l1 l2 =
  match l1 with
      []->l2
    | h::t -> if existe h l2 then update_state_list t l2 else update_state_list t (h::l2)
;;

let is_stable (a,b,c)=
  let rec check_order = function
    | [] -> true
    | [_] -> true
    | h1 :: h2 :: t -> h1 < h2 && check_order (h2 :: t)
  in
  check_order a && 
  check_order b && 
  check_order c && 
  correct_list 1 (List.sort compare (a@b@c))
;;

let make_move_aux (a, b, c) movimiento =
  let mover_de_torre torre destino =
    match torre with
    | [] -> (torre, destino)
    | hd :: tl -> (tl, hd :: destino)
  in

  match movimiento with
  | LtoC -> 
      let (na, nb) = mover_de_torre a b in (na, nb, c)
  | LtoR ->
      let (na, nc) = mover_de_torre a c in (na, b, nc)
  | CtoL ->
      let (nb, na) = mover_de_torre b a in (na, nb, c)
  | CtoR ->
      let (nb, nc) = mover_de_torre b c in (a, nb, nc)
  | RtoL ->
      let (nc, na) = mover_de_torre c a in (na, b, nc)
  | RtoC ->
      let (nc, nb) = mover_de_torre c b in (a, nb, nc)
;;

let move estado movimiento= 
    if is_stable estado then let nuevo_estado=make_move_aux estado movimiento in
                              if is_stable nuevo_estado
                              then nuevo_estado else raise (Invalid_argument "move")
    else raise (Invalid_argument "move")
;;

let get_possible_moves (a, b, c)=
  let rec aux l =
    match l with
    | [] -> []
    | h :: t ->
        let nuevo_estado = move (a, b, c) h in
        if is_stable nuevo_estado (*&& nuevo_estado <> (a, b, c)*) then
          (nuevo_estado) :: aux t
        else
          aux t
  in
  (aux movimientos)
;;

let get_states (a,b,c) n=[(a@[n],b,c);(a,b@[n],c);(a,b,c@[n])];;

let rec iterate_possible_states n= function
    [] -> []
  | h::t -> get_states h n @ iterate_possible_states n t
;;

let all_states n =
    let rec aux cont l=
        if cont=n then l
        else aux (cont + 1) (iterate_possible_states (cont+1) l)
    in aux 0 [([],[],[])]
;;

let rec move_sequence estado= function
    [] -> estado
  | h::t -> move_sequence (move estado h) t
;;