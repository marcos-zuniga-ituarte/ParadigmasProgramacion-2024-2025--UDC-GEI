type move = LtoC | LtoR | CtoL | CtoR | RtoL | RtoC;;

exception Invalid_argument;;

(* Verifica si los discos en un poste están en orden creciente *)
let is_stable_state (post: int list) : bool =
  let rec check_order = function
    | [] | [_] -> true
    | x :: y :: rest -> x < y && check_order (y :: rest)
  in
  check_order post
;;

(* Verifica que el estado es estable y que todos los discos están distribuidos correctamente *)
let validate_state (left, center, right) n =
  if List.length left + List.length center + List.length right <> n ||
     not (is_stable_state left && is_stable_state center && is_stable_state right)
  then raise Invalid_argument
;;

(* Mueve el disco de src a dst solo si es válido *)
let move_disk (src: int list) (dst: int list) =
  match src with
  | [] -> raise Invalid_argument (* No hay disco para mover *)
  | hd :: tl ->
    match dst with
    | top :: _ when top < hd -> raise Invalid_argument (* No se puede colocar un disco grande sobre uno más pequeño *)
    | _ -> (tl, hd :: dst)
;;

(* Realiza un movimiento en el estado dado, respetando las reglas de los discos *)
let perform_move (state: int list * int list * int list) (m: move) =
  let (left, center, right) = state in
  match m with
  | LtoC -> let (new_left, new_center) = move_disk left center in (new_left, new_center, right)
  | LtoR -> let (new_left, new_right) = move_disk left right in (new_left, center, new_right)
  | CtoL -> let (new_center, new_left) = move_disk center left in (new_left, new_center, right)
  | CtoR -> let (new_center, new_right) = move_disk center right in (left, new_center, new_right)
  | RtoL -> let (new_right, new_left) = move_disk right left in (new_left, center, new_right)
  | RtoC -> let (new_right, new_center) = move_disk right center in (left, new_center, new_right)
;;

let all_possible_moves = [LtoC; LtoR; CtoL; CtoR; RtoL; RtoC];;

(* Búsqueda en anchura para encontrar la secuencia de movimientos mínima *)
let rec bfs queue visited target =
  match queue with
  | [] -> raise Not_found
  | (state, moves) :: rest ->
    if state = target then List.rev moves
    else if List.mem state visited then bfs rest visited target
    else
      let new_states = List.filter_map (fun m ->
        try Some (perform_move state m, m :: moves) with Invalid_argument -> None
      ) all_possible_moves in
      bfs (rest @ new_states) (state :: visited) target
;;

(* Función principal hanoi *)
let hanoi n (initial_left, initial_center, initial_right) (final_left, final_center, final_right) =
  validate_state (initial_left, initial_center, initial_right) n;
  validate_state (final_left, final_center, final_right) n;
  let initial_state = (initial_left, initial_center, initial_right) in
  let final_state = (final_left, final_center, final_right) in
  bfs [(initial_state, [])] [] final_state
;;





let secuencias = [
  [(([1; 2], [], [3; 4]), (1, 3)); (([4], [1], [2; 3]), (1, 3))];
  [(([1; 2], [], [3; 4]), (1, 2)); (([], [4], [1; 2; 3]), (2, 3))]
];;


(* Crear una lista de sublistas para probar *)
let lista_de_prueba = [
  [((1, 2, 3), LtoC); ((2, 3, 4), CtoL); ((3, 4, 5), RtoL)];
  [((2, 3, 4), LtoC); ((4, 5, 6), LtoR)];
  [((3, 4, 5), CtoL); ((1, 2, 7), RtoL)];
];;