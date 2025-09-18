type move = LtoC | LtoR | CtoL | CtoR | RtoL | RtoC;;
let movimientos = [(1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2)];;

let is_stable (a,b,c)=
  let rec check_order = function
    | [] -> true
    | [_] -> true
    | h1 :: h2 :: t -> h1 < h2 && check_order (h2 :: t)
  in
  check_order a && check_order b &&check_order c
;;


let make_move (a, b, c) (ori, des) =
  let mover_de_torre torre destino =
    match torre with
    | [] -> (torre, destino)  (* Si la torre está vacía, no hace nada *)
    | hd :: tl -> (tl, hd :: destino)  (* Mueve el disco superior *)
  in

  (* Ejecutar el movimiento según el índice de origen y destino *)
  match (ori, des) with
  | (1, 2) -> 
      let (na, nb) = mover_de_torre a b in (na, nb, c)
  | (1, 3) ->
      let (na, nc) = mover_de_torre a c in (na, b, nc)
  | (2, 1) ->
      let (nb, na) = mover_de_torre b a in (na, nb, c)
  | (2, 3) ->
      let (nb, nc) = mover_de_torre b c in (a, nb, nc)
  | (3, 1) ->
      let (nc, na) = mover_de_torre c a in (na, b, nc)
  | (3, 2) ->
      let (nc, nb) = mover_de_torre c b in (a, nb, nc)
  | _ -> (a, b, c)  (* Si los índices no son válidos, devuelve el estado sin cambios *)
;;

let hash_state (torre1, torre2, torre3) =
  (torre1, torre2, torre3)
;;

let get_possible_moves (a, b, c)=
  let rec aux l =
    match l with
    | [] -> []  (* Si no hay movimientos, devuelve una lista vacía *)
    | h :: t ->
        let nuevo_estado = make_move (a, b, c) h in
        if is_stable nuevo_estado && nuevo_estado <> (a, b, c) then
          (h) :: aux t
        else
          aux t
  in
  (aux movimientos)  (* Llama a la auxiliar con la lista de movimientos predeterminada *)
;;

let existe elem lista =
  List.exists (fun x -> x = elem) lista
;;

(* Función que verifica si se puede pasar de un estado al otro con un solo movimiento *)
let es_un_movimiento estado1 estado2 =
  (* Obtiene los movimientos posibles desde el estado inicial *)
  let posibles_movimientos = get_possible_moves estado1 in
  (* Verifica si alguno de los estados generados coincide con el estado objetivo *)
  List.exists (fun mov -> make_move estado1 mov = estado2) posibles_movimientos
;;

let rec get_states estado=function
    [] ->[]
  | h::t -> make_move estado h :: get_states estado t
;;

let rec combine sec_estados=function
    [] -> [] 
  | h::t -> if not (existe h (List.tl (List.rev sec_estados))) then sec_estados @ [h] else combine sec_estados t
;;

let rec last lista =
  match lista with
  | [] -> failwith "La lista está vacía"
  | [x] -> x  (* Caso base: si solo hay un elemento, es el último *)
  | _ :: t -> last t  (* Recursión en el resto de la lista *)
;;

let iterar sec_estados=
    let actual=last sec_estados in
    let next_states=get_states (actual) (get_possible_moves actual) in
    combine sec_estados next_states
;; 



let movement_interpreter= function
                (1,2)->LtoC
              | (1,3)->LtoR
              | (2,1)->CtoL
              | (2,3)->CtoR
              | (3,1)->RtoL
              | (3,2)->RtoC
;;

let rec transform=function
    [] -> []
  | (a,b)::t -> movement_interpreter (a,b) :: transform t
;;




let buscar_solucion estado l=
    (last l = estado)
;;

let rec cut=
    let rec aux1 estado=function
        []->[]
      | h::t-> if es_un_movimiento h estado then [h] else h:: aux1 estado t
    in function
    [] -> []
  | h::t -> h::cut (List. rev (aux1 h (List.rev t)))
;;

let hanoi n (a1, b1, c1) (a2, b2, c2) =
  if not(is_stable (a1,b1,c1)) || not(is_stable (a2,b2,c2)) 
  then raise (Invalid_argument "Estados no estables") 
  else
    let estado_inicial = (a1, b1, c1) in
    let estado_final = (a2, b2, c2) in
    let secuencias = [estado_inicial] in
    let rec aux1 secuencias1=
        let secuencias2=iterar secuencias1 in
        let encontrado=buscar_solucion estado_final secuencias1 in
        if encontrado then secuencias1
        else
        aux1 secuencias2
    in cut (aux1 secuencias)
;;
