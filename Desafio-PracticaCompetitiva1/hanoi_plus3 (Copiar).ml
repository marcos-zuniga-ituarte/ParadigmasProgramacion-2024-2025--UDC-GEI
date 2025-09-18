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

let rec get_possible_moves (a, b, c) l =
  match l with
  | [] -> []  (* Si no hay movimientos, devuelve una lista vacía *)
  | h :: t ->
      let nuevo_estado = make_move (a, b, c) h in
      if is_stable nuevo_estado && nuevo_estado<>(a,b,c)then
        h::get_possible_moves (a, b, c) t
      else
        get_possible_moves (a, b, c) t
;;

let add_final l elemento =
  l @ [elemento]
;;

let rec expandir_secuencias secuencias =
  match secuencias with
  | [] -> []  (* Caso base: si no hay secuencias, devuelve una lista vacía *)
  | h :: t ->
      (* Obtener el último estado y el historial de movimientos de la secuencia actual *)
      let (estado_actual, _) = List.hd (List.rev h) in

      (* Obtener movimientos posibles desde el estado actual *)
      let movimientos_posibles = get_possible_moves estado_actual movimientos in

      (* Filtrar movimientos que llevan a estados ya visitados en esta secuencia *)
      let movimientos_validos =
        List.filter (fun (estado_nuevo, mov) ->
          not (List.exists (fun (estado, _) -> estado = estado_nuevo) h)
        ) movimientos_posibles
      in

      (* Crear nuevas secuencias agregando los movimientos válidos a la secuencia actual *)
      let nuevas_secuencias = List.map (fun (estado_nuevo, mov) ->
        add_final h (estado_nuevo, mov)
      ) movimientos_validos
      in

      (* Llamada recursiva para expandir las secuencias restantes y añadir las nuevas secuencias *)
      nuevas_secuencias @ expandir_secuencias t
;;

let rec last = function
        [] -> raise (Failure "last")
      | h::[] -> h
      | h::t -> last t
;;

let hanoi n (a1, b1, c1) (a2, b2, c2) =
  let secuencias=  List.map (fun x -> [((make_move (a1,b1,c1) x),x)]) (get_possible_moves (a1,b1,c1) movimientos)in
  let rec aux1 l=
      let rec aux2= function
        [] -> []
      | h::t -> let (a,b)=last h in if a = (a2,b2,c2) then h else aux2 t
      in let lista=aux2 l in if lista=[] then aux1 (expandir_secuencias l) else lista
  in transform (aux1 secuencias)
;;

let hanoi n (a1, b1, c1) (a2, b2, c2) =
  let secuencias=  List.map (fun x -> [((make_move (a1,b1,c1) x),x)]) (get_possible_moves (a1,b1,c1) movimientos)in
  secuencias
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
  | (a,b)::t -> movement_interpreter b :: transform t
;;