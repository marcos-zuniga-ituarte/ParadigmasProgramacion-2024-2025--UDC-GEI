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
      if is_stable nuevo_estado && nuevo_estado <> (a, b, c) then
        (nuevo_estado, h) :: get_possible_moves (a, b, c) t
      else
        get_possible_moves (a, b, c) t
;;


let add_final l elemento =
  l @ [elemento]
;;

(* Expande las secuencias sin incluir estados ya visitados globalmente *)
(* Función auxiliar para filtrar y generar nuevas secuencias *)
(* Expande las secuencias sin incluir estados ya visitados globalmente *)
(* Función auxiliar para filtrar y generar nuevas secuencias *)
(* Función auxiliar para verificar si un movimiento revierte el anterior *)
let revierte_movimiento (origen1, destino1) (origen2, destino2) =
  origen1 = destino2 && destino1 = origen2
;;

(* Expande las secuencias sin incluir estados ya visitados globalmente y sin movimientos revertidos *)
let rec expandir_secuencias secuencias visitados =
  match secuencias with
  | [] -> []  (* Caso base: si no hay secuencias, devuelve una lista vacía *)
  | h :: t ->
      (* Obtener el último estado y movimiento de la secuencia actual *)
      let (estado_actual, ultimo_mov) = List.hd (List.rev h) in
      let (a, b, c) = estado_actual in

      (* Obtener movimientos posibles desde el estado actual *)
      let movimientos_posibles = get_possible_moves (a, b, c) movimientos in

      (* Filtrar movimientos que llevan a estados ya visitados o revierten el movimiento anterior *)
      let movimientos_validos =
        List.filter (fun (estado_nuevo, mov) ->
          let (a_nuevo, b_nuevo, c_nuevo) = estado_nuevo in
          not (List.exists (fun (va, vb, vc) -> va = a_nuevo && vb = b_nuevo && vc = c_nuevo) visitados) &&
          not (revierte_movimiento ultimo_mov mov)
        ) movimientos_posibles
      in

      (* Generar nuevas secuencias o eliminar secuencia si no hay movimientos válidos *)
      let nuevas_secuencias = 
        if movimientos_validos = [] then []  (* Elimina secuencia si todos los movimientos son inválidos *)
        else List.map (fun (estado_nuevo, mov) -> h @ [(estado_nuevo, mov)]) movimientos_validos
      in

      (* Actualizar la lista de visitados con los nuevos estados alcanzados *)
      (* Actualizar la lista de visitados con los nuevos estados alcanzados *)
      let nuevos_visitados = 
        List.fold_left (fun acc secuencia ->
          let (estado_final, _) = List.hd (List.rev secuencia) in
          if List.exists ((=) estado_final) acc then acc
          else estado_final :: acc
        ) visitados nuevas_secuencias
      in


      (* Llamada recursiva para expandir el resto de las secuencias con la lista de visitados actualizada *)
      nuevas_secuencias @ expandir_secuencias t nuevos_visitados
;;



let rec last = function
        [] -> raise (Failure "last")
      | h::[] -> h
      | h::t -> last t
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

let filtrar_mejores_opciones secuencias estado_objetivo =
  let rec aux secuencias mejores_opciones =
    match secuencias with
    | [] -> mejores_opciones  (* Devuelve las mejores opciones encontradas *)
    | h :: t ->
        let (estado_final, _) = List.hd (List.rev h) in  (* Obtener el estado final de la secuencia actual *)
        let longitud_h = List.length h in
        let longitud_mejores = if mejores_opciones = [] then max_int else List.length (List.hd mejores_opciones) in
        if estado_final = estado_objetivo then
          if longitud_h < longitud_mejores then
            (* Si esta secuencia es mejor, reemplaza las mejores opciones con esta *)
            aux t [h]
          else if longitud_h = longitud_mejores then
            (* Si es igual de buena, añadir a las mejores opciones *)
            aux t (h :: mejores_opciones)
          else
            (* Si es peor, ignórala y sigue con las demás *)
            aux t mejores_opciones
        else
          (* Si no alcanza el estado objetivo, ignórala y sigue con las demás *)
          aux t mejores_opciones
  in
  aux secuencias []
;;





(* Función para verificar si un estado ya está en la lista *)
let rec existe estado lista =
  match lista with
  | [] -> false
  | h :: t -> if h = estado then true else existe estado t
;;


(* Función para agregar un estado si no está en la lista *)
let agregar_si_no_existe estado lista_acumulada =
  if existe estado lista_acumulada then lista_acumulada
  else estado :: lista_acumulada
;;

(* Función principal para obtener todos los estados únicos *)
let extraer_estados_unicos lista_de_listas =
  (* Procesar cada sublista y acumular estados únicos *)
  let rec procesar_sublistas sublistas acumulado =
    match sublistas with
    | [] -> acumulado  (* Devuelve la lista acumulada de estados únicos *)
    | sublista :: resto ->
        (* Agregar cada estado en la sublista a la lista acumulada, sin duplicados *)
        let acumulado_actualizado = 
          List.fold_left (fun acc (estado, _) -> agregar_si_no_existe estado acc) acumulado sublista
        in
        procesar_sublistas resto acumulado_actualizado
  in

  procesar_sublistas lista_de_listas []
;;




(* Función principal para filtrar secuencias *)
let filtrar_pasadas secuencias =
  let visited = extraer_estados_unicos secuencias in
  
  let rec aux1 l acumulado =
    match l with
    | [] -> List.rev acumulado  (* Devolver la lista acumulada en orden original *)
    | h :: t ->
        let (ultimo_estado, _) = last h in
        if not (existe ultimo_estado visited) then
          (* Si el último estado no está en visited, añadir la secuencia *)
          aux1 t (h :: acumulado)
        else
          (* Si el último estado está en visited, ignorar esta secuencia *)
          aux1 t acumulado
  in
  aux1 secuencias []
;;


(* Función para actualizar la lista de visitados con los últimos estados de cada secuencia *)
let actualizar_visitados visitados secuencias =
  List.fold_left (fun acc secuencia ->
    let (estado_final, _) = List.hd (List.rev secuencia) in  (* Obtener el último estado de la secuencia *)
    if List.exists ((=) estado_final) acc then acc          (* Si ya está en visitados, no lo añade *)
    else estado_final :: acc                                (* Si no está, lo añade *)
  ) visitados secuencias
;;


let hanoi n (a1, b1, c1) (a2, b2, c2) =
  if not(is_stable (a1,b1,c1)) || not(is_stable (a2,b2,c2)) then raise (Invalid_argument "Estados no estables") 
  else
    let secuencias=  List.map (fun x -> [x]) (get_possible_moves (a1,b1,c1) movimientos) in
    let rec aux1 l visitados2=
        let rec aux2= function
          [] -> []
        | h::t -> let (a,b)=last h in if a = (a2,b2,c2) then h else aux2 t
        in let lista=aux2 l in if lista=[] then aux1 ((expandir_secuencias l visitados2)) (actualizar_visitados visitados2 secuencias) else lista
    in transform (aux1 secuencias (extraer_estados_unicos secuencias))
;;



let hanoi n (a1, b1, c1) (a2, b2, c2) =
  if not(is_stable (a1,b1,c1)) || not(is_stable (a2,b2,c2)) then raise (Invalid_argument "Estados no estables") 
  else
    let secuencias=  List.map (fun x -> [x]) (get_possible_moves (a1,b1,c1) movimientos) in
    let rec aux1 l=
        let rec aux2= function
          [] -> []
        | h::t -> let (a,b)=last h in if a = (a2,b2,c2) then h else aux2 t
        in let lista=aux2 l in if lista=[] then aux1 (let expansion=filtrar_pasadas (expandir_secuencias l) in let filtered= filtrar_mejores_opciones (expansion) (a2, b2, c2)in if filtered=[] then   expansion else   filtered) else lista
    in transform (aux1 secuencias)
;;

hanoi 1 ([1], [], []) ([], [], [1]);;
hanoi 2 ([1; 2], [], []) ([], [], [1; 2]);;
hanoi 3 ([1; 2; 3], [], []) ([], [], [1; 2; 3]);;
hanoi 4 ([1; 2; 3; 4], [], []) ([], [], [1; 2; 3; 4]);;
hanoi 5 ([1; 2; 3; 4; 5], [], []) ([], [], [1; 2; 3; 4; 5]);;
hanoi 6 ([1; 2; 3; 4; 5; 6], [], []) ([], [], [1; 2; 3; 4; 5; 6]);;
hanoi 7 ([1; 2; 3; 4; 5; 6; 7], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7]);;
hanoi 8 ([1; 2; 3; 4; 5; 6; 7; 8], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8]);;
hanoi 9 ([1; 2; 3; 4; 5; 6; 7; 8; 9], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9]);;
hanoi 10 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);;
hanoi 11 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11]);;
hanoi 12 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12]);;
hanoi 13 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13]);;
hanoi 14 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14]);;
hanoi 15 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15]);;
hanoi 16 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16]);;
hanoi 17 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17]);;
hanoi 18 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18]);;
hanoi 19 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19]);;
hanoi 20 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20]);;
hanoi 21 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21]);;
hanoi 22 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22]);;
hanoi 23 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23]);;
hanoi 24 ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24], [], []) ([], [], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24]);;
