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
  let calcular_hash torre =
    List.fold_left (fun acc (disco, pos) -> acc + disco * pos) 0 (List.mapi (fun i disco -> (disco, List.length torre - i)) torre)
  in
  calcular_hash torre1 + calcular_hash torre2 + calcular_hash torre3
;;


let existe elem lista =
  List.exists (fun x -> x = elem) lista
;;

(* Supongamos que ya tenemos definida la función hash_state que genera un hash único para cada estado *)

(* Filtra movimientos que generan estados únicos en función del hash del estado resultante *)
let filtrar_movimientos_unicos estado_inicial movimientos =
  let rec aux movimientos acumulado visitados =
    match movimientos with
    | [] -> acumulado  (* Devuelve la lista de movimientos únicos *)
    | mov :: resto ->
        let nuevo_estado = make_move estado_inicial mov in
        let hash_nuevo_estado = hash_state nuevo_estado in
        (* Si el hash del nuevo estado ya está en visitados, ignora el movimiento *)
        if List.exists ((=) hash_nuevo_estado) visitados then
          aux resto acumulado visitados
        else
          aux resto (mov :: acumulado) (hash_nuevo_estado :: visitados)  (* Añade movimiento y actualiza visitados *)
  in
  List.rev (aux movimientos [] [])  (* Revierte la lista acumulada para preservar el orden original *)
;;


let get_possible_moves (a, b, c) visitados=
  let rec aux l =
    match l with
    | [] -> []  (* Si no hay movimientos, devuelve una lista vacía *)
    | h :: t ->
        let nuevo_estado = make_move (a, b, c) h in
        if is_stable nuevo_estado && nuevo_estado <> (a, b, c) && not (existe (hash_state nuevo_estado) visitados) then
          (h) :: aux t
        else
          aux t
  in
  (aux movimientos)  (* Llama a la auxiliar con la lista de movimientos predeterminada *)
;;

(* Asume que hash_state ya está definida para obtener un hash único de cada estado *)

let actualizar_visitados (a, b, c) posibles visitados =
  List.fold_left (fun acc mov ->
    let nuevo_estado = make_move (a, b, c) mov in
    let hash_nuevo_estado = hash_state nuevo_estado in
    if List.exists ((=) hash_nuevo_estado) acc then acc
    else hash_nuevo_estado :: acc
  ) visitados posibles
;;


let rec filtrar posibles visitados =
  match posibles with
      [] -> []
    | (a,b)::t -> if existe (hash_state b) visitados then filtrar t visitados else (a,b)::(filtrar t visitados)
;;


let add_final l elemento =
  l @ [elemento]
;;

let rec actualizar_secuencia movimientos (a,b)=
    match movimientos with
        []->[]
      | h::t -> (a @ [h],make_move b h)::actualizar_secuencia t (a,b)
;;

let iterar secuencias visitados=
    let rec aux sec=
        match sec with
            [] ->[]
          | (a,b)::t -> let mposibles=get_possible_moves b visitados in
                        let updated_secuencia=actualizar_secuencia mposibles (a,b) in
                        updated_secuencia @ aux t
    in aux secuencias
;;

let iterar secuencias visitados =
  List.fold_left (fun (nuevas_secuencias, nuevos_visitados) (movimientos, ultimo_estado) ->
    let posibles_movimientos = get_possible_moves ultimo_estado visitados in
    let nuevas_tuplas = 
      List.fold_left (fun acc mov ->
        let nuevo_estado = make_move ultimo_estado mov in
        let hash_nuevo_estado = hash_state nuevo_estado in
        if List.exists ((=) hash_nuevo_estado) visitados then acc
        else
          (* Agregar el nuevo movimiento a la secuencia de movimientos y el nuevo estado *)
          let nueva_secuencia = (mov :: movimientos, nuevo_estado) in
          nueva_secuencia :: acc
      ) [] posibles_movimientos
    in
    (* Actualizar la lista de visitados con los nuevos estados generados *)
    let nuevos_visitados_actualizado = actualizar_visitados ultimo_estado posibles_movimientos nuevos_visitados in
    (nuevas_tuplas @ nuevas_secuencias, nuevos_visitados_actualizado)
  ) ([], visitados) secuencias
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


let hanoi n (a1, b1, c1) (a2, b2, c2) =
  if not (is_stable (a1, b1, c1)) || not (is_stable (a2, b2, c2)) then
    raise (Invalid_argument "Estados no estables")
  else
    let estado_inicial = (a1, b1, c1) in
    let secuencias = [([], estado_inicial)] in
    let visitados_inicial = [] in

    let rec aux1 l visitados =
      let rec aux2 = function
        | [] -> []
        | (movimientos, ultimo_estado) :: t ->
            if ultimo_estado = (a2, b2, c2) then
              List.rev movimientos  (* Devolvemos la lista de movimientos en el orden correcto *)
            else
              aux2 t
      in
      let resultado = aux2 l in
      if resultado = [] then
        let nuevas_secuencias, nuevos_visitados = iterar l visitados in
        aux1 (filtrar nuevas_secuencias visitados) nuevos_visitados  (* Llamada recursiva para continuar buscando *)
      else
        resultado
    in
    transform (aux1 secuencias visitados_inicial)  (* Llamada inicial a aux1 *)
;;


let hanoi n (a1, b1, c1) (a2, b2, c2) =
  if not (is_stable (a1, b1, c1)) || not (is_stable (a2, b2, c2)) then
    raise (Invalid_argument "Estados no estables")
  else
    let estado_inicial = (a1, b1, c1) in
    let secuencias = [([], estado_inicial)] in
    let visitados_inicial = [] in

    let rec aux1 l visitados =
      let rec aux2 = function
        | [] -> []
        | (movimientos, ultimo_estado) :: t ->
            if ultimo_estado = (a2, b2, c2) then
              List.rev movimientos  (* Devolvemos la lista de movimientos en el orden correcto *)
            else
              aux2 t
      in
      let resultado = aux2 l in
      let nuevas_secuencias, nuevos_visitados = iterar l visitados in nuevas_secuencias
    in
    aux1 secuencias visitados_inicial  (* Llamada inicial a aux1 *)
;;



let hanoi n (a1, b1, c1) (a2, b2, c2) =
  if not(is_stable (a1,b1,c1)) || not(is_stable (a2,b2,c2)) then raise (Invalid_argument "Estados no estables") 
  else
    let secuencias=  [([], estado_inicial)] in
    let visitados_inicial = [] in
    let rec aux1 l visitados2=
        let rec aux2= function
          [] -> []
        | h::t -> let (a,b)=last h in if a = (a2,b2,c2) then h else aux2 t
        in let lista=aux2 l in if lista=[] then aux1 ((expandir_secuencias l visitados2)) (actualizar_visitados visitados2 secuencias) else lista
    in transform (aux1 secuencias (extraer_estados_unicos secuencias))
;;