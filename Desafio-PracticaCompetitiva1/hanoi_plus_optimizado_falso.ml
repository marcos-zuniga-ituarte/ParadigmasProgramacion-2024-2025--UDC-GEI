type move = LtoC | LtoR | CtoL | CtoR | RtoL | RtoC;;
let movimientos = [(1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2)];;

let is_stable (a, b, c) =
  let rec check_order = function
    | [] -> true
    | [_] -> true
    | h1 :: h2 :: t -> h1 < h2 && check_order (h2 :: t)
  in
  check_order a && check_order b && check_order c
;;

let make_move (a, b, c) (ori, des) =
  let mover_de_torre torre destino =
    match torre with
    | [] -> (torre, destino)  (* Si la torre está vacía, no hace nada *)
    | hd :: tl -> (tl, hd :: destino)  (* Mueve el disco superior *)
  in
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

let rec exp base power =
  if power = 0 then 1
  else if power mod 2 = 0 then
    let half = exp base (power / 2) in
    half * half
  else
    base * exp base (power - 1)
;;

let hash_state (torre1, torre2, torre3) =
  let calcular_hash torre base =
    List.fold_left (fun acc (disco, pos) -> acc + disco * (exp base pos)) 0
      (List.mapi (fun i disco -> (disco, i + 1)) torre)
  in
  calcular_hash torre1 11 + calcular_hash torre2 13 + calcular_hash torre3 17
;;

let get_possible_moves (a, b, c) =
  let rec aux l =
    match l with
    | [] -> []
    | h :: t ->
        let nuevo_estado = make_move (a, b, c) h in
        if is_stable nuevo_estado && nuevo_estado <> (a, b, c) then
          h :: aux t
        else
          aux t
  in
  aux movimientos
;;

let rec actualizar_secuencia movimientos (a, b) =
  match movimientos with
  | [] -> []
  | h :: t -> (a @ [h], make_move b h) :: actualizar_secuencia t (a, b)
;;

let iterar secuencias =
  let rec aux sec acumulador =
    match sec with
    | [] -> acumulador
    | (a, b) :: t ->
        let mposibles = get_possible_moves b in
        let updated_secuencia = actualizar_secuencia mposibles (a, b) in
        aux t (List.rev_append updated_secuencia acumulador)
  in
  filtrar_local (aux secuencias [])
;;

let movement_interpreter = function
  | (1, 2) -> LtoC
  | (1, 3) -> LtoR
  | (2, 1) -> CtoL
  | (2, 3) -> CtoR
  | (3, 1) -> RtoL
  | (3, 2) -> RtoC
;;

let rec transform = function
  | [] -> []
  | (a, b) :: t -> movement_interpreter a :: transform t
;;

let buscar_solucion (a, b, c) l =
  let rec aux = function
    | [] -> []
    | (x, y) :: t -> if y = (a, b, c) then x else aux t
  in
  aux l
;;

let filtrar_local secuencias =
  let rec aux sec visitados =
    match sec with 
    | [] -> []
    | (a, b) :: t ->
        let hash = hash_state b in
        if Hashtbl.mem visitados hash then aux t visitados
        else (
          Hashtbl.add visitados hash true;
          (a, b) :: aux t visitados
        )
  in
  aux secuencias (Hashtbl.create 1000)
;;

let hanoi n (a1, b1, c1) (a2, b2, c2) =
  if not (is_stable (a1, b1, c1)) || not (is_stable (a2, b2, c2)) 
  then raise (Invalid_argument "Estados no estables") 
  else
    let estado_inicial = (a1, b1, c1) in
    let secuencias = [([], estado_inicial)] in
    let visitados = Hashtbl.create 1000 in
    let rec aux1 secuencias2 =
      let secuencias2 = iterar secuencias2 in
      let encontrado = buscar_solucion (a2, b2, c2) secuencias2 in
      if encontrado <> [] then encontrado
      else
        let sec_filtered = filtrar_local secuencias2 in
        if sec_filtered = [] then []
        else aux1 sec_filtered
    in
     (aux1 secuencias)
;;
