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
    | [] -> (torre, destino)
    | hd :: tl -> (tl, hd :: destino)
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
  | _ -> (a, b, c)
;;
let rec hash l=
    match l with
        []->0
      | h::t -> h*2 + hash t
;;

let hash_state (torre1, torre2, torre3) =
  (torre1, torre2, torre3)
;;

let existe elem lista =
  List.exists (fun x -> x = elem) lista
;;

let get_possible_moves (a, b, c)=
  let rec aux l =
    match l with
    | [] -> []
    | h :: t ->
        let nuevo_estado = make_move (a, b, c) h in
        if is_stable nuevo_estado && nuevo_estado <> (a, b, c) then
          (h) :: aux t
        else
          aux t
  in
  (aux movimientos)
;;

let rec actualizar_visitados secuencias visitados =
    match secuencias with
        []->visitados
      | (a,b)::t -> let hash=hash_state b in if existe hash visitados then actualizar_visitados t visitados else actualizar_visitados t (hash::visitados)
;;

let rec filtrar posibles visitados =
  match posibles with
      [] -> []
    | (a,b)::t -> if existe (hash_state b) visitados then filtrar t visitados else (a,b)::(filtrar t visitados)
;;

let rec actualizar_secuencia movimientos (a,b)=
    match movimientos with
        []->[]
      | h::t -> (a @ [h],make_move b h)::actualizar_secuencia t (a,b)
;;

let iterar secuencias=
    let rec aux sec=
        match sec with
            [] ->[]
          | (a,b)::t -> let mposibles=get_possible_moves b in
                        let updated_secuencia=actualizar_secuencia mposibles (a,b) in
                        updated_secuencia @ aux t
    in aux secuencias
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

let rec buscar_solucion (a,b,c) l=
    match l with
        [] -> []
      | (x,y)::t -> if y=(a,b,c) then x else buscar_solucion (a,b,c) t
;;

let contar_discos_en_posicion t1 t2 =
  let rec aux l1 l2 acc =
    match (l1, l2) with
    | ([], _) | (_, []) -> acc
    | (d1 :: tl1, d2 :: tl2) ->
        if d1 = d2 then aux tl1 tl2 (acc + 1)
        else acc
  in
  aux (List.rev t1) (List.rev t2) 0
;;

let puntaje_estado (t1_ini, t2_ini, t3_ini) (t1_fin, t2_fin, t3_fin) =
  contar_discos_en_posicion t1_ini t1_fin +
  contar_discos_en_posicion t2_ini t2_fin +
  contar_discos_en_posicion t3_ini t3_fin
;;


let filtrar_por_puntaje_final estado_final secuencias =
  let puntajes = List.map (fun (a, b) -> (a, b, puntaje_estado b estado_final)) secuencias in
  let max_puntaje = List.fold_left (fun acc (_, _, p) -> max acc p) 0 puntajes in
  List.fold_right (fun (a, b, p) acc -> if p = max_puntaje then (a, b) :: acc else acc) puntajes []
;;


let hanoi n (a1, b1, c1) (a2, b2, c2) =
  if not(is_stable (a1,b1,c1)) || not(is_stable (a2,b2,c2)) 
  then raise (Invalid_argument "Estados no estables") 
  else
    let estado_inicial = (a1, b1, c1) in
    let secuencias = [([], estado_inicial)] in
    let visitados_inicial = [] in
    let rec aux1 secuencias2 visitados2=
        let secuencias2=iterar secuencias2 in
        let encontrado=buscar_solucion (a2, b2, c2)  secuencias2 in
        let sec_filtered=filtrar secuencias2 visitados2 in
        let sec_filtered=filtrar_por_puntaje_final (a2, b2, c2) sec_filtered in
        let visitados3=actualizar_visitados sec_filtered visitados2 in
            if encontrado<>[] 
            then encontrado
            else if sec_filtered = [] then [] else aux1 (sec_filtered) visitados3
    in transform (aux1 secuencias visitados_inicial)
;;
