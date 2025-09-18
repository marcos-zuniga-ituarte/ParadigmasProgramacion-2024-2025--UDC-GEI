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
let rec hash l=
    match l with
        []->0
      | h::t -> h*2 + hash t
;;
(*)
let hash_state (torre1, torre2, torre3) =
  let calcular_hash torre =
    List.fold_left (fun acc (disco, pos) -> acc + disco * pos) 0 (List.mapi (fun i disco -> (disco, List.length torre - i)) torre)
  in
  calcular_hash torre1 + (calcular_hash torre2*2) + (calcular_hash torre3)*3
;;*)

let hash_state (torre1, torre2, torre3) =
  (torre1, torre2, torre3)
;;

let existe elem lista =
  List.exists (fun x -> x = elem) lista
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

(* Función que cuenta discos en posición correcta desde la base *)
let contar_discos_en_posicion t1 t2 =
  let rec aux l1 l2 acc =
    match (l1, l2) with
    | ([], _) | (_, []) -> acc  (* Detener si una de las listas se agota *)
    | (d1 :: tl1, d2 :: tl2) ->
        if d1 = d2 then aux tl1 tl2 (acc + 1)
        else acc
  in
  aux (List.rev t1) (List.rev t2) 0
;;

(* Función principal para obtener el puntaje total entre dos estados *)
let puntaje_estado (t1_ini, t2_ini, t3_ini) (t1_fin, t2_fin, t3_fin) =
  contar_discos_en_posicion t1_ini t1_fin +
  contar_discos_en_posicion t2_ini t2_fin +
  contar_discos_en_posicion t3_ini t3_fin
;;

let filtrar_por_puntaje_final estado_final secuencias =
  (* Calcular los puntajes de cada secuencia en relación al estado final *)
  let puntajes = List.map (fun (a, b) -> (a, b, puntaje_estado b estado_final)) secuencias in
  (* Obtener el puntaje máximo de la lista *)
  let max_puntaje = List.fold_left (fun acc (_, _, p) -> max acc p) 0 puntajes in
  (* Filtrar solo las secuencias que tengan el puntaje máximo *)
  List.fold_right (fun (a, b, p) acc -> if p = max_puntaje then (a, b) :: acc else acc) puntajes []
;;


let rec existe_sec posibles visitados =
  match posibles with
      [] -> []
    | (a,b)::t -> if existe (hash_state b) visitados then [(a,b)] else existe_sec t visitados 
;;


let invertir_tuplas lista =
  List.map (fun (a, b) -> (b, a)) lista
;;


let buscar_solucion2 sec rev_sec=
    let visitados=actualizar_visitados sec [] in
    let l=existe_sec rev_sec visitados in
    if l=[] then []
    else let (a,b) = List.hd l in (buscar_solucion b sec) @ (invertir_tuplas(List.rev a))
;;









(* Función auxiliar para encontrar la torre que no contiene ni el disco inicial ni el final *)
let torre_auxiliar torre_inicial torre_destino =
  match (torre_inicial, torre_destino) with
  | (1, 2) | (2, 1) -> 3
  | (1, 3) | (3, 1) -> 2
  | (2, 3) | (3, 2) -> 1
  | _ -> failwith "Torres no válidas"
;;

(* Función para obtener el disco más grande presente en una lista de torres *)
let disco_mas_grande t1 t2 t3 =
  List.fold_left max 0 (List.concat [t1; t2; t3])
;;

(* Función que calcula el estado intermedio *)
let estado_intermedio estado_inicial estado_final =
  let (t1_ini, t2_ini, t3_ini) = estado_inicial in
  let (t1_fin, t2_fin, t3_fin) = estado_final in

  (* Determinar el disco más grande que necesita moverse *)
  let disco_mayor = disco_mas_grande t1_ini t2_ini t3_ini in

  (* Determinar en qué torre está el disco más grande al inicio y al final *)
  let torre_inicial = if List.mem disco_mayor t1_ini then 1 else if List.mem disco_mayor t2_ini then 2 else 3 in
  let torre_destino = if List.mem disco_mayor t1_fin then 1 else if List.mem disco_mayor t2_fin then 2 else 3 in
  let torre_aux = torre_auxiliar torre_inicial torre_destino in

  (* Obtener los discos menores *)
  let discos_menores = List.filter (fun x -> x < disco_mayor) (List.concat [t1_ini; t2_ini; t3_ini]) in

  (* Construir el estado intermedio *)
  match (torre_inicial, torre_aux, torre_destino) with
  | (1, 2, 3) -> ([disco_mayor], discos_menores, [])
  | (1, 3, 2) -> ([disco_mayor], [], discos_menores)
  | (2, 1, 3) -> (discos_menores, [disco_mayor], [])
  | (2, 3, 1) -> ([], [disco_mayor], discos_menores)
  | (3, 1, 2) -> (discos_menores, [], [disco_mayor])
  | (3, 2, 1) -> ([], discos_menores, [disco_mayor])
  | _ -> failwith "Configuración no válida"
;;




















let hanoi n (a1, b1, c1) (a2, b2, c2) =
  if not(is_stable (a1,b1,c1)) || not(is_stable (a2,b2,c2)) 
  then raise (Invalid_argument "Estados no estables") 
  else
    let estado_inicial = (a1, b1, c1) in
    let estado_final = (a2, b2, c2) in
    let secuencias = [([], estado_inicial)] in
    let rev_secuencias = [([], estado_final)] in
    let visitados_inicial = [] in
    let rev_visitados_inicial = [] in
    let rec aux1 secuencias1 visitados2 rev_secuencias2 rev_visitados2=
        
        let secuencias2=iterar secuencias1 in
        let encontrado=buscar_solucion2 secuencias2 rev_secuencias2 in
        if encontrado <> [] then encontrado
        else
        let rev_secuencias2=iterar rev_secuencias2 in
        let encontrado=buscar_solucion2 secuencias1 rev_secuencias2 in
        if encontrado <> [] then encontrado
        else
        let sec_filtered=filtrar secuencias2 visitados2 in
        let rev_sec_filtered=filtrar rev_secuencias2 rev_visitados2 in
        let sec_filtered=filtrar_por_puntaje_final (a2, b2, c2) sec_filtered in
        let rev_sec_filtered=filtrar_por_puntaje_final (a1, b1, c1) rev_sec_filtered in
        (*let sec_filtered=rev_filtrar_por_puntaje_final (a2, b2, c2) sec_filtered in*)
        let visitados3=actualizar_visitados sec_filtered visitados2 in
        let rev_visitados3=actualizar_visitados rev_sec_filtered rev_visitados2 in
        if sec_filtered = [] || rev_sec_filtered = [] then [] else aux1 (sec_filtered) visitados3 (rev_sec_filtered) rev_visitados3
    in transform (aux1 secuencias visitados_inicial rev_secuencias rev_visitados_inicial)
;;
