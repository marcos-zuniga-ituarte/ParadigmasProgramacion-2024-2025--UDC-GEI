let tl' l=
    try List.tl l with
        Failure _ -> [];;


let tl_opt l=
    try Some (List.tl l) with
        Failure _ -> None;;

let come (i1,j1) (i2,j2) =
    i1 = i2 || j1 = j2 || (i1-i2) = abs (j1-j2)
;;

let compatible q l = not (List.exists (come q) l);;

let reinas n =
    let rec completar camino i j =
        if i > n then camino
        else if j > n then raise Not_found
        else if compatible (i,j) camino
                then try completar ((i,j)::camino) (i+1) 1
                     with Not_found -> completar camino i (j+1)
                else completar camino i (j+1)
    in completar [] 1 1                
;;

let reinas n =
    let rec completar camino i j =
        if i > n then camino
        else if j > n then raise Not_found
        else if compatible (i,j) camino
                then match completar ((i,j)::camino) (i+1) 1
                     with None -> completar camino i (j+1)
                       |  Some sol -> Some sol
                else completar camino i (j+1)
    in completar [] 1 1                
;;

let all_reinas n =
    let rec completar camino i j =
        if i > n then [camino]
        else if j > n then []
        else if compatible (i,j) camino
                then completar ((i,j)::camino) (i+1) 1 @ 
                     completar camino i (j+1)                
                else completar camino i (j+1)
    in completar [] 1 1                
;;

