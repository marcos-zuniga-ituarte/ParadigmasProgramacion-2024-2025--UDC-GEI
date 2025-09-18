let move (origen,destino)= " 1"^(if origen=1 && destino=3 then " ---"else if origen=2 && destino=1 then " <- " else if origen=3 && destino=1 then " <--" else if origen=1 && destino=2 then "--> " else "    ")^"2"^(if origen=3 && destino=1 then "--- "else if origen=3 && destino=2 then " <- " else if origen=2 && destino=3 then " -> "else if origen=1 && destino=3 then "--> " else "    ")^"3\n";;
let otro origen destino= 6 - origen - destino;;


let rec hanoi n ori des = 
    (* n número de discos, 1 <= ori <= 3, 1 <= dest <= 3, ori <> des *)
    if n = 0 then "" else
    let otro = otro ori des in
    hanoi (n-1) ori otro ^ move (ori, des) ^ hanoi (n-1) otro des;;
    
let hanoi n ori des =
    if n = 0 || ori = des then "\n"
    else hanoi n ori des;;
       
let print_hanoi n ori des =
    if n < 0 || ori < 1 || ori > 3 || des < 1 || des > 3
       then print_endline  " ** ERROR ** \n"
       else print_endline (" =========== \n" ^ 
                           hanoi n ori des ^
                           " =========== \n");;

let rec nth_hanoi_move n nd ori des =
    let nmovimientos=(int_of_float (2.0**(float_of_int (nd-1))))-1 in
    let otro=otro ori des in
    if n<=nmovimientos then nth_hanoi_move n (nd-1) ori otro
    else if n=nmovimientos+1 then (ori,des)
    else nth_hanoi_move (n-nmovimientos-1) (nd-1) otro des;;

let crono f x =
    let t = Sys.time () in
    let _ = f x in
    Sys.time () -. t;;


(*
# crono (fun () -> hanoi 20 3 2) ();;
- : float = 0.298503000000000185
# crono (fun () -> hanoi 25 3 2) ();;
- : float = 9.39128499999999633
# crono (fun () -> hanoi 20 3 2) ();;
- : float = 0.329662000000006117
# crono (fun () -> hanoi 25 3 2) ();;
- : float = 9.51196399999999898
# crono (fun () -> hanoi 20 3 2) ();;
- : float = 0.340241000000006
# crono (fun () -> hanoi 25 3 2) ();;
- : float = 9.63658700000000579

COMPROBACION PARA LA CUADRATICA
# 0.329662000000006117*.(2.0**5.0);;
- : float = 10.5491840000001957
*)

(*
# crono (fun () -> nth_hanoi_move 1_000_000 2_000 3 2) ();;
- : float = 9.70000000037885e-05
# crono (fun () -> nth_hanoi_move 1_000_000 4_000 3 2) ();;
- : float = 0.000185999999999353349
# crono (fun () -> nth_hanoi_move 1_000_000 2_000 3 2) ();;
- : float = 8.6000000003139121e-05
# crono (fun () -> nth_hanoi_move 1_000_000 4_000 3 2) ();;
- : float = 0.00015799999999899228
# crono (fun () -> nth_hanoi_move 1_000_000 2_000 3 2) ();;
- : float = 8.50000000056638783e-05
# crono (fun () -> nth_hanoi_move 1_000_000 4_000 3 2) ();;
- : float = 0.000155999999996936367

COMPROBACION PARA LA LINEAL:
# 8.6000000003139121e-05 *. (4000.0 /. 2000.0);;
- : float = 0.000172000000006278242
*)