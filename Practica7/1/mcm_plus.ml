let rec mcd a b = let r = a mod b in if r=0 then b else mcd b r;;

let mcm' x y=
    if x<=0 || y<=0 then raise (Failure "mcm': uno de los argumentos es menor o igual a 0")    
    else let presol = x/(mcd x y) in
    if presol>max_int/y then raise (Failure "mcm':  mcm excede max_int") else presol*y;;
