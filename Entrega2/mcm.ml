let rec mcd a b = let r = a mod b in if r=0 then b else mcd b r;;

let mcm x y=
     let solmcd = mcd x y in
     if x*y<x || x*y<y then (x/solmcd)*(y/solmcd)*solmcd
     else (x*y)/solmcd;;

let mcm' x y=
     let solmcd = mcd x y in
     if x<=0 || y<=0 then (-1)
     else if x*y<x || x*y<y then let sol=(x/solmcd)*(y/solmcd)*solmcd in if sol<x || sol<y then (-1) else sol
     else (x*y)/solmcd;;
