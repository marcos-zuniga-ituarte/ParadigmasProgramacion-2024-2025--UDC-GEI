let rec mcd_pasos a b = let r = a mod b in if r=0 then (b,1) else let (x,y) = mcd_pasos b r in (x,y+1);;


let rec mcd_pasos a b = 
    let r = a mod b in 
    if r=0 then (b,1) 
    else ((function (x,y) -> (x, y+1))(mcd_pasos b r));;