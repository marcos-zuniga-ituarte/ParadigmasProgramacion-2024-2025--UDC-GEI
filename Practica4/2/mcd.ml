let rec mcd a b = if a=0 || b==0 then max a b else if a > b then let r = a-b in if r = 0 then b else mcd r b else let r = b-a in if r=0 then a else mcd a r;;

let rec mcd' a b = let r = a mod b in if r=0 then b else mcd' b r;;