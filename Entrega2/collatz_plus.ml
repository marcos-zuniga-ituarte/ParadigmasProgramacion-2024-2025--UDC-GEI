let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec length'n'top = function 1 -> (1,1) | n -> let (a,b) = length'n'top (f n) in if n > b then ((function (x,y) -> (x+1, y))(a,n)) else ((function (x,y) -> (x+1, y))(a,b));;
