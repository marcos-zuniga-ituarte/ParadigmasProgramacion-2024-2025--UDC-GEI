let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec verify n =
	n = 1 || verify (f n);;

let rec verify_to n = if n = 1 then verify (n) else verify (n) && verify_to (n-1);;

let rec orbit n = string_of_int n ^ (if n = 1 then "" else ", "^(orbit(f n)));;

let rec length n = if n = 1 then 1 else 1 + length (f n);;

let rec top = function 1 -> 1 | n -> let m = top (f n) in if n > m then n else m;;
