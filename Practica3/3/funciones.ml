let rec factorial = function 0 -> 1 | n -> n * factorial (n-1);;
(*val factorial : int -> int=<fun>*)
factorial 0 + factorial 1 + factorial 2;;
(*- : int = 4*)
factorial 10;;
(*- : int = 3628800*)
factorial 100;;
(*- : int = 0*)
factorial (-1);;
(*Stack overflow during evaluation (looping recursion?).*)
let rec sumto = function 0 -> 0 | x->x + sumto(x-1);;
(*val sumto : int -> int = <fun>*)
let rec exp2 = function 0 -> 1 | x -> 2 * exp2(x-1);;
(*val exp2 : int -> int = <fun>*)
let rec num_cifras = function 0->1 | x -> if x < 0 then num_cifras (-x) else if 0<x && x<10 then 1 else 1+num_cifras(x/10);;
(*val num_cifras : int -> int = <fun>*)
let rec sum_cifras = function 0 -> 0 | x -> abs(x mod 10) + sum_cifras(x/10);;
(*val sum_cifras : int -> int = <fun>*)
