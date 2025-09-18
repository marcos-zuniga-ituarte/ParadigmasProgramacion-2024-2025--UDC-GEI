let rec fib n =
if n <= 2 then 1
else fib (n-1) + fib (n-2);;

let print_fib_to max = 
    let rec aux i = 
        let f = fib i in
            if f < max then let _ = print_endline (string_of_int f) 
                            in aux (i+1)
            else ()
    in aux 1;;

let print_fib_to max = 
    let rec aux i = 
        let f = fib i in
            if f < max then let _ = aux (i+1) in
                            print_endline (string_of_int f) 
            else ()
    in aux 1;;




let rec fib_to n =
    let n1, n2 = n-1, n-2 in
    if n <= 2 then (print_endline("1"),1)
    else 
    (print_endline(string_of_int ((function (a,c)-> c)(fib_to (n1)) + (function (a,c)-> c)(fib_to (n2)))),
    (function (a,c)-> c)(fib_to (n1)) + (function (a,c)-> c)(fib_to (n2)));;

(function (a,b,c)-> c)


let rec fib2 n = 
    if n = 1 then 1, 0, ()
    else let fa, faa = (function (a,b,c)-> a, b)(fib2 (n-1))
        in fa + faa, fa, if (fa)<n then print_endline(string_of_int(faa+fa)) else ();;






let rec fib2 n = 
    if n = 1 then 1, 0
    else let fa, faa = fib2 (n-1)
        in fa + faa, fa;;
let fib n = fst (fib2 n);;


let fastfib n = fst (fib2 n);;

let crono f x =
    let t = Sys.time () in
    let _= f x



let fib' n =
    let rec aux (i, u, p) =
        if i = n then u
        else aux (i+1, u+p, u)
    in aux (1, 1, 0);;



let rec fib n =
    if n <= 2 then 1
    else 