let rec fib2 n = 
    if n = 1 then 1, 0
    else let fa, faa = fib2 (n-1)
        in fa + faa, fa;;

let fib n = fst (fib2 n);;

let print_fib_to max = 
    let rec aux i = 
        let f = fib i in
            if f < max then let _ = print_endline (string_of_int f) 
                            in aux (i+1)
            else ()
    in aux 1;;

let _ = print_fib_to (int_of_string(Sys.argv.(1)))