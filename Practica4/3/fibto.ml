let rec fib n =
    if n <= 2 then 1
    else fib (n-1) + fib (n-2)

let print_fib_to max = 
    let rec aux i = 
        let f = fib i in
            if f < max then let _ = print_endline (string_of_int f) 
                            in aux (i+1)
            else ()
    in aux 1

let _ = if Array.length Sys.argv = 2 then print_fib_to (int_of_string(Sys.argv.(1))) else print_endline "fibto: Invalid number of arguments"