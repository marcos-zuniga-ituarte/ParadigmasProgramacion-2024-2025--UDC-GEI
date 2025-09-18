let pi = 2.0*.asin 1.0

let e = exp(1.0)

let max_int_f = float_of_int (max_int)

let per = function x -> 2.0 *. pi *. x

let area = function x -> pi *. x *. x

let next_char = function x -> char_of_int(int_of_char(x)+1)

let absf = function x -> if x<0. then x *. (-1.) else x

let odd = function x -> (x mod 2) <> 0

let next5mult = function x -> ((x/5)+1)*5

let is_letter = function x -> (x>='a' && x<='z') || (x>='A' && x<='Z')

let string_of_bool = function x -> if x then "verdadero" else "falso"
