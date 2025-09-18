let rec binstr_of_int n = 
    if n/2 = 0 then string_of_int(n mod 2) else binstr_of_int(n/2)^string_of_int(n mod 2);;


let int_of_binstr s1 =
    let rec aux s=
        if 1=String.length s then int_of_char s.[0] - 48 else int_of_char s.[String.length s - 1] - 48 + 2 * aux(String.sub s 0 (String.length s - 1))
        in if String.length s1 >63 then aux (String.sub s1 (String.length s1 -63) (63)) else aux s1;;


let int_of_binstr' s1 =
    let rec aux s=
        if 1=String.length s then if s.[String.length s - 1]=' ' then 0 else int_of_char s.[0] - 48 else if s.[String.length s - 1]=' ' then aux(String.sub s 0 (String.length s - 1)) else int_of_char s.[String.length s - 1] - 48 + 2 * aux(String.sub s 0 (String.length s - 1))
in if String.length s1 > 63 then aux (String.sub s1 (String.length s1 -63) (63)) else aux s1;;
