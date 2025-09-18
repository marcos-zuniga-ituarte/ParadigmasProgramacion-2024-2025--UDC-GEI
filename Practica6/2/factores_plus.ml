let factoriza =function 0->"0" 
                      | 1->"1" 
                      |n->let rec aux n2 f rep=
                          if f>n2/f then if rep=0 then string_of_int(n2) else if n2 mod f <> 0 then if rep=1 then " * "^string_of_int n2 else "^"^(string_of_int (rep))^" * "^string_of_int n2  else "^"^string_of_int (rep+1)
                          else if n2 mod f = 0 then if rep=0 then string_of_int f ^ aux (n2/f) f (rep+1) else aux (n2/f) f (rep+1)
                                               else if rep=0 then aux n2 (f+1) 0 else if rep = 1 then " * "^aux n2 (f+1) 0 else "^"^string_of_int rep ^" * "^aux n2 (f+1) 0
  in 
  if n=min_int then "(-1)"^" * 2"^aux ((-(n/2))mod max_int) 2 1
  else if n<0 then if n=(-1) then "(-1)" else "(-1) * "^aux ((-n)) 2 0
  else aux n 2 0;;

