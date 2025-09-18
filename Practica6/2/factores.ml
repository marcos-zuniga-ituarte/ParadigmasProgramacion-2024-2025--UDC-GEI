let factoriza =function 0->"0" 
                      | 1->"1" 
                      |n->let rec aux n2 f=
                          if f>n2/f then string_of_int(n2)
                          else if n2 mod f = 0 then string_of_int f ^" * "^ aux (n2/f) f
                                               else aux n2 (f+1)
  in 
  if n=min_int then "(-1)"^" * "^(aux ((-(n/2))mod max_int) 2) ^ " * 2"
  else if n<0 then if n=(-1) then "(-1)" else "(-1) * "^aux ((-n)mod max_int) 2
  else aux n 2;;
