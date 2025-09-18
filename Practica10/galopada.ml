let posible_jump_tranformations = [(-1,-2);(1,-2);(2,-1);(2,1);(1,2);(-1,2);(-2,1);(-2,-1)];;

let valid_box n (x,y)=x>=1 && y>=1 && x<=n && y<=n;;

let rec peones_validos n =function
    [] -> true
  | h::t -> valid_box n h && not (List.mem h t) && peones_validos n t
;;

let make_transformation (x1,y1) (x2,y2)=(x1+x2,y1+y2);;

let rec posible_jumps (x,y) =
    List.map (make_transformation (x,y)) posible_jump_tranformations;;

let come cosa1 cosa2 = 
    let listaCosa2=posible_jumps cosa2 in
    List.mem cosa1 listaCosa2
;;

let rec puede_galopar k = function
    [] -> [] 
  | h::t -> if come k h then h::puede_galopar k t else puede_galopar k t
;;


let puede_comer k peones = List.filter (come k) peones;;

let rec remove elemento =function
    [] -> []
  | h::t -> if h = elemento then t else h::remove elemento t
;;


let rec buscar k  = function 
    [] -> [k] 
  | peones -> 
        let rec intenta = function
            [] -> raise Not_found
          | h::t ->
                try k::buscar h (remove h peones)
                with Not_found -> intenta t
        in intenta (puede_comer k peones)
;;


let galopada n l =
  if peones_validos n l && l<>[] then
      let rec aux (counter1,counter2)= 
          if not (List.mem  (counter1,counter2) l) then
              try buscar (counter1,counter2) l
              with Not_found -> 
                if (counter1,counter2)=(1,1)
                then raise Not_found 
                else if counter2 = 1 then aux ((counter1-1),(n)) else aux ((counter1),(counter2-1))
          else if (counter1,counter2)=(1,1)
            then raise Not_found 
            else if counter2 = 1 then aux ((counter1-1),(n)) else aux ((counter1),(counter2-1))
      in aux (n,n)
  else raise (Invalid_argument "galopada")
;;