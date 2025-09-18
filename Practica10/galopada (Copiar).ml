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


let rec buscar 





let rec galopada n l=
    if List.length l = 1 then l
    else
    let rec aux =function
        [] -> []
      | h1::t1 -> 
            let posibles_galopados = puede_galopar n h1 l in
            if posibles_galopados = [] then aux t1
            else let rec aux2 = function
                     [] -> []
                   | h2::t2 -> let galopado = galopada n (remove h1 l) in 
                               if galopado = [] then aux2 t2 else h1::galopado
                  in let solucion = aux2 posibles_galopados in if solucion = [] then aux t1 else 
                                                                                                if come n h1 (List.hd (remove h1 solucion)) then solucion else aux t1
    in aux l
;;



let rec camino_correcto n = function
    [] -> true 
  | h::[] -> true
  | h1::h2::t -> come n h1 h2 && camino_correcto n (h2::t)
;;


    
galopada 6 [(1,4);(2,6);(3,4);(4,5);(2,2);(3,3);(6,6);(5,4);(5,3);(4,1)];;

(*correcto*)
galopada 6 [(1,4);(2,6);(3,4);(4,5);(2,2);(3,3);(5,3);(4,1)];;
galopada 6 [(1,4);(2,6);(3,4);(4,5);(2,2);(3,3);(6,6);(5,3);(4,1)];;
galopada 6 [(1,4);(2,6);(3,4);(4,5);(2,2);(3,3);(6,6);(5,4);(5,3);(4,1)];;
[(5, 4); (6, 6); (4, 5); (5, 3); (4, 1); (2, 2); (3, 4); (2, 6); (1, 4); (3, 3); (1, 2)]



camino_correcto 6 (galopada 6 [(1,4);(2,6);(3,4);(4,5);(2,2);(3,3);(6,6);(5,4);(5,3);(4,1)]);;
posible_jumps 5 (3,3);;
puede_galopar 5 (3,3) [(5,4);(3,5);(5,2)];;

galopada 5 [(3,3);(5,4)];;
galopada 5 [(3,3);(5,4);(4,5)];;




let rec camino_correcto n = function
    [] -> []
  | h::[] -> [h]
  | h1::h2::t -> if come n h1 h2 then h1::camino_correcto n (h2::t) else [h1]
;;





puede_galopar 5 (3,3) [(5,4);(3,5)];;
