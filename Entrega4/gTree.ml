type 'a gtree = GT of 'a * 'a gtree list;;

type 'a t = 'a gtree;;

let leaftree r = GT(r,[]);;

let is_leaf = function
    GT(r,[]) -> true
  | _ -> false
;;

let root (GT(r,ln)) = r;;

let branches (GT(r,ln)) = ln;;

let rec size = function
    GT(_,[]) -> 1
  | GT(_,ln) -> let rec aux = function
                    [] -> 0
                  | h::t -> size h + aux t
                in 1 + aux ln
;;

let rec height = function
    GT(_,[]) -> 1
  | GT(_,ln) -> let rec aux = function
                    [] -> 0
                  | h::t -> max (height h)  (aux t)
                in 1 + aux ln
;;

let rec preorder = function
    GT(x,[]) -> [x]
  | GT(x,ln) -> let rec aux = function
                    [] -> []
                  | h::t -> preorder h @ (aux t)
                in [x] @ aux ln
;;

let rec postorder = function
    GT(x,[]) -> [x]
  | GT(x,ln) -> let rec aux = function
                    [] -> []
                  | h::t -> postorder h @ (aux t)
                in aux ln @ [x]
;;

let breadth a =
  let rec aux cola2 = function
      [] -> if cola2=[] then [] else aux [] (List.rev cola2)
    | GT(x,[])::t -> x :: aux cola2 t
    | GT(x,ln) :: t -> x :: aux ((List.rev ln) @ cola2) (t)  (* ineficiente *)
  in aux [] [a]
;;

let rec leaves = function
    GT(x,[]) -> [x]
  | GT(x,ln) -> let rec aux = function
                    [] -> []
                  | h::t -> leaves h @ (aux t)
                in aux ln
;;

let rec find_in_depth_aux f b = if f (root b) 
                            then  Some (root b)
                            else  let rec aux = function
                                      [] -> None
                                    | h::t -> let finded2 = find_in_depth_aux f h in
                                              if finded2<>None then finded2 else aux t
                                  in aux (branches b)
;;

let find_in_depth f b = let solucion = find_in_depth_aux f b in if solucion = None then raise Not_found else Option.get(solucion);;

let find_in_depth_opt = find_in_depth_aux;;

let breadth_find f b =find_in_depth f b;;

let breadth_find_opt f b = find_in_depth_opt f b;;

let rec exists f b = if is_leaf b then f (root b) else  let rec aux = function
                                                            [] -> false
                                                          | h::t -> exists f h || aux t
                                                        in (f (root b)) || aux (branches b)
;;

let rec for_all f b = ((is_leaf b) && f (root b)) ||  let rec aux = function
                                                          [] -> true
                                                        | h::t -> exists f h && aux t
                                                      in (f (root b)) && aux (branches b)
;;

let rec map f b = if is_leaf b
  then GT(f (root b),[])
  else GT(f (root b), let rec aux = function
                          [] -> []
                        | h::t -> map f h :: aux t
                      in aux (branches b))
;;

let rec mirror (GT(r,ln))= GT(r,List.rev (List.map (fun x -> mirror x) ln));;

let rec replace_when f b1 b2 = 
    if f (root b1)
    then b2 else GT(root b1, List.map (fun x -> replace_when f x b2) (branches b1))
;;

let rec cut_below f b1 =
    if f (root b1)
    then GT(root b1,[])
    else GT(root b1, let rec aux = function
                          [] -> []
                        | h::t -> cut_below f h :: aux t
                      in aux (branches b1))
;;

let rec from_bin_aux b =
    let l,r = (BinTree.left_b b), BinTree.right_b b in
    GT(BinTree.root b, (if BinTree.is_empty l then [] else [from_bin_aux l])@(if BinTree.is_empty r then [] else [from_bin_aux r]))
;;

let from_bin b = if BinTree.is_empty b then raise(Failure "from_bin") else from_bin_aux b;;

let rec from_stbin b =
  let l,r = (StBinTree.left_b b), StBinTree.right_b b in
  GT(StBinTree.root b, (if StBinTree.is_leaf l then [GT(StBinTree.root l,[])] else [from_stbin l])@(if StBinTree.is_leaf r then [GT(StBinTree.root r,[])] else [from_stbin r]))
;;