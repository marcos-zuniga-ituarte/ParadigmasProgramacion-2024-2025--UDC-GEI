type 'a st_bintree =
    Leaf of 'a
  | SBT of 'a st_bintree * 'a * 'a st_bintree
;;

type 'a t = 'a st_bintree;;

let leaftree n = Leaf(n);;

let is_leaf = function
    Leaf(_) -> true
  | _ -> false
;;

let comb x l r = SBT(l,x,r);;

let root = function
    Leaf(x) -> x
  | SBT(_,x,_) -> x
;;

let left_b = function
    Leaf(_) -> raise (Failure "left_b")
  | SBT(r,_,_) -> r
;;

let right_b = function
    Leaf(_) -> raise (Failure "right_b")
  | SBT(_,_,r) -> r
;;

let root_replacement b n = 
    match b with
        Leaf(x) -> Leaf(n)
      | SBT(l,x,r) -> SBT(l,n,r)
;;

let left_replacement b b2 = 
    match b with
        Leaf(_) -> raise (Failure "left_replacement")
      | SBT(l,x,r) -> SBT(b2,x,r)
;;

let right_replacement b b2 = 
    match b with
        Leaf(_) -> raise (Failure "right_replacement")
      | SBT(l,x,r) -> SBT(l,x,b2)
;;

let rec size = function
    Leaf(_) -> 1
  | SBT(l,_,r) -> 1+(size l)+(size r)
;;

let rec height = function
    Leaf(_) -> 1
  | SBT(l,_,r) -> 1+max (height l) (height r)
;;

let rec preorder = function
    Leaf(x) -> [x]
  | SBT(l,x,r) -> [x] @ (preorder l) @ (preorder r)
;;

let rec inorder = function
    Leaf(x) -> [x]
  | SBT(l,x,r) -> (inorder l) @ [x] @ (inorder r)
;;

let rec postorder = function
    Leaf(x) -> [x]
  | SBT(l,x,r) -> (postorder l) @ (postorder r) @ [x]
;;

let breadth a =
  let rec aux cola2 = function
      [] -> if cola2=[] then [] else aux [] (List.rev cola2)
    | Leaf(x)::t -> x :: aux cola2 t
    | SBT (l,x,r) :: t -> x :: aux (r::l::cola2) (t)  (* ineficiente *)
  in aux [] [a]
;;

let rec leaves = function
    Leaf(x) -> [x]
  | SBT(l,n,r) -> (leaves l) @ (leaves r)
;;
(*
let rec find_in_depth_aux f b = if f (root b) 
                            then  Some (root b)
                            else let find_l = find_in_depth_aux f (left_b b) in 
                                 if find_l <> None 
                                 then find_l
                                 else let find_r = find_in_depth_aux f (right_b b) in
                                                       if find_r <> None
                                                       then find_r
                                                       else None
;;
*)
let rec find_in_depth_aux f b = if f (root b) 
                            then  Some (root b)
                            else if is_leaf b then None else
                              let find_l = find_in_depth_aux f (left_b b) in 
                                 if find_l <> None 
                                 then find_l
                                 else let find_r = find_in_depth_aux f (right_b b) in
                                                       if find_r <> None
                                                       then find_r
                                                       else None
;;

let find_in_depth f b = let solucion = find_in_depth_aux f b in if solucion = None then raise Not_found else Option.get(solucion);;

let find_in_depth_opt = find_in_depth_aux;;

let rec exists f b = if is_leaf b then f (root b) else (f (root b) || exists f (left_b b) || exists f (right_b b));;

let rec for_all f b = ((is_leaf b) && f (root b)) || (f (root b) && for_all f (left_b b) && for_all f (right_b b));;

let rec map f b = if is_leaf b
  then Leaf(f (root b))
  else SBT(map f (left_b b), f(root b),map f (right_b b))
;;

let rec mirror = function
    Leaf(x) -> Leaf(x)
  | SBT(l,x,r) -> SBT(mirror r,x,mirror l)
;;

let rec replace_when f b1 b2 = if is_leaf b1
  then if f (root b1) then b2 else b1
  else if f (root b1) then b2 else SBT(replace_when f (left_b b1) (b2), root b1, replace_when f (right_b b1) (b2))
;;

let rec cut_below f b1 = if f (root b1) || is_leaf b1 then Leaf(root b1) else SBT(cut_below f (left_b b1), root b1, cut_below f (right_b b1))
;;

let rec to_bin = function
    Leaf(x) -> BinTree.leaftree x
  | SBT(l,x,r) -> let arbol=BinTree.leaftree x in let arbol1=BinTree.left_replacement arbol (to_bin l) in BinTree.right_replacement arbol1 (to_bin r)
;;

let rec from_bin_aux b = 
    if BinTree.is_empty (BinTree.left_b b) && BinTree.is_empty (BinTree.right_b b)
    then Leaf(BinTree.root b)
    else if BinTree.is_empty (BinTree.left_b b) || BinTree.is_empty (BinTree.right_b b) then raise (Failure "from_bin") else SBT(from_bin_aux(BinTree.left_b b),BinTree.root b,from_bin_aux(BinTree.right_b b))
;;

let from_bin b=if BinTree.is_empty b then raise (Failure "from_bin") else from_bin_aux b;;
