type 'a bintree = Empty | BT of 'a bintree * 'a * 'a bintree;;

type 'a t = 'a bintree;;

let empty = Empty;;

let is_empty b = b=Empty;;

let leaftree n = BT(Empty,n,Empty);;

let root = function
    Empty -> raise (Failure "root")
  | BT(_,r,_) -> r
;;

let left_b = function
    Empty -> raise (Failure "left_b")
  | BT(r,_,_) -> r
;;

let right_b = function
    Empty -> raise (Failure "right_b")
  | BT(_,_,r) -> r
;;

let root_replacement b n = 
    match b with
        Empty -> raise (Failure "root_replacement")
      | BT(l,x,r) -> BT(l,n,r)
;;

let left_replacement b b2 = 
    match b with
        Empty -> raise (Failure "left_replacement")
      | BT(l,x,r) -> BT(b2,x,r)
;;

let right_replacement b b2 = 
    match b with
        Empty -> raise (Failure "right_replacement")
      | BT(l,x,r) -> BT(l,x,b2)
;;

let rec size = function
    Empty -> 0
  | BT(l,x,r) -> 1+(size l)+(size r)
;;

let rec height = function
    Empty -> 0
  | BT(l,x,r) -> 1+max (height l) (height r)
;;

let rec preorder = function
    Empty -> []
  | BT(l,x,r) -> [x] @ (preorder l) @ (preorder r)
;;

let rec inorder = function
    Empty -> []
  | BT(l,x,r) -> (inorder l) @ [x] @ (inorder r)
;;

let rec postorder = function
    Empty -> []
  | BT(l,x,r) -> (postorder l) @ (postorder r) @ [x]
;;

let breadth a =
  let rec aux cola2 = function
      [] -> if cola2=[] then [] else aux [] (List.rev cola2)
    | Empty::t -> aux cola2 t
    | BT (l,x,r) :: t -> x :: aux (r::l::cola2) (t)  (* ineficiente *)
  in aux [] [a]
;;

let rec leaves = function
    Empty -> []
  | BT(Empty,n,Empty) -> [n]
  | BT(l,n,r) -> (leaves l) @ (leaves r)
;;

let rec find_in_depth_aux f b = if is_empty b
                            then None
                            else if f (root b) 
                            then  Some (root b)
                            else let find_l = find_in_depth_aux f (left_b b) in 
                                 if find_l <> None 
                                 then find_l
                                 else let find_r = find_in_depth_aux f (right_b b) in
                                                       if find_r <> None
                                                       then find_r
                                                       else None
;;

let find_in_depth f b = let solucion = find_in_depth_aux f b in if solucion = None then raise Not_found else Option.get(solucion);;

let find_in_depth_opt = find_in_depth_aux;;

let rec exists f b = (not (is_empty b)) && (f (root b) || exists f (left_b b) || exists f (right_b b));;

let rec for_all_aux f b = (not (is_empty b)) && (f (root b) && for_all_aux f (left_b b) && for_all_aux f (right_b b)) || is_empty b;;
let for_all f b = not (is_empty b) && for_all_aux f b;;

let rec map f b = if is_empty b
  then Empty
  else BT(map f (left_b b), f(root b),map f (right_b b))
;;

let rec mirror = function
    Empty -> Empty
  | BT(l,x,r) -> BT(mirror r,x,mirror l)
;;

let rec replace_when f b1 b2 = if is_empty b1
  then Empty
  else if f (root b1) then b2 else BT(replace_when f (left_b b1) b2, root b1, replace_when f (right_b b1) b2)
;;

let rec cut_above f b1 = if is_empty b1
  then Empty
  else if f (root b1) then Empty else BT(cut_above f (left_b b1), root b1, cut_above f (right_b b1))
;;

let rec cut_below f b1 = if is_empty b1
  then Empty
  else if f (root b1) then BT(Empty,root b1,Empty) else BT(cut_below f (left_b b1), root b1, cut_below f (right_b b1))
;;