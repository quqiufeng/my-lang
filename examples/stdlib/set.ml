type 'a option = None | Some of 'a;

type set = Empty | Node of int * int * set * set;

let rec height = fun m ->
  match m with
  | Empty -> 0
  | Node (_, h, _, _) -> h
in

let max = fun a -> fun b ->
  let a : int = a in
  let b : int = b in
  if a > b then a else b
in

let make_node = fun k -> fun left -> fun right ->
  let h = 1 + max (height left) (height right) in
  Node (k, h, left, right)
in

let rotate_right = fun m ->
  match m with
  | Node (k, _, Node (lk, _, ll, lr), right) ->
      make_node lk ll (make_node k lr right)
  | _ -> m
in

let rotate_left = fun m ->
  match m with
  | Node (k, _, left, Node (rk, _, rl, rr)) ->
      make_node rk (make_node k left rl) rr
  | _ -> m
in

let balance_factor = fun m ->
  match m with
  | Empty -> 0
  | Node (_, _, left, right) -> height left - height right
in

let balance = fun k -> fun left -> fun right ->
  let m = make_node k left right in
  let bf = balance_factor m in
  if bf > 1 then
    match left with
    | Node _ ->
        if balance_factor left >= 0 then
          rotate_right m
        else
          let left_prime = rotate_left left in
          rotate_right (make_node k left_prime right)
    | _ -> m
  else if bf < -1 then
    match right with
    | Node _ ->
        if balance_factor right <= 0 then
          rotate_left m
        else
          let right_prime = rotate_right right in
          rotate_left (make_node k left right_prime)
    | _ -> m
  else
    m
in

let rec insert = fun k -> fun m ->
  match m with
  | Empty -> make_node k Empty Empty
  | Node (k2, _, left, right) ->
      if k = k2 then m
      else if k < k2 then balance k2 (insert k left) right
      else balance k2 left (insert k right)
in

let rec min_binding = fun m ->
  match m with
  | Empty -> None
  | Node (k, _, Empty, _) -> Some k
  | Node (_, _, left, _) -> min_binding left
in

let rec remove_min = fun m ->
  match m with
  | Empty -> Empty
  | Node (_, _, Empty, right) -> right
  | Node (k, _, left, right) ->
      balance k (remove_min left) right
in

let rec remove = fun k -> fun m ->
  match m with
  | Empty -> Empty
  | Node (k2, _, left, right) ->
      if k = k2 then
        match right with
        | Empty -> left
        | _ ->
            match min_binding right with
            | Some k3 ->
                balance k3 left (remove_min right)
            | None -> left
      else if k < k2 then
        balance k2 (remove k left) right
      else
        balance k2 left (remove k right)
in

let rec mem = fun k -> fun m ->
  match m with
  | Empty -> false
  | Node (k2, _, left, right) ->
      if k = k2 then true
      else if k < k2 then mem k left
      else mem k right
in

let s = Empty in
let s = insert 10 s in
let s = insert 5 s in
let s = remove 5 s in
if mem 5 s then "found" else "not found"
