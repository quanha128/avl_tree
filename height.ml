let rec height t =
    match t with
    | Node(_,t1,t2) -> 1 + max (height t1) (height t2)
    | Leaf -> 0