let left_rotation t =
    match t with
    | Node(v1,l,r) ->
    begin
        match r with
        | Node(v2,rl,rr) -> Node(v2,Node(v1,l,rl),rr)
        | Leaf -> r
    end
    | Leaf -> Leaf
;;

let right_rotation t =
    match t with
    | Node(v1,l,r) ->
    begin
        match l with
        | Node(v2,ll,lr) -> Node(v2,ll,Node(v1,lr,r))
        | Leaf -> l
    end
    | Leaf -> Leaf
;;

let lf_rt_rotation t = 
    match t with
    | Node(v1,l,r) ->
        let rot1 = left_rotation l in
        right_rotation (Node(v1,rot1,r))
    | Leaf -> Leaf
;;

let rt_lf_rotation t = 
    match t with
    | Node(v1,l,r) ->
        let rot1 = right_rotation r in
        left_rotation (Node(v1,l,rot1))
    | Leaf -> Leaf
;;