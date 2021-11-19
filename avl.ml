let rec append_bst node t =
    match t with
        | Node(v,t1,t2) when node<v -> Node(v,append_bst node t1,t2)
        | Node(v,t1,t2) when node>v -> Node(v,t1,append_bst node t2)
        | Node(v,t1,t2) when node=v -> t
        | Leaf -> Node(node,Leaf,Leaf)
        | _ -> raise Impossible
;;

let rec append_avl node t =
    retrace (append_bst node t)
;;