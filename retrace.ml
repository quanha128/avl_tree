let rec retrace t =
    match t with
    | Node(v,t1,t2) ->
    begin
        match check_balance_factor t with
            | 2 ->
                if check_balance_factor t2 = 1 then left_rotation t
                else rt_lf_rotation t
            | -2 ->
                if check_balance_factor t1 = -1 then right_rotation t
                else lf_rt_rotation t
            | 0 -> t
            | 1 -> Node(v,t1,retrace t2)
            | -1 -> Node(v,retrace t1,t2)
            | _ -> raise BadBalanceFactor
    end
    | Leaf -> t
;;