let rec check_balance t =
    match t with
    | Node(_,t1,t2) ->
        let diff = abs ((height t1) - (height t2)) in
        (diff < 2) && (check_balance t1) && (check_balance t2)
    | Leaf -> true
;;

let check_balance_factor t =
    match t with
    | Node(_,t1,t2) -> 
        let diff = (height t2) - (height t1) in
        print_int(diff);
        print_newline();
        diff
    | Leaf -> 0
;;