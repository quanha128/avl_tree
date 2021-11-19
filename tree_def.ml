type 'a tree =
    Node of 'a * 'a tree * 'a tree
    | Leaf;;

exception BadBalanceFactor
exception Impossible