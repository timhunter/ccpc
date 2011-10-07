type 'a tree = Leaf of 'a | NonLeaf of ('a * 'a tree list * Rule.r)   (* list should never be empty in the NonLeaf case *)
val generate : string -> (string tree * Num.num) list
