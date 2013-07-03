type 'a tree = Leaf of 'a | NonLeaf of ('a * 'a tree list * Rule.r)   (* list should never be empty in the NonLeaf case *)
val generate : string -> (string tree * Util.weight) list
val get_sentence: string tree -> string list
val write_tree : string tree -> string -> unit
