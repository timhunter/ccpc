type 'a tree = Node of 'a * 'a tree list
val generate : string -> (string tree * float)
val write_tree : string tree -> string -> unit
