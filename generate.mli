type 'a tree = Node of 'a * 'a tree list
val generate : string -> (string tree * Num.num) list
