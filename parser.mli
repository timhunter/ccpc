type input= Prefix of (string list) | Sentence of (string list)
type prim

val deduce: int -> Rule.r list -> input -> Chart.item list
val is_goal: input -> Chart.item -> bool 
val get_axioms: Rule.r list -> input -> Chart.item list 

