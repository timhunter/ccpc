type input= Prefix of (string list) | Infix of (string list) | Sentence of (string list)
type prim

val deduce: int -> Rule.r list -> input -> Chart.chart
val is_goal: string -> int -> Chart.item -> bool 

