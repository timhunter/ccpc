
(** CKY-style MCFG parsing. *)

(** Constructs a completed chart by "parsing" the specified FSA with the specified grammar. *)
val deduce: Rule.r list -> Fsa.fsa -> Chart.chart

