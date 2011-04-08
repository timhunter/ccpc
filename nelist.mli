(***************************)
(*** Non-empty list type ***)

		type 'a t
		val cons : 'a -> 'a t -> 'a t
		val length : 'a t -> int
		val map : ('a -> 'b) -> 'a t -> 'b t
		val fold : ('a -> 'a -> 'a) -> 'a t -> 'a
		val from_list : 'a list -> 'a t
		val to_list : 'a t -> 'a list
		val nth : 'a t -> int -> 'a

