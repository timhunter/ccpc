
(** A collection of miscellaneous utility functions. *)

(** [take n xs] is the length-[n] prefix of [xs], or [xs] itself if its length is less than [n]. *)
val take : int -> 'a list -> 'a list

(** [take_while p xs] is the longest prefix (possibly empty) of [xs] containing only elements that satisfy [p]. *)
val take_while : ('a -> bool) -> 'a list -> 'a list

(** [optlistmap f as] collects only the non-[None] elements of [map f as]. *)
val optlistmap : ('a -> 'b option) -> 'a list -> 'b list

(** Returns [Some xs] if every element of the argument list is of the form [Some x]; otherwise returns [None]. *)
val require_no_nones : ('a option) list -> ('a list) option

(** [range a b] is the list [\[a; a+1; ...; b-1\]].*)
val range : int -> int -> int list

(** Concatenates two strings with a space in between them. *)
val (^^) : string -> string -> string

(** Returns the positions at which the given element is appears in the given list. *)
val find_in_list : 'a -> 'a list -> int list

(** [split c str] splits the string [str] at the delimiter character [c]. *)
val split : char -> string -> string list

(** Removes duplicates from a list. The optional argument [eq] is an equality function (if not present, [=] is used. *)
val uniques : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list

(** [show_list f xs] constructs a string representation of the list [xs], using [f] to generate a string representation for each element. 
    For example, [show_list string_of_int \[3;4;5\]] returns the string ["\[3;4;5\]"]. *)
val show_list : ('a -> string) -> 'a list -> string

(** {2 Tail-recursive versions of standard functions} *)

val reverse_tr : 'a list -> 'a list
val map_tr : ('a -> 'b) -> 'a list -> 'b list
val concatmap_tr: ('a -> 'b list) -> 'a list -> 'b list
val append_tr : 'a list -> 'a list -> 'a list

(** {2 Weights} *)

(** A [weight] is either an exact-fraction weight (for use with weighted grammars) 
    or the distinguished value [no_weight] (for use with unweighted grammars). *)
type weight
val no_weight : weight
val weight_one : weight
val make_weight : Big_int.big_int -> Big_int.big_int -> weight
val weight_from_float : float -> weight
val float_of_weight : weight -> float
val show_weight : weight -> string
val show_weight_float : weight -> string
val mult_weights : weight -> weight -> weight
val add_weights : weight -> weight -> weight
val compare_weights : weight -> weight -> int
val weighted_random : ('a * weight) list -> 'a
