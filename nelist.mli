
(** Non-empty lists. *)

exception EmptyListException

(** A type of non-empty lists of elements of type ['a]. *)
type 'a t

val cons : 'a -> 'a t -> 'a t
val length : 'a t -> int
val map : ('a -> 'b) -> 'a t -> 'b t
val nth : 'a t -> int -> 'a

val to_list : 'a t -> 'a list

(** Raises [EmptyListException] if the argument list is empty. *)
val from_list : 'a list -> 'a t

(** Analogous to [List.fold_left]. *)
val fold_l : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** Analogous to [List.fold_left] but with no "initialiser", since we know that the first argument is non-empty. *)
val fold : ('a -> 'a -> 'a) -> 'a t -> 'a

