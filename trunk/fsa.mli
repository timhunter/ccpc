
(** Finite-state automata. *)

type fsa
type state

(** A range that can be covered by a CJK-style parsing item. A [range] with a second 
    component of the form [Some(i,j)] indicates coverage of a sequence of transitions from 
    state [i] to state [j]. A [range] with a second component [None] indicates coverage of 
    some sequence of epsilon transitions. *)
type range = Range of fsa * ((state * state) option)

(** See function [concat_ranges]. *)
exception RangesNotAdjacentException

(** {2 Constructing FSAs} *)

(** Constructs an FSA that accepts all and only strings with the given prefix. *)
val make_fsa_prefix : string -> fsa

(** Constructs an FSA that accepts all and only strings with the given infix. *)
val make_fsa_infix : string -> fsa

(** Constructs an FSA that accepts precisely the given string. *)
val make_fsa_exact : string -> fsa

(** Constructs an FSA as specified by the given file. *)
val make_fsa_from_file : string -> fsa

(** {2 Basic FSA functions} *)

(** Returns [true] iff the FSA was constructed by [make_fsa_exact]. *)
val is_exact : fsa -> bool

(** [weight_of_arc fsa (i,j) str] returns the weight of the arc from state [i] to state [j] that emits [str]. This means it returns weight zero if 
    "there is no such arc". *)
val weight_of_arc : fsa -> (state * state) -> string -> Util.weight

val start_state : fsa -> state
val end_state : fsa -> state

val description : fsa -> string

(** {2 States} *)

val string_of : state -> string

(** An arbitrary identifier that is unique for each state of a single FSA. *)
(* Included because the graph-drawing code needs an int for each state. *)
val index_of : state -> int

(** {2 Ranges} *)

val ranges_equal : range -> range -> bool
val get_consumed_span : range -> (state * state) option
val goal_span : fsa -> (state * state) option

(** Computes the sequence of transitions through the FSA that can be "covered" by the given string. *)
val axiom_spans : fsa -> string -> ((state * state) option) list

(** Concatenate two ranges. Both ranges must refer to the same FSA.
    {ul {- Concatenating an [(i,j)] range with a [(k,l)] range produces an [(i,l)] range iff [j = k], otherwise raises [RangesNotAdjacentException].}
        {- Concatenating two [None] ranges produces a [None] range.}
        {- Concatenating a [None] range with a non-[None] range produces the non-[None] range iff there is not a "star-loop" at its edge to which 
           the [None] range if being concatenated, otherwise raises [RangesNotAdjacentException].}} *)
val concat_ranges : range -> range -> range

