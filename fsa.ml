
type fsa = Prefix of int | Infix of int | Sentence of int
type range = Range of fsa * ((int * int) option)

exception RangesNotAdjacentException

let get_consumed_span (Range(input,span)) = span

let goal_span fsa =
    match fsa with
    | Infix n -> Some(0,n)
    | Prefix n -> Some(0,n)
    | Sentence n -> Some(0,n)

(* Are we able to use an epsilon transition from i to i? *)
(* Note that this is only intended to accommodate epsilons that are "hidden in the input". 
   For example, if we're parsing a prefix of length n, then this function will return 
   FALSE for the n-to-n transition; while it's true that an epsilon could cover the n-to-n 
   span, this is taken care of by the general fact that anything can cover the n-to-n span 
   when we're parsing a prefix of length n. *)
(* In other words: is the transition from i to i one that can ONLY emit an epsilon? *)
let epsilon_transition_possible input i =
    match input with
        | Infix(len)    -> (i >  0 && i <  len)       (* true for all states except the first and last one *)
        | Prefix(len)   -> (i >= 0 && i <  len)       (* true for all states except the last one *)
        | Sentence(len) -> (i >= 0 && i <= len)       (* true for all states *)

let concat_ranges (Range(input1,span1)) (Range(input2,span2)) =
    assert (input1 = input2) ;
    let input = input1 in
    let new_span =
        match (span1,span2) with
        | (Some(i,j), Some(k,l)) -> if (j = k) then Some(i,l) else (raise RangesNotAdjacentException)
        | (Some(i,j), None)      -> if (epsilon_transition_possible input j) then Some(i,j) else (raise RangesNotAdjacentException)
        | (None, Some(i,j))      -> if (epsilon_transition_possible input i) then Some(i,j) else (raise RangesNotAdjacentException)
        | (None, None)           -> None
    in
    Range(input, new_span)


