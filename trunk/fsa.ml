open Util

type fsa = Prefix of (string list) | Infix of (string list) | Sentence of (string list)
type range = Range of fsa * ((int * int) option)

exception RangesNotAdjacentException

let get_consumed_span (Range(input,span)) = span

let end_state fsa =
    match fsa with
    | Infix ws    -> List.length ws
    | Prefix ws   -> List.length ws
    | Sentence ws -> List.length ws

let goal_span fsa = Some(0, end_state fsa)

let find_arcs fsa str =
    let indices =
        match fsa with
        | Infix ws    -> find_in_list str ws
        | Prefix ws   -> find_in_list str ws
        | Sentence ws -> find_in_list str ws
    in
    map_tr (fun i -> (i,i+1)) indices

(* Are we able to use an epsilon transition from i to i? *)
(* Note that this is only intended to accommodate epsilons that are "hidden in the input". 
   For example, if we're parsing a prefix of length n, then this function will return 
   FALSE for the n-to-n transition; while it's true that an epsilon could cover the n-to-n 
   span, this is taken care of by the general fact that anything can cover the n-to-n span 
   when we're parsing a prefix of length n. *)
(* In other words: is the transition from i to i one that can ONLY emit an epsilon? *)
let epsilon_transition_possible input i =
    match input with
        | Infix ws    -> let len = List.length ws in (i >  0 && i <  len)       (* true for all states except the first and last one *)
        | Prefix ws   -> let len = List.length ws in (i >= 0 && i <  len)       (* true for all states except the last one *)
        | Sentence ws -> let len = List.length ws in (i >= 0 && i <= len)       (* true for all states *)

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


