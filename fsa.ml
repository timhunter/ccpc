open Util

type fsa = Prefix of (string list) | Infix of (string list) | Sentence of (string list)
type range = Range of fsa * ((int * int) option)

exception RangesNotAdjacentException

let make_fsa_prefix s = Prefix s
let make_fsa_infix s = Infix s
let make_fsa_exact s = Sentence s
let is_exact = function Sentence _ -> true | _ -> false

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

let axiom_spans fsa str =
    let len = end_state fsa in
    let string_independent_results =    (* Axiom spans that we allow because of the structure of the FSA, not dependent at all on the string *)
        match fsa with
        | Infix _ -> [Some (len,len); Some (0,0)]
        | Prefix _   -> [Some (len,len)]
        | Sentence _ -> []
    in
    let string_dependent_results =
        match (str, fsa) with
        | (" ", Infix _)    -> if (len > 1) then [None] else []
        | (" ", Prefix _)   -> if (len > 0) then [None] else []
        | (" ", Sentence _) -> [None]
        | _                 -> map_tr (fun (i,j) -> Some (i,j)) (find_arcs fsa str)
    in
    string_dependent_results @ string_independent_results

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

let description fsa =
    match fsa with
    | Infix ws    -> Printf.sprintf "infix: %s" (String.concat " " ws)
    | Prefix ws   -> Printf.sprintf "prefix: %s" (String.concat " " ws)
    | Sentence ws -> Printf.sprintf "exact string: %s" (String.concat " " ws)

