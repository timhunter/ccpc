open Util

type state = int

type string_construal = Infix | Prefix | Exact

type fsa = StringBased of (string_construal * string list)
         | FileBased of (string * state * state * ((state * state), (string * float)) Hashtbl.t)

type range = Range of fsa * ((state * state) option)

exception RangesNotAdjacentException

exception FileBasedNotYetImplementedException

let make_fsa_infix s  = StringBased(Infix,  (Str.split (Str.regexp_string " ") s))
let make_fsa_prefix s = StringBased(Prefix, (Str.split (Str.regexp_string " ") s))
let make_fsa_exact s  = StringBased(Exact,  (Str.split (Str.regexp_string " ") s))
let is_exact = function StringBased(Exact,_) -> true | _ -> false

let make_fsa_from_file filename =
    let channel = try open_in filename
                  with Sys_error _ -> failwith (Printf.sprintf "Couldn't open FSA file %s" filename)
    in
    let tbl = Hashtbl.create 100 in
    let start_state = ref None in
    let end_state = ref None in
    begin
        try
            while true; do
                let line = input_line channel in
                let fields = Str.split (Str.regexp_string "\t") line in
                match fields with
                | [state1_str; state2_str; word_str; weight_str] ->
                    let (state1, state2, weight) =
                        try (int_of_string state1_str, int_of_string state2_str, float_of_string weight_str)
                        with Failure _ -> failwith (Printf.sprintf "Bad line (0) in FSA file %s: %s" filename line)
                    in
                        if (!start_state = None) then (start_state := Some state1) ;     (* state1 of first line is the start state *)
                        Hashtbl.add tbl (state1,state2) (word_str,weight)
                | [state_str] ->
                    let state = try (int_of_string state_str)
                                with Failure _ -> failwith (Printf.sprintf "Bad line (1) in FSA file %s: %s" filename line)
                    in
                        if (!end_state = None) then (end_state := Some state) ;     (* state on its own on a line is the end state *)
                | _ -> failwith (Printf.sprintf "Bad line (2) in FSA file %s: %s" filename line)
            done
        with End_of_file -> close_in channel
    end ;
    match (!start_state, !end_state) with
    | (Some x, Some y) -> FileBased(filename, x, y, tbl)
    | (None, _) -> failwith (Printf.sprintf "Couldn't determine start state in FSA file %s" filename)
    | _         -> failwith (Printf.sprintf "Couldn't determine end state in FSA file %s" filename)

let string_of n = string_of_int n

let get_consumed_span (Range(input,span)) = span

let start_state fsa = 0

let end_state fsa =
    match fsa with
    | StringBased(_,ws) -> List.length ws
    | FileBased _       -> raise FileBasedNotYetImplementedException

let goal_span fsa = Some(0, end_state fsa)

let find_arcs fsa str =
    match fsa with
    | StringBased(_,ws) -> map_tr (fun i -> (i,i+1)) (find_in_list str ws)
    | FileBased _       -> raise FileBasedNotYetImplementedException

let axiom_spans fsa str =
    let len = end_state fsa in
    let string_independent_results =    (* Axiom spans that we allow because of the structure of the FSA, not dependent at all on the string *)
        match fsa with
        | StringBased(Infix,_)  -> [Some (len,len); Some (0,0)]
        | StringBased(Prefix,_) -> [Some (len,len)]
        | StringBased(Exact,_)  -> []
        | FileBased _           -> raise FileBasedNotYetImplementedException
    in
    let string_dependent_results =
        match (str, fsa) with
        | (" ", StringBased(Infix,_))  -> if (len > 1) then [None] else []
        | (" ", StringBased(Prefix,_)) -> if (len > 0) then [None] else []
        | (" ", StringBased(Exact,_))  -> [None]
        | (_,   StringBased _)         -> map_tr (fun (i,j) -> Some (i,j)) (find_arcs fsa str)
        | (_, FileBased _)             -> raise FileBasedNotYetImplementedException
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
    | StringBased(form,ws) ->
        begin
        let len = List.length ws in
        match form with
        | Infix  -> (i >  0 && i <  len)       (* true for all states except the first and last one *)
        | Prefix -> (i >= 0 && i <  len)       (* true for all states except the last one *)
        | Exact  -> (i >= 0 && i <= len)       (* true for all states *)
        end
    | FileBased _ -> raise FileBasedNotYetImplementedException

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

let symbol_on_arc fsa (i,j) =
    match fsa with
    | StringBased(form,ws) ->
        if (i+1 != j) then
            None
        else
            Some (List.nth ws i)
    | FileBased _ -> raise FileBasedNotYetImplementedException

let description fsa =
    match fsa with
    | StringBased(Infix,ws)  -> Printf.sprintf "infix: %s" (String.concat " " ws)
    | StringBased(Prefix,ws) -> Printf.sprintf "prefix: %s" (String.concat " " ws)
    | StringBased(Exact,ws)  -> Printf.sprintf "exact string: %s" (String.concat " " ws)
    | FileBased _ -> raise FileBasedNotYetImplementedException

let index_of x = x
