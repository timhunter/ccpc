
let reverse_tr lst =
	let rec reverse' lst acc =
		match lst with
		| [] -> acc
		| x::xs -> reverse' xs (x::acc)
	in reverse' lst []

let map_tr f lst =
	let rec map' f lst acc =
		match lst with
		  [] -> acc
		| x::xs -> map' f xs ((f x)::acc)
	in reverse_tr (map' f lst [])

let concatmap_tr f lsts =
	let rec concatmap' f lsts acc =
		match lsts with
		| [] -> acc
		| (xs::xss) -> concatmap' f xss ((f xs) @ acc)
	in concatmap' f lsts []


exception EmptyListException

let optlistmap f xs =
	let rec optlistmap' f lst acc =
		match lst with
		  [] -> acc
		| x::xs ->
			match f x with
			  Some y -> optlistmap' f xs (y::acc)
			| None -> optlistmap' f xs acc
	in
	optlistmap' f xs []

let (^^) s t =
	if (s = "") || (t = "") then
		s ^ t
	else
		s ^ " " ^ t

(* Generate all the lists you can, of a given length, using the elements of a given list (maybe more than once) *)
let rec all_lists lst length =
	match (lst,length) with
	  (_,0) -> [[]]
	| ([],_) -> []
	| _ ->
		let lists_starting_with x = map_tr (fun l -> x::l) (all_lists lst (length-1)) in
		concatmap_tr lists_starting_with lst

let uniques lst =
	let rec uniques' checked rest =
		match rest with
		  [] -> checked
		| (x::xs) -> if (List.mem x checked) then (uniques' checked xs) else (uniques' (x::checked) xs)
	in
	reverse_tr (uniques' [] lst)

let range i j =
	let rec range' i j acc =
		if i >= j then
			acc
		else
			range' (i+1) j (i::acc)
	in
	reverse_tr (range' i j [])

let find_in_list target lst =
	let rec find_in_list' target lst n =
		match lst with
		  [] -> []
		| (x::xs) ->
			let rest = find_in_list' target xs (n+1) in
			if (x = target) then (n::rest) else rest
	in
	find_in_list' target lst 0

let rec split sep str =
  try
    let i = String.index str sep in
    String.sub str 0 i ::
      split sep (String.sub str (i+1) (String.length str - i - 1))
  with Not_found ->
    [str] 


exception RangesNotAdjacentException

type range_item = RangeVal of int | EpsVar

let concat_ranges (i,j) (k,l) =
	match (i,j) with 
		| (EpsVar, EpsVar) -> (match (k,l) with
														| (EpsVar, EpsVar) -> (EpsVar, EpsVar)
														| (RangeVal a, RangeVal b) -> (RangeVal a, RangeVal b)
														| _ -> failwith "Should never mix EpsVar with RangeVal")
		| (RangeVal a, RangeVal b) -> (match (k,l) with 
																		| (EpsVar, EpsVar) -> (RangeVal a, RangeVal b)
																		| (RangeVal c, RangeVal d) -> if (b = c) then (RangeVal a, RangeVal d) else (raise RangesNotAdjacentException)
																		| _ -> failwith "Should never mix EpsVar with RangeVal")
		| _ -> failwith "Should never mix EpsVar with RangeVal"

