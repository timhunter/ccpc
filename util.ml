
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


(***************************)
(*** Non-empty list type ***)

module type LISTLIKE =
	sig
		type 'a t
		val cons : 'a -> 'a t -> 'a t
		val length : 'a t -> int
		val map : ('a -> 'b) -> 'a t -> 'b t
		val fold : ('a -> 'a -> 'a) -> 'a t -> 'a
		val from_list : 'a list -> 'a t
		val to_list : 'a t -> 'a list
		val nth : 'a t -> int -> 'a
	end ;;

exception EmptyListException
module NEList : LISTLIKE =
	struct

		type 'a t = NonEmptyList of ('a * ('a list))

		let cons x (NonEmptyList (y,ys)) = NonEmptyList (x, y::ys)
		let length (NonEmptyList (x,xs)) = 1 + List.length xs

		let map f (NonEmptyList (x,xs)) = NonEmptyList(f x, map_tr f xs)

		let fold f (NonEmptyList (x,xs)) = List.fold_left f x xs
		let from_list lst =
			match lst with
			| [] -> raise EmptyListException
			| (x::xs) -> NonEmptyList(x,xs)
		let to_list (NonEmptyList(x,xs)) = x::xs
		let nth (NonEmptyList (x,xs)) i  =
		  match i with
		    | 0 -> x
		    | _ -> List.nth xs (i-1)
	end ;;

(***************************)
(***************************)

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

