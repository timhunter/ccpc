open Util
		type 'a t = NonEmptyList of ('a * ('a list))

		let cons x (NonEmptyList (y,ys)) = NonEmptyList (x, y::ys)
		let length (NonEmptyList (x,xs)) = 1 + List.length xs

		let map f (NonEmptyList (x,xs)) = NonEmptyList(f x, map_tr f xs)

		let fold f (NonEmptyList (x,xs)) = List.fold_left f x xs
	
		let fold_l f a (NonEmptyList (x,xs)) = List.fold_left f a (x::xs)

	  let from_list lst =
			match lst with
			| [] -> raise EmptyListException
			| (x::xs) -> NonEmptyList(x,xs)
		let to_list (NonEmptyList(x,xs)) = x::xs
		let nth (NonEmptyList (x,xs)) i  =
		  match i with
		    | 0 -> x
		    | _ -> List.nth xs (i-1)
