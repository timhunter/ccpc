open Util

type backpointer = item option * item option
and item = ParseItem of string * ((range_item * range_item) list) * backpointer option (*range_item defined in Util*) 
type t = Table of (item, string) Hashtbl.t 

let get_nonterm = function ParseItem(nt, _,_) -> nt
 let create_item str ranges bp = ParseItem(str, ranges, bp)
 	let get_ranges = function ParseItem(_, rs,_) -> rs
   
			let get_backpointer = function ParseItem(_,_,bp) -> bp

	let to_string item =
			let ParseItem (nt, ranges, bp) = item in
			let range_strings = map_tr (fun (p,q) -> match (p,q) with 
																									| (EpsVar, EpsVar) -> Printf.sprintf "(Epsilon, Epsilon)"
																									| (RangeVal a, RangeVal b) -> Printf.sprintf "(%d,%d)" a b  
																									| _ -> failwith "Should never mix RangeVal and EpsVar!") ranges in 
			  Printf.sprintf "'{%s, %s}'" nt (List.fold_left (^) "" range_strings) 
let create i = Table (Hashtbl.create i)
let add s elt =
	match s with 
		Table t -> Hashtbl.add t elt " "
let mem s elt =
	match s with 
		Table t -> Hashtbl.mem t elt 
let length s =
	match s with
		Table t -> Hashtbl.length t
let iter f s = 
	match s with 
		Table t -> Hashtbl.iter f t
let find s elt =
	match s with 
		Table t -> Hashtbl.find t elt
let fold f s a = 
	match s with 
		Table t -> Hashtbl.fold f t a

