open Util
open Rational

type backpointer = item ref option * item ref option
and item = ParseItem of string * ((range_item * range_item) list) * backpointer option * Rational.rat  (*range_item defined in Util*) 
type chart = Table of (item, unit) Hashtbl.t 

let get_nonterm = function ParseItem(nt, _,_,_) -> nt

let create_item str ranges bp weight = ParseItem(str, ranges, bp, weight)

let get_ranges = function ParseItem(_, rs,_,_) -> rs
   
   
let get_backpointer = function ParseItem(_,_,bp,_) -> bp

let get_string sentence range_list =
  let find_words (first, last) =
    let rec find' index acc =
      if index= last then acc
      else
        find' (index+1) ((List.nth sentence index)::acc) in
    ["("]@(List.rev  (find' first []))@[")"] in
  let rec get' lst acc =
    match lst with 
      [] -> acc
    | h::t -> match h with 
                  (RangeVal x, RangeVal y) -> get' t ((find_words (x,y))::acc)
                | (EpsVar, EpsVar) -> get' t acc 
                | _ -> failwith "Should not mix EpsVar with RangeVal" in
  List.flatten (get' range_list [])

  

let to_string item sentence =
  let ParseItem (nt, ranges, _,_) = item in
  let words = get_string sentence ranges in
  Printf.sprintf "'{%s, %s}'" nt (List.fold_left (fun x y -> x ^ (y ^ " ")) "" words) 

let debug_str item =
	let ParseItem (nt, ranges, bps, _) = item in
	let show_range r =
		match r with
		| (RangeVal x, RangeVal y) -> Printf.sprintf "%d:%d" x y
		| (EpsVar, EpsVar)         -> Printf.sprintf "eps"
		| _ -> failwith "Should not mix EpsVar with RangeVal"
	in
	let show_bps bps =
		match bps with
		| Some (Some r1, Some r2) -> Printf.sprintf "%s %s" (get_nonterm !r1) (get_nonterm !r2)
		| Some (Some r1, None)    -> Printf.sprintf "%s" (get_nonterm !r1)
		| Some (None,    Some r2) -> Printf.sprintf "%s" (get_nonterm !r2)
		| _                       -> Printf.sprintf ""
	in
	("[" ^^ nt ^^ (List.fold_left (^^) "" (map_tr show_range ranges)) ^^ "|" ^^ (show_bps bps) ^^ "]")

let create i = Table (Hashtbl.create i)
let get_tbl cht =
  match cht with 
    Table t -> t

let add s elt =
 Hashtbl.add (get_tbl s) elt ()

let mem s elt =
  Hashtbl.mem (get_tbl s) elt 
let length s =
  Hashtbl.length (get_tbl s)
let iter f s = 
  Hashtbl.iter f (get_tbl s)
let find s elt =
  Hashtbl.find (get_tbl s) elt
let fold f s a = 
  Hashtbl.fold f (get_tbl s) a

