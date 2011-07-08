open Util
open Rational

type proposition = Proposition of string * ((range_item * range_item) list)

type backpointer = item ref option * item ref option
and item = ParseItem of string * ((range_item * range_item) list) * backpointer option * (Rational.rat option)  (*range_item defined in Util*) 

type history = backpointer option * Rational.rat option

(* An item is basically a proposition with a history. *)
(* We don't actually store items ever internally; we store propositions, and (perhaps) a list of histories for each one. *)

type chart = Table of (proposition, unit) Hashtbl.t | TableWithHistory of (proposition, history list) Hashtbl.t

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

let create i with_history =
  if with_history then
    TableWithHistory (Hashtbl.create i)
  else
    Table (Hashtbl.create i)

let add c item =
  let ParseItem(nt, ranges, bp, w) = item in
  let prop = Proposition(nt, ranges) in
  match c with
  | Table tbl -> Hashtbl.replace tbl prop ()
  | TableWithHistory tbl ->
    if Hashtbl.mem tbl prop then
      let existing = Hashtbl.find tbl prop in
      Hashtbl.replace tbl prop ((bp,w)::existing)
    else
      Hashtbl.add tbl prop [(bp,w)]

let mem c item =
  let ParseItem(nt, ranges, bp, w) = item in
  let prop = Proposition(nt,ranges) in
  match c with
  | Table tbl -> Hashtbl.mem tbl prop
  | TableWithHistory tbl -> (Hashtbl.mem tbl prop) && (List.mem (bp,w) (Hashtbl.find tbl prop))

let length c =
  match c with
  | Table tbl -> Hashtbl.length tbl
  | TableWithHistory tbl -> Hashtbl.length tbl

let item_list c =
  match c with
  | Table tbl ->
    let item_from_prop prop =
      match prop with Proposition(nt,ranges) -> ParseItem(nt,ranges,None,None) in
    Hashtbl.fold (fun prop _ items -> (item_from_prop prop)::items) tbl []
  | TableWithHistory tbl ->
    let items_from_prop prop =
      match prop with Proposition(nt,ranges) -> map_tr (fun (bp,w) -> ParseItem(nt,ranges,bp,w)) (Hashtbl.find tbl prop) in
    Hashtbl.fold (fun prop _ items -> (items_from_prop prop)@items) tbl []
