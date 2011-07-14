open Util
open Rational

type item = ParseItem of string * ((range_item * range_item) list)  (*range_item defined in Util*) 

type route = (item list) * Rule.r * Rational.rat option

type chart = TableWithHistory of (item, route) Hashtbl.t

let get_nonterm = function ParseItem(nt,_) -> nt

let create_item str ranges = ParseItem(str, ranges)

let get_ranges = function ParseItem(_,rs) -> rs

let get_routes prop c =
  match c with
  | TableWithHistory tbl -> Hashtbl.find_all tbl prop

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
  let ParseItem (nt, ranges) = item in
  let words = get_string sentence ranges in
  Printf.sprintf "'{%s, %s}'" nt (List.fold_left (fun x y -> x ^ (y ^ " ")) "" words) 

let debug_str item =
	let ParseItem (nt, ranges) = item in
	let show_range r =
		match r with
		| (RangeVal x, RangeVal y) -> Printf.sprintf "%d:%d" x y
		| (EpsVar, EpsVar)         -> Printf.sprintf "eps"
		| _ -> failwith "Should not mix EpsVar with RangeVal"
	in
	("[" ^^ nt ^^ (List.fold_left (^^) "" (map_tr show_range ranges)) ^^ "]")

let create i = TableWithHistory (Hashtbl.create i)

let add c item route =
  match c with
  | TableWithHistory tbl ->
    Hashtbl.add tbl item route

let mem c item =
  match c with
  | TableWithHistory tbl -> Hashtbl.mem tbl item

let mem_route c item route =
  if not (mem c item) then
    failwith "mem_route: Asked about a route for an *item* we don't have"
  else
    match c with
    | TableWithHistory tbl -> List.mem route (Hashtbl.find_all tbl item)

let goal_items c (start_symbol : string) (length : int) : (item list) =
  let check_item (i : item)  _ (acc : item list) : item list =
    if (get_nonterm i = start_symbol) && (get_ranges i = [(RangeVal 0, RangeVal length)]) then
      i::acc
    else
      acc
  in
  match c with
  | TableWithHistory tbl -> Hashtbl.fold check_item tbl []

(*** WARNING: Functions below here are very slow. Not recommended outside of debugging contexts. ***)

let all_items c =
  let (TableWithHistory tbl) = c in
  let t = Hashtbl.create 10000 in
  let add_if_new x _ acc =
    let is_new = not (Hashtbl.mem t x) in
    if is_new then (
      Hashtbl.add t x () ;
      x::acc
    ) else (
      acc
    )
  in
  (** reverse just for backwards compatibility with earlier debugging output *)
  reverse_tr (Hashtbl.fold add_if_new tbl [])

let length c = List.length (all_items c)

let iter_items c f = List.iter f (all_items c)

let map_items c f = List.map f (all_items c)
