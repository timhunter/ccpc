open Util

type item = ParseItem of string * (range list)  (*range defined in Util*) 

type route = (item list) * Rule.r * weight

type chart = TableWithHistory of (item, route) Hashtbl.t

type item_route_status = NewItem | OldItemOldRoute | OldItemNewRoute

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
                | Pair (x,y) -> get' t ((find_words (x,y))::acc)
                | VarRange _ -> get' t acc
    in
  List.flatten (get' range_list [])

  

let to_string item sentence =
  let ParseItem (nt, ranges) = item in
  let words = get_string sentence ranges in
  Printf.sprintf "'{%s, %s}'" nt (List.fold_left (fun x y -> x ^ (y ^ " ")) "" words) 

let debug_str item =
	let ParseItem (nt, ranges) = item in
	let show_range r =
		match r with
		| Pair (x,y) -> Printf.sprintf "%d:%d" x y
		| VarRange _ -> Printf.sprintf "eps"
	in
	("[" ^^ nt ^^ (List.fold_left (^^) "" (map_tr show_range ranges)) ^^ "]")

let debug_str_long item chart =
	let ParseItem (nt, ranges) = item in
	let show_range r =
		match r with
		| Pair (x,y) -> Printf.sprintf "%d:%d" x y
		| VarRange _ -> Printf.sprintf "eps"
	in
	let show_backpointer (items,r,wt) = "(" ^ (String.concat "," (map_tr (fun i -> string_of_int (Hashtbl.hash i)) items)) ^ ")" in
	let backpointers_str = List.fold_left (^^) "" (map_tr show_backpointer (get_routes item chart)) in
	("[" ^^ (string_of_int (Hashtbl.hash item)) ^^ nt ^^ (List.fold_left (^^) "" (map_tr show_range ranges)) ^^ backpointers_str ^^ "]")

let create i = TableWithHistory (Hashtbl.create i)

let add c item route =
  match c with
  | TableWithHistory tbl ->
    Hashtbl.add tbl item route

let get_status c item route =
  match (get_routes item c) with
  | [] -> NewItem
  | rs -> if (List.mem route rs) then OldItemOldRoute else OldItemNewRoute

let goal_items c (start_symbol : string) (length : int) : (item list) =
  let check_item (i : item)  _ (acc : item list) : item list =
    if (get_nonterm i = start_symbol) && (get_ranges i = [Pair (0,length)]) then
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
