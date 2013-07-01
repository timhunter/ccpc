open Util
open Fsa

type item = ParseItem of string * (range list)

(* Provide our own equality function for use by item-keyed hashtables *)
module ItemHashtbl = Hashtbl.Make(
    struct
        type t = item
        let rec lists_equal eq list1 list2 =
            match (list1,list2) with
            | ([], [])       -> true
            | (x::xs, [])    -> false
            | ([], y::ys)    -> false
            | (x::xs, y::ys) -> if (eq x y) then (lists_equal eq xs ys) else false
        let equal item1 item2 =
            match (item1,item2) with (ParseItem(s1,ranges1), ParseItem(s2,ranges2)) ->
                (s1 = s2) && (lists_equal ranges_equal ranges1 ranges2)
        let hash = Hashtbl.hash
    end
)

type route = (item list) * Rule.r * weight

type chart = TableWithHistory of route ItemHashtbl.t

type item_route_status = NewItem | OldItemOldRoute | OldItemNewRoute

let get_nonterm = function ParseItem(nt,_) -> nt

let create_item str ranges = ParseItem(str, ranges)

let get_ranges = function ParseItem(_,rs) -> rs

let get_routes prop c =
  match c with
  | TableWithHistory tbl -> ItemHashtbl.find_all tbl prop

let debug_str item =
	let ParseItem (nt, ranges) = item in
	let show_range r =
		match (get_consumed_span r) with
		| Some (x,y) -> Printf.sprintf "%s:%s" (Fsa.string_of x) (Fsa.string_of y)
		| None -> Printf.sprintf "eps"
	in
	("[" ^^ nt ^^ (List.fold_left (^^) "" (map_tr show_range ranges)) ^^ "]")

let debug_str_long item chart =
	let ParseItem (nt, ranges) = item in
	let show_range r =
		match (get_consumed_span r) with
		| Some (x,y) -> Printf.sprintf "%s:%s" (Fsa.string_of x) (Fsa.string_of y)
		| None -> Printf.sprintf "eps"
	in
	let show_backpointer (items,r,wt) = show_weight wt ^^ (show_weight (Rule.get_weight r)) ^^ ("(" ^ (String.concat "," (map_tr (fun i -> string_of_int (Hashtbl.hash i)) items)) ^ ")") in
	let backpointers_str = List.fold_left (^^) "" (map_tr show_backpointer (get_routes item chart)) in
	("[" ^^ (string_of_int (Hashtbl.hash item)) ^^ nt ^^ (List.fold_left (^^) "" (map_tr show_range ranges)) ^^ backpointers_str ^^ "]")

let compare_items i1 i2 =
        compare (debug_str i1) (debug_str i2)

let create i = TableWithHistory (ItemHashtbl.create i)

let add c item route =
  match c with
  | TableWithHistory tbl ->
    ItemHashtbl.add tbl item route

let get_status c item route =
  match (get_routes item c) with
  | [] -> NewItem
  | rs -> if (List.mem route rs) then OldItemOldRoute else OldItemNewRoute

(* This function is kind of a remnant from the bad old days when an item included a 
 * list of backpointers. Now that an item is just a ``proposition'', it makes more 
 * sense to just use the below function goal_item, and then call get_routes on the result. *)
let goal_items c (start_symbol : string) fsa : (item list) =
  let check_item (i : item)  _ (acc : item list) : item list =
    if (get_nonterm i = start_symbol) && (map_tr get_consumed_span (get_ranges i) = [Some(start_state fsa, end_state fsa)]) then
      i::acc
    else
      acc
  in
  match c with
  | TableWithHistory tbl -> ItemHashtbl.fold check_item tbl []

let goal_item start_symbol fsa = ParseItem(start_symbol, [Range(fsa, goal_span fsa)])

(*** WARNING: Functions below here are very slow. Not recommended outside of debugging contexts. ***)

let all_items c =
  let (TableWithHistory tbl) = c in
  let t = ItemHashtbl.create 10000 in
  let add_if_new x _ acc =
    let is_new = not (ItemHashtbl.mem t x) in
    if is_new then (
      ItemHashtbl.add t x () ;
      x::acc
    ) else (
      acc
    )
  in
  ItemHashtbl.fold add_if_new tbl []

let length c = List.length (all_items c)

let iter_items c f = List.iter f (all_items c)

let map_items c f = List.map f (all_items c)
