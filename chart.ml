open Util
open Rational

type item = ParseItem of string * ((range_item * range_item) list)  (*range_item defined in Util*) 

type route = (item list) * Rational.rat option

type chart = Table of (item, unit) Hashtbl.t | TableWithHistory of (item, route list) Hashtbl.t

let get_nonterm = function ParseItem(nt,_) -> nt

let create_item str ranges = ParseItem(str, ranges)

let get_ranges = function ParseItem(_,rs) -> rs

let get_routes prop c =
  match c with
  | Table tbl -> failwith "No histories stored in this table"
  | TableWithHistory tbl -> Hashtbl.find tbl prop

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

let create i with_history =
  if with_history then
    TableWithHistory (Hashtbl.create i)
  else
    Table (Hashtbl.create i)

let add c item route =
  match c with
  | Table tbl -> Hashtbl.replace tbl item ()
  | TableWithHistory tbl ->
    if Hashtbl.mem tbl item then
      let existing = Hashtbl.find tbl item in
      Hashtbl.replace tbl item (route::existing)
    else
      Hashtbl.add tbl item [route]

let mem c item =
  match c with
  | Table tbl -> Hashtbl.mem tbl item
  | TableWithHistory tbl -> Hashtbl.mem tbl item

let mem_route c item route =
  if not (mem c item) then
    failwith "mem_route: Asked about a route for an *item* we don't have"
  else
    match c with
    | Table tbl -> true (* There's never a new route to an existing item in this kind of table *)
    | TableWithHistory tbl -> List.mem route (Hashtbl.find tbl item)
  
let length c =
  match c with
  | Table tbl -> Hashtbl.length tbl
  | TableWithHistory tbl -> Hashtbl.length tbl

let goal_items c (start_symbol : string) (length : int) : (item list) =
  let check_item (i : item)  _ (acc : item list) : item list =
    if (get_nonterm i = start_symbol) && (get_ranges i = [(RangeVal 0, RangeVal length)]) then
      i::acc
    else
      acc
  in
  match c with
  | Table tbl -> Hashtbl.fold check_item tbl []
  | TableWithHistory tbl -> Hashtbl.fold check_item tbl []

let item_list c =
  match c with
  | Table tbl -> Hashtbl.fold (fun i _ acc -> i::acc) tbl []
  | TableWithHistory tbl -> Hashtbl.fold (fun i _ acc -> i::acc) tbl []
