open Util
open Rational

type backpointer = item option * item option
and item = ParseItem of string * ((range_item * range_item) list) * backpointer option * Rational.rat  (*range_item defined in Util*) 
type t = Table of (item, string) Hashtbl.t 

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

let create i = Table (Hashtbl.create i)
let get_tbl cht =
  match cht with 
    Table t -> t

let add s elt =
 Hashtbl.add (get_tbl s) elt " "

let mem s elt =
  Hashtbl.mem (get_tbl s) elt 
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

