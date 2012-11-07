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

exception EmptyListException

let rec take n lst =
  if (n = 0) then
    []
  else
    match lst with
    | [] -> []
    | (x::xs) -> x :: (take (n-1) xs)

let rec take_while p lst =
  match lst with
  | [] -> []
  | (x::xs) -> if p x then (x::(take_while p xs)) else []

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

let rec require_no_nones (lst : 'a option list) : 'a list option =
        match lst with
        | [] -> Some []
        | (None :: xs) -> None
        | ((Some x) :: xs) -> match (require_no_nones xs) with | None -> None | Some rest -> Some (x::rest)

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

let rec split sep str =
  try
    let i = String.index str sep in
    String.sub str 0 i ::
      split sep (String.sub str (i+1) (String.length str - i - 1))
  with Not_found ->
    [str] 

let show_list f lst =
        "[" ^ (String.concat ";" (map_tr f lst)) ^ "]"

exception RangesNotAdjacentException

type range = Pair of int * int | VarRange of int * int   (* VarRange (m,n) means (i,i) for any i, m <= i < n *)

let concat_ranges range1 range2 =
	match (range1,range2) with
	| (Pair (i,j), Pair (k,l)) -> if (j = k) then (Pair (i,l)) else (raise RangesNotAdjacentException)
	| (Pair (i,j), VarRange (k,l)) -> if (k <= j && j < l) then (Pair (i,j)) else (raise RangesNotAdjacentException)
	| (VarRange (k,l), Pair (i,j)) -> if (k <= i && i < l) then (Pair (i,j)) else (raise RangesNotAdjacentException)
	| (VarRange (i,j), VarRange (k,l)) -> let ik = max i k in let jl = min j l in if ik < jl then (VarRange (ik,jl)) else (raise RangesNotAdjacentException)

(****************************************************************************)

type weight = (Num.num * Num.num) option

let no_weight = None
let make_weight n1 n2 = Some(n1,n2)

let weight_denominator w =
  match w with
  | Some (_,d) -> Some d
  | None -> None

let weight_numerator w =
  match w with
  | Some (n,_) -> Some n
  | None -> None

let show_weight w =
  match w with
  | Some (x,y) -> Printf.sprintf "%s / %s" (Num.string_of_num x) (Num.string_of_num y)
  | None -> ""

let show_weight_float w =
  match w with
  | Some (x,y) -> string_of_float (Num.float_of_num (Num.div_num x y))
  | None -> ""

let mult_weights w1 w2 =
  match (w1,w2) with
  | (None, None) -> None
  | (Some (n1,d1), Some (n2,d2)) -> Some (Num.mult_num n1 n2, Num.mult_num d1 d2)
  | _ -> Printf.eprintf "WARNING: Multiplying a None weight with a non-None weight!" ; None

let compare_weights w1 w2 =
  match (w1,w2) with
  | (None, None) -> 0
  | (Some (n1,d1), Some (n2,d2)) -> Num.compare_num (Num.div_num n1 d1) (Num.div_num n2 d2)
  | (None, Some (n,d)) -> Printf.eprintf "WARNING: Comparing a None weight with a non-None weight!" ; Num.compare_num (Num.num_of_int 0) (Num.div_num n d)
  | (Some (n,d), None) -> Printf.eprintf "WARNING: Comparing a None weight with a non-None weight!" ; Num.compare_num (Num.div_num n d) (Num.num_of_int 0)
