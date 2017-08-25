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

let append_tr lst1 lst2 =
  let rec helper xs ys =
    match xs with
    | [] -> ys
    | (x::rest) -> helper rest (x::ys)
  in
  helper (reverse_tr lst1) lst2

let log2 = fun x -> (log x) /. (log 2.)

let sum l = List.fold_left (+.) 0. l
                                 
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

let uniques ?(eq = (=)) lst =
  let mem x xs = List.exists (eq x) xs in
  let rec uniques' checked rest =
    match rest with
      [] -> checked
    | (x::xs) -> if (mem x checked) then (uniques' checked xs) else (uniques' (x::checked) xs)
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

(****************************************************************************)

type weight = (Num.num * Num.num) option

let no_weight = None
let weight_one = Some(Num.num_of_int 1, Num.num_of_int 1)
let make_weight n1 n2 = Some(Num.num_of_big_int n1, Num.num_of_big_int n2)

(* Computes (n * 10^x) all in type num *)
let scientific_num n x =
    let zero = Num.num_of_int 0 in
    let rec power_of_ten x =
        assert (Num.ge_num x zero) ;
        if (Num.eq_num x zero) then
            (Num.num_of_int 1)
        else
            (Num.mult_num (Num.num_of_int 10) (power_of_ten (Num.pred_num x)))
    in
    let result_num =
        if (Num.ge_num x zero) then
            Num.mult_num n (power_of_ten x)
        else
            Num.div_num n (power_of_ten (Num.minus_num x))
    in
    result_num

(* I'll go to hell for this, I'm sure. *)
let num_from_decimal (str : string) : Num.num =
    let regex = Str.regexp "^\\([-+]\\)?\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?\\(e\\(-?[0-9]+\\)\\)?$" in
    if (Str.string_match regex str 0) then (
        let sign = try (Str.matched_group 1 str) with Not_found -> "+" in
        let whole_part = try (Str.matched_group 2 str) with Not_found -> (failwith "You've got to be kidding") in
        let decimal_part = try (Str.matched_group 4 str) with Not_found -> "0" in
        let exponent_part = try (Str.matched_group 6 str) with Not_found -> "0" in
        let numerator = Num.num_of_string decimal_part in
        let denominator = Num.num_of_string ("1" ^ (Str.global_replace (Str.regexp ".") "0" decimal_part)) in
        let sign_factor = match sign with "+" -> (Num.num_of_int 1) | _ -> (Num.num_of_int (-1)) in
        let result = scientific_num (Num.mult_num sign_factor
                                                  (Num.add_num (Num.num_of_string whole_part)
                                                               (Num.div_num numerator denominator)))
                                    (Num.num_of_int (int_of_string exponent_part))
        in
        result
    ) else (
        failwith (Printf.sprintf "num_from_decimal: can't interpret string '%s' as a decimal" str)
    )

(* Guaranteed to return a weight of the form Some(n,d) where n and d are both nums that correspond 
 * to whole integers. We keep all our weights this way so that show_weight works correctly. *)
let weight_from_float f =
    let n = num_from_decimal (Printf.sprintf "%g" f) in
    if (Num.is_integer_num n) then
        Some (n, (Num.num_of_int 1))
    else
        let s = Num.string_of_num n in     (* s is something like "234/5678" *)
        let regex = Str.regexp "^\\([0-9]+\\)\\/\\([0-9]+\\)$" in
        if (Str.string_match regex s 0) then (
            let num = Str.matched_group 1 s in
            let denom = Str.matched_group 2 s in
            Some (Num.num_of_string num, Num.num_of_string denom)
        ) else (
            failwith (Printf.sprintf "weight_from_float: I thought string_of_num was guaranteed to give me a string in fractional form, but it gave me '%s'" s)
        )

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
  | _ -> Printf.eprintf "WARNING: Multiplying a None weight with a non-None weight!\n" ; None

let add_weights w1 w2 =
  match (w1,w2) with
  | (None, None) -> None
  | (Some (n1,d1), Some (n2,d2)) ->
        let new_numerator = Num.add_num (Num.mult_num n1 d2) (Num.mult_num n2 d1) in
        let new_denominator = Num.mult_num d1 d2 in
        Some (new_numerator, new_denominator)
  | _ -> Printf.eprintf "WARNING: Adding a None weight with a non-None weight!\n" ; None

let compare_weights w1 w2 =
  match (w1,w2) with
  | (None, None) -> 0
  | (Some (n1,d1), Some (n2,d2)) -> Num.compare_num (Num.div_num n1 d1) (Num.div_num n2 d2)
  | (None, Some (n,d)) -> Printf.eprintf "WARNING: Comparing a None weight with a non-None weight!\n" ; Num.compare_num (Num.num_of_int 0) (Num.div_num n d)
  | (Some (n,d), None) -> Printf.eprintf "WARNING: Comparing a None weight with a non-None weight!\n" ; Num.compare_num (Num.div_num n d) (Num.num_of_int 0)

let num_of_weight w =
    match w with
    | None -> failwith "num_of_weight: None weight!"
    | Some(n,d) -> Num.div_num n d

let float_of_weight w = Num.float_of_num (num_of_weight w)

(* weighted random sampling
 assumes that all the weights have the same denominator (and also, I think, that the weights sum to one)
 we're using the 64-bit random number generator because these denominators are so freakin large
*)
let weighted_random xs_with_weights =
    match xs_with_weights with
    [] -> failwith "weighted_random: nothing to choose from"
    | (r,wt)::[] -> r
    | _ ->
        let sorted_xs_with_weights = List.sort (fun (_,wt1) (_,wt2) -> compare_weights wt1 wt2) xs_with_weights in
        let ratios = match (require_no_nones (map_tr snd sorted_xs_with_weights)) with 
                     Some rs -> rs | _ -> failwith "weighted_random: Got a none weight!" in
        let sorted_xs_with_ratios = List.map2 (fun (x,wt) ratio -> (x,ratio)) sorted_xs_with_weights ratios in
        let denominators = List.map snd ratios in
        let common_denominator = match (uniques denominators) with [d] -> d | _ -> failwith "weighted_random: Denominators should be equal!" in
        let sum_of_numerators = List.fold_left Num.add_num (Num.num_of_int 0) (List.map fst ratios) in
        if (sum_of_numerators <> common_denominator) then failwith "weighted_random: Weights do not sum to one!" ;
        let i64_of_num n = Big_int.int64_of_big_int (Num.big_int_of_num n) in
        let one = Num.num_of_int 1 in
        let die_roll = Random.int64 (i64_of_num (Num.add_num common_denominator one)) in
        let rec select stillleft = function
                [] -> failwith "select: nothing to choose from"
             | (x,wt)::[] -> x
             | (x,wt)::rest -> let (numerator,_) = wt in
                               let diff = Int64.sub stillleft (i64_of_num numerator) in
                               if diff < Int64.zero then x else select diff rest
        in
        select die_roll sorted_xs_with_ratios

