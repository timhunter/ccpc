(* At the bottom, setting the Elts module equal to one of the following uses
 * that module for the creation of matrices and simplex *)

module Floats : EltsI.ORDERED_AND_OPERATIONAL =
struct

  exception NonElt

  type t = float
  
  (* Need an epsilon to compare floats. We define the precion here *)
  let epsilon = 0.000001

  let zero = 0.

  let one = 1.

  (* We compare based on epsilon. We first find the distance
   * between the two numbers and divide my the maximum to see
   * if that's below epsilon. *)
  let compare a b =
    let a', b' = abs_float a, abs_float b in
    if a' < epsilon && b' < epsilon then 
      Order.Equal
    else 
      let diff = (a -. b) /. (max a' b') in 
      if abs_float diff < epsilon then Order.Equal
      else if a < b then Order.Less
      else if a > b then Order.Greater
      else
        raise (Failure "Error in compare.")

  let to_float x = x

  let from_float x = x

  let to_string = string_of_float

  let from_string (x: string) = 
    try
      float_of_string x
    with
    | Failure ("float_of_string") -> raise NonElt

  let add = (+.)

  let subtract = (-.)

  let multiply = ( *. ) 

  let divide = (/.) 

  let print a = print_string ((to_string a) ^ "\n")

  let trim = int_of_float

  let generate () = 3. 

  let generate_gt a () = a +. 1.

  let generate_lt a () = a -. 1. 

  let generate_between a b () = if a > b then None else Some((a +. b)/. 2.)
  
  let generate_x x () = (x:t)

  let generate_random bound () =
    let x = Random.float bound in
    x -. mod_float x epsilon

  (************************ TESTS ********************************)  

  let rec test_compare (times:int) : unit =
    let random t = float_of_int (Random.int t - Random.int t) in
    if times = 0 then ()
    else
      let x, y = random times, random times in
      match compare x y with
      | Order.Equal -> assert(x = y)
      | Order.Greater -> assert(x > y)
      | Order.Less -> assert(x < y)


  (* Honestly, probably don't need to test the other functions *)
  let run_tests (times: int) : unit =
    test_compare times ;
    ()
end

module Num_floats : EltsI.ORDERED_AND_OPERATIONAL = 
struct

  exception NonElt

  type t = Num.num

  let zero = Num.num_of_int 0

  let one = Num.num_of_int 1

  let compare a b = 
    if Num.compare_num a b = -1 then Order.Less
    else if Num.compare_num a b = 0 then Order.Equal
    else if Num.compare_num a b = 1 then Order.Greater
    else
      raise (Failure "Error in compare.")

  let to_float = Num.float_of_num

  let from_float f = Num.num_of_string (string_of_float f)

  let to_string = Num.string_of_num

  let from_string (x: string) = 
    try
      Num.num_of_string x
    with
    | Failure ("num_of_string") -> raise NonElt

  let add = Num.add_num

  let subtract = Num.sub_num

  let multiply = Num.mult_num

  let divide = Num.div_num

  let generate () = Num.num_of_int 3

  let generate_gt a () = Num.add_num a one 

  let generate_lt a () = Num.sub_num a (subtract zero one) 

  let generate_between a b () = 
    if Num.gt_num a b then None 
    else Some (Num.div_num (Num.add_num a b) (Num.num_of_int 2))

  let generate_x (x:float) () = 
    let new_x = int_of_float x in Num.num_of_int new_x

  (* This actually only generates integers. *)
  let generate_random (bound: float) () = 
    let new_bound = int_of_float bound in 
    Num.num_of_int (Random.int new_bound)

  let print a = print_string ((to_string a) ^ "\n")

  let rec test_compare (times:int) : unit =
    let random t = Num.num_of_int (Random.int t - Random.int t) in
    if times = 0 then ()
    else
      let x, y = random times, random times in
      match compare x y with
      | Order.Equal -> assert(x = y)
      | Order.Greater -> assert(x > y)
      | Order.Less -> assert(x < y)


  let run_tests (times: int) : unit =
    test_compare times ;
    ()
end


(*** HERE IS WHERE YOU PICK THE MODULE *)
module Elts = Floats
(* module Elts = Num_floats *)
