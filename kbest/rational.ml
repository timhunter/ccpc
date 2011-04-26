(* A rational number will be represented by the type rat,
which is an ordered pair of ints, namely the numerator and denominator*)
type rat = int*int

let zero = (0, 1)

(* Calculating GCF will be important for other operations. 
Here, Euclid's efficient algorithm is used to find the GCF of 2 ints*)
let gcf n1 n2 =
  let rec gcf_aux big small =
    let remainder = big mod small in
    if remainder == 0 then small
    else gcf_aux small remainder
  in if n1>n2 then gcf_aux n1 n2 else gcf_aux n2 n1

(* Use the GCF to reduce a rational number to its canonical form:*)
let reduce r =
  if (fst r) == 0 then (0,1) else
  let g = gcf (fst r) (snd r) in
  (fst r)/g,(snd r)/g

(* Add two rationals: *)
let plusr r1 r2 = reduce ((fst r1)*(snd r2)+(fst r2)*(snd r1),(snd r1)*(snd r2))

(* Multiply two rationals: *)
let timesr r1 r2 = reduce ((fst r1)*(fst r2),(snd r1)*(snd r2))

(* Reciprocal *)
let recip r1 = (snd r1),(fst r1)

(* Divide two rationasl: *)
let divider r1 r2 = timesr r1 (recip r2)

(* Real value: *)
let realval r1 = (float_of_int (fst r1))/.(float_of_int (snd r1))

let compare r1 r2 =
  Pervasives.compare (realval r1) (realval r2)

(* Greater than *)
let greaterthanequals r1 r2 = (realval r1)>(realval r2)

(* Negate *)
let neg r1 = timesr (-1,1) r1

(* Absolute value: *)
let abs r1 = if (greaterthanequals r1 (0,1)) then r1 else (neg r1)

(* Exponentiation *)
let rec exp r1 pow = match pow with
    0 -> (1,1)
  |n -> timesr r1 (exp r1 (pow - 1))

(* String *)
let string_of_rat r1 = (string_of_int (fst r1))^"/"^(string_of_int (snd r1))
