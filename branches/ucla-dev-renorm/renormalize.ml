
open Util
open Rule

exception Failure of string

type mode = Naive | Newton

(****************************************************************************************)
(***implementation of getOrderedMutuallyRecursiveSets***)

(* remove dupilcated from the rules *)
let removeDup xs x = 
    if List.mem x xs then xs else x:: xs;;

let remove_from_left xs = List.rev (List.fold_left removeDup [] xs);;

(* get all the nonterminals in the rules *)
let getAllNonterm rules = 
    remove_from_left (List.map (fun r -> get_nonterm r) rules);;

(* helper function for reachable
 * merge two list in a way that no item is duplicated
 *)
let mergeList l1 l2 = 
    remove_from_left (List.append l1 l2);;

(* helper function for reachable
 * take a nonterminal, a rule and a list as arguments. 
 * If 1. the nonterminal matches 2.  the righthand side is nonterminating 3.the righthand side has not appeared in the list
 * push the right handside to the list (to be analyzed later)
 *)
let reachableHelper nonterm rule acc =
    if (nonterm = get_nonterm rule) then
        match (get_expansion rule) with
          | PublicTerminating str -> acc
          | PublicNonTerminating (nts,_) -> mergeList acc (Nelist.to_list nts)
    else acc;; 

(* go through the rules and collect all the right hand side in a no duplicated way*)
let reachableHelper2 nonterm rules acc =
    List.fold_left mergeList [] (List.map (fun r -> reachableHelper nonterm r acc) rules);;

(* find the first element from the list and return its index *)
let rec find x lst = 
     match lst with
      | [] -> raise (Failure "Not Found")
      | h :: t -> if x = h then 0 else 1 + find x t;;

(* print out a list of nonterminals that this symbol can reach 
   acc is used to store all the reachable nonterminals and avoid loop, the default acc for a nonterm is itself
*)
let rec reachable nonterm rules acc =  
     match acc with
      | [] -> raise (Failure "acc could not be empty")
      | h::t -> let temp = reachableHelper2 nonterm rules acc in
                  let index = find nonterm temp in
                   if (List.length temp = index + 1) then temp
                   else reachable (List.nth temp (index + 1)) rules temp;; 

(* test whether two nonterminals are mututally reachable *)
let mutallyReachable nonTermList reachableList nonTerm1 nonTerm2 = 
    let index2 = find nonTerm2 nonTermList in
      List.mem nonTerm1 (List.nth reachableList index2);;
(* remove all nonterminals that A can reach to but could not reach A *)
let mutallyReachable2 nonTermList reachableList nonTerm1 =
    let index1 = find nonTerm1 nonTermList in
    let workingCopy = List.nth reachableList index1 in
      List.filter (fun ele -> mutallyReachable nonTermList reachableList nonTerm1 ele) workingCopy;;

(* if return true, the two lists are equivalent. If return false, then the lists are not equivalent*)
let equalStringList l1 l2 = 
    if (List.length l1 = List.length l2) then not (List.mem false (List.map (fun ele -> List.mem ele l2) l1))
    else false;;

(* similar to List.mem, check whether this element appeared later *)
let rec existEquivalent l ll =
    match ll with
      | [] -> false
      | h::t -> if (equalStringList l h ) then true else existEquivalent l t;;

(* to remove lists that share the same element, for example [A;B] and [B;A]*)
let removeDupList x xs = 
    if (existEquivalent x xs) then xs else x::xs ;;

let removeDupListFromLeft xs = List.fold_right removeDupList xs [];;

let getMutuallyRecursiveSets rules =
    let nonTermList = getAllNonterm rules in
      let reachableList = List.map (fun nonterm -> reachable nonterm rules [nonterm]) nonTermList in
        removeDupListFromLeft (List.rev (List.map (fun ele -> mutallyReachable2 nonTermList reachableList ele) nonTermList));;  

(* Now we need to put these recursive sets into orders *)

(* helper function for left2num. for an element, find out which set includes it. Counter has default value 0*)
let rec findFromListofList ele ll counter= 
      match ll with
       | [] -> raise (Failure "Not Found")
       | h::t -> if (List.mem ele h) then counter else findFromListofList ele t (counter + 1);;

(* Assign every rules' left hand side nonterminal a number*)
let left2num mutuallyRecursiveSets rules= 
    let lhs = List.map (fun r -> get_nonterm r) rules in
      List.map (fun ele -> findFromListofList ele mutuallyRecursiveSets 0) lhs;;

(* If the right hand side is terminating, then return empty list. Otherwise, convert the right hand side symbols into numbers*)
(* Also remove the number that is on the lhs*)
let right2numHelper lhsNum mutuallyRecursiveSets rule=
    match (get_expansion rule) with
          | PublicTerminating str -> []
          | PublicNonTerminating (nts,_) -> let rightHandSide = Nelist.to_list nts in 
                                              List.filter (fun x -> x <> lhsNum) (List.map (fun ele -> findFromListofList ele mutuallyRecursiveSets 0) rightHandSide);;

(* Convert the right hand side of the rules into int list list*)
let right2num lhsList mutuallyRecursiveSets rules=
    List.map (fun r -> right2numHelper (List.nth lhsList (find r rules)) mutuallyRecursiveSets r) rules;;

(* find an element from lst1 that does not exist in lst2*)
let rec findUnique lst1 lst2=
    match lst1 with
      | [] -> raise (Failure "Not Found") (*This condition shall not appear*)
      | h::t -> if (List.mem h lst2) then findUnique t lst2 else h;;

(* same as zip function in python, combine two list together*)
let rec zip lst1 lst2=
    match (lst1,lst2) with
      | ([],[]) -> []
      | (x::xs, y::ys) -> (x,y) :: (zip xs ys)
      | _ -> raise (Failure "unmatched left and right hand side");;

(* the default value of lst1 and lst2 are []*)
let rec unzip zippedList lst1 lst2= 
    match zippedList with
      | [] -> (List.rev lst1, List.rev lst2)
      | h::t -> unzip t ((fst h)::lst1) ((snd h)::lst2);;

(* convert the rules to number tuples, with the first element as lhs and second element as rhs*)
let convert2tuple mutuallyRecursiveSets rules=
    let lhsList = left2num mutuallyRecursiveSets rules in
      let rhsList = right2num lhsList mutuallyRecursiveSets rules in
        zip lhsList rhsList;;

let findRemoveNum zippedList = 
    let (lst1, lst2) = unzip zippedList [] [] in 
        findUnique lst1 (List.flatten lst2);;
        
(* If the first element of the tuple equal to removeNum, then remove it from the list*)
let tupleReduction removeNum zippedList=
    List.filter (fun tup -> (fst tup) <> removeNum) zippedList;;

(* acc has a default value of []*)
let rec getOrder rules acc zippedList= 
(*     let mutuallyRecursiveSets = getMutuallyRecursiveSets rules in
      let zippedList = convert2tuple mutuallyRecursiveSets rules in  *)
        match zippedList with 
          | [] -> acc
          | _ -> let removeNum = findRemoveNum zippedList in
                    getOrder rules (removeNum::acc) (tupleReduction removeNum zippedList);;

(* acc has a default value of []*)
(* This function is used to tease out nonterminals that are not depended by any other
nonterminals. By doing this we can avoid the problem of order*)
let rec outputInOrder mutuallyRecursiveSets order acc= 
    match order with
      | [] -> acc
      | h::t -> outputInOrder mutuallyRecursiveSets t ((List.nth mutuallyRecursiveSets h) :: acc)


let getOrderedMutuallyRecursiveSets rules = 
    let mutuallyRecursiveSets = getMutuallyRecursiveSets rules in 
      let zippedList = convert2tuple mutuallyRecursiveSets rules in
        let order = getOrder rules [] zippedList in 
          List.rev (outputInOrder mutuallyRecursiveSets order []);;







(* test function used to print out*)
let rec print_list_string myList = 
  match myList with
    | [] -> print_endline "]"
    | head::body -> 
          begin
          Printf.printf "\"%s\"; " head;
          print_list_string body
          end;;

(* test function*)
let rec print_string_list myList = 
  match myList with
    | [] -> print_endline "]"
    | head::body ->
        begin
          Printf.printf "[";
          print_list_string head;
          print_string_list body
        end;;



(****************************************************************************************)
(*implementation of naive method and Newton's method*)


(* the type used in the table function storing the in-progress Z value for all terminals at depth k
 and the settled Z value for all terminals*)
type indicator= Depth of int | Settled;;


(* initialTable is the initial getZ function that takes an indicator
-(Depth int) if the approximate has not been reached
-(Settled) if the approximate has already been reached
and a string as the non-terminal 
and returns the float 0.0 *)
(* val initialTable : indicator -> string -> float = <fun> *)
let initialTable (k:indicator) (s:string)= 0.0;;


(*add is a function that takes in a getZ function and an indicator, a string and a float to return
a new getZ function that will add in the information of if getting that indicator and that string,
it will return that float*)
(* val add :
  (indicator -> string -> float) ->
  indicator -> string -> float -> indicator -> string ->float = <fun> *)
let add (getZ:indicator->string->float) (k:indicator) (s:string)(v:float)=
(*fun k' s'=0.0;*) 
fun k' s'->
    if k'=k && s'=s then v else getZ k' s';;


(* helper function of sameSet 
return true if the string element is within the string list 
otherwise return false
*)
let rec contains (element: string)(stringList: string list)=
(*false;;*)
match stringList with
|h::t->if h=element then true else (contains element t)
|_->false;;


(* helper function
return true if the two strings are in the same set
return false otherwise *)
let rec sameSet (nonterminal:string) (comparedString: string) (mutuallyRecursiveSets: string list list)=
(*false;*)
match mutuallyRecursiveSets with 
|h::t-> if (contains nonterminal h) then 
            if (contains comparedString h) then true else false
        else (sameSet nonterminal comparedString t)
|_->false


(* get a string as nontermal and a rule lits, return a list of tuples consisting of 
nonterminal list and a probability
for example, if in the rules there are VP -> V NP (0.4); VP -> V CP(0.6)
calling (findRule "VP" rules) will return [(["V";"NP"],0.4); (["V";"CP"],0.6)]
*)
let rec findRule (nonterminal:string) (rules: Rule.r list)(* =[(["nonterminal1";"nonterminal12"],0.05)] *)
= match rules with
|h::t -> if (get_nonterm h) = nonterminal then 
        match (get_expansion h) with
        |PublicTerminating str -> ([],(float_of_weight (get_weight h)))::(findRule nonterminal t)
        |PublicNonTerminating (nts,_) ->  ((Nelist.to_list nts),(float_of_weight (get_weight h)))::
        (findRule nonterminal t)
    else (findRule nonterminal t)
| _-> [];;

(****************************************************************************************)
(*naive special function*)
(*this function returns a float representing the probability of one expansion of one nonterminal at depth k based on
either probability of other sameSet nonterminals at depth (k-1)
or probability of other non-sameSet nonterminals's settled approximate
*)
let rec naiveOneNonTeminalOneRuleAtLevelK (k:int)(nonterminal:string)(onePair:(string list*float))
(someMiddleTable: indicator->string->float)(mutuallyRecursiveSets: string list list)=
(*0.0;*)
match onePair with
|(h::t,f)-> (* (Printf.printf "I am here, the h is %s and the two floats are %f and %f \n" h (someMiddleTable Settled h)(naiveOneNonTeminalOneRuleAtLevelK k nonterminal (t,f) someMiddleTable mutuallyRecursiveSets));*)
        if (sameSet nonterminal h mutuallyRecursiveSets) then (someMiddleTable (Depth (k-1)) h)*. 
        (naiveOneNonTeminalOneRuleAtLevelK k nonterminal (t,f) someMiddleTable mutuallyRecursiveSets)
    else (someMiddleTable Settled h)*. (naiveOneNonTeminalOneRuleAtLevelK k nonterminal (t,f) someMiddleTable mutuallyRecursiveSets)

|(_,f)-> f;;


let rec naiveOneNonTeminalAtLevelK (k:int)(nonterminal:string)(alllist:(string list*float) list)
(someMiddleTable: indicator->string->float)(mutuallyRecursiveSets: string list list)
=match alllist with
                |(a,b)::t-> (* (Printf.printf "the head is %f and the float is %f \n" b 
                  (naiveOneNonTeminalOneRuleAtLevelK k nonterminal (a,b) someMiddleTable mutuallyRecursiveSets)); *)
                  (naiveOneNonTeminalOneRuleAtLevelK k nonterminal (a,b) someMiddleTable mutuallyRecursiveSets) +. 
                  (naiveOneNonTeminalAtLevelK k nonterminal t someMiddleTable mutuallyRecursiveSets)
                | _ -> 0.0
(****************************************************************************************)

(*Newton's Method's special functions*)

(*Yi*)
let rec getf (nonterminal: string)(nonterminalList: string list) (k:int)(someMiddleTable: indicator->string->float)
(mutuallyRecursiveSets: string list list)
(* =0.0 *)
=match nonterminalList with 
		|[]->1.0
		|(h::t)-> 
				let rest=getf nonterminal t k someMiddleTable mutuallyRecursiveSets in
				if (sameSet nonterminal h mutuallyRecursiveSets) then (someMiddleTable (Depth k) h)*.rest
				else (someMiddleTable Settled h)*.rest;;

let rec getFForOneX (k: int) (nonterminal: string) (someMiddleTable: indicator->string->float)
(expansion: (string list * float) list) (mutuallyRecursiveSets: string list list)
=match expansion with
	|((a,b)::t)-> (b *.(getf nonterminal a k someMiddleTable mutuallyRecursiveSets))+.(getFForOneX k nonterminal someMiddleTable t mutuallyRecursiveSets)
	|([]) -> 0.0 -. (someMiddleTable (Depth k) nonterminal);;


let rec getFForVector (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float)
(rules: Rule.r list) (mutuallyRecursiveSets: string list list)
(* =[0.0;0.0;0.0];; *)
=match oneSet with
	|[] -> []
	|(h::t) -> Util.append_tr [getFForOneX k h someMiddleTable (findRule h rules) mutuallyRecursiveSets] 
				(getFForVector k t someMiddleTable rules mutuallyRecursiveSets)

(****************************************************************************************)
(*Junyi*)
let calculateDelta (i: string) (j: string)=
  if (i = j) then 1.0 else 0.0;;

(*can use the getf function. return float*)
(* Notice that the equation No.32 in the original paper might be confusing. The index i for the delat(i,j) in equation 32 no longer
   stands for the nontermial, instead, it stands for the head of alpha(or in the equation, B).
*)
let rec getDFAlphaJ (alpha: string list)(i: string) (j: string) (k:int) (someMiddleTable: indicator -> string -> float)
(mutuallyRecursiveSets: string list list)=
  match (alpha, j) with
    |([], j) -> 0.0 (*Equation No.29*)
    |(h::t, j) ->
        let rest = getDFAlphaJ t i j k someMiddleTable mutuallyRecursiveSets in 
          if (sameSet i h mutuallyRecursiveSets) 
            then ((someMiddleTable (Depth k) h)*. rest) +. ((calculateDelta h j) *. (getf i t k someMiddleTable mutuallyRecursiveSets)) (* Equation No.32 *)
            else (someMiddleTable Settled h) *. rest;; (* Equation No. 31*)

let rec getDFijHelper (i: string) (j: string) (k:int) (someMiddleTable: indicator->string->float) (allList:(string list*float) list)
(mutuallyRecursiveSets: string list list) (rules: Rule.r list)=
  match allList with
    |(a,b)::t ->  (* (Printf.printf "the current rule is %f \n" ((getDFAlphaJ a i j k someMiddleTable mutuallyRecursiveSets) *. b)); *)
                  ((getDFAlphaJ a i j k someMiddleTable mutuallyRecursiveSets) *. b) +. 
                  (getDFijHelper i j k someMiddleTable t mutuallyRecursiveSets rules)
    |[] -> 0.0 -. (calculateDelta i j);; (* (Printf.printf "the delta is %f \n" (0.0 -. (calculateDelta i j)));; *)

let getDFij k someMiddleTable mutuallyRecursiveSets rules i j  = 
    getDFijHelper i j k someMiddleTable (findRule i rules) mutuallyRecursiveSets rules;;


(****************************************************************************************)
let getJFMatrix (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float)
(rules: Rule.r list) (mutuallyRecursiveSets: string list list)
= let m=Matrix.create_square_matrix oneSet (getDFij k someMiddleTable mutuallyRecursiveSets rules) in
if(k<10) then Matrix.print m;
m
;;


(*with two float lists of the same length, substract the second float list from the first one to get a new float list*)
let rec floatListSubstraction (list1: float list) (list2: float list)
=match (list1,list2) with
	| (h1::t1,h2::t2) -> Util.append_tr [h1 -. h2] (floatListSubstraction t1 t2)
	| ([],[])-> []
	| _ -> raise (Failure "cannot substract two float lists with different length");;

let rec fillTableForSetAtDepthKNewton (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float)
(floatlist: float list)
=match (oneSet,floatlist) with 
	| (h1::t1,h2::t2) -> 
		if(k<10) then Printf.printf "at level %i for %s put in value %f \n" k h1 h2;
	fillTableForSetAtDepthKNewton k t1 (add someMiddleTable (Depth k) h1 h2) t2
	| ([],[])->someMiddleTable
	| _->raise (Failure "string list and float lost don't match in length");;

let rec getFloatListForSetAtLevelKFromTable (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float)
(*TODO*)
=match oneSet with 
	|(h::t)-> Util.append_tr [someMiddleTable (Depth k) h] (getFloatListForSetAtLevelKFromTable k t someMiddleTable)
	| [] -> [];;

(* Testing floatlist *)
let rec printList l=
  match l with
  | (h::t) -> Printf.printf "%f !!!" h;
              printList t
  | [] -> Printf.printf "\n\n\n";;

let getNewFloatListForSetAtLevelK (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float)
(rules: Rule.r list) (mutuallyRecursiveSets: string list list)
= floatListSubstraction (getFloatListForSetAtLevelKFromTable k oneSet someMiddleTable) (Matrix.mult_by_vec 
	(Matrix.invert (getJFMatrix k oneSet someMiddleTable rules mutuallyRecursiveSets))
	(getFForVector k oneSet someMiddleTable rules mutuallyRecursiveSets)
	)


(****************************************************************************************)

(*put in values into the table for a set at depth k*)
(*this is where Newton's method and naive method takes apart*)
let rec oneSetAtLevelK mode (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float) 
(rules: Rule.r list) (mutuallyRecursiveSets: string list list)
=(* fun (k':indicator) (s':string) -> 5.0;; *)
match mode with 
  |Naive->(match oneSet with
              | h::t -> 
              (* Printf.printf "I am adding the value %.20f for %s at level %i \n" (naiveOneNonTeminalAtLevelK mode k h (findRule h rules) someMiddleTable mutuallyRecursiveSets) h k; *)
              let newTable = (add someMiddleTable (Depth k) h 
                  (naiveOneNonTeminalAtLevelK k h (findRule h rules) someMiddleTable mutuallyRecursiveSets))  in
              (oneSetAtLevelK mode k t newTable rules mutuallyRecursiveSets)
              | _ -> someMiddleTable)
  |Newton->fillTableForSetAtDepthKNewton k oneSet someMiddleTable 
  	(getNewFloatListForSetAtLevelK (k-1) oneSet someMiddleTable rules mutuallyRecursiveSets);;
   

(*helper of oneSetWithInRangeAtLevelK, check whether one non-terminal has change below shreshold at depth k*)
let oneNonTerminalWithInRangeAtLevelK (r:float) (someMiddleTable: indicator->string->float) 
(k:int) (nonterminal:string)=
(*false*)
abs_float((someMiddleTable (Depth k) nonterminal)-.(someMiddleTable (Depth (k-1)) nonterminal))<=r 


(*whether every element in a given set has change below shreshold at depth k*)
let rec oneSetWithInRangeAtLevelK (r:float) (someMiddleTable: indicator->string->float) 
(k:int) (oneSet:string list)=
(* false;; *)
match oneSet with
| h::t -> if (oneNonTerminalWithInRangeAtLevelK r someMiddleTable k h) 
            then (oneSetWithInRangeAtLevelK r someMiddleTable k t)
        else
            false

| _ -> true;;


(*settle down the Z value for the set*)
let rec settleTableForSet (oneSet: string list) (k: int) (someMiddleTable: indicator->string->float)
=match oneSet with
| h::t-> 
(* Printf.printf "I am settling the value for %s at level %i: %f \n" h k (someMiddleTable (Depth k) h); *)
let updateTable= add someMiddleTable Settled h (someMiddleTable (Depth k) h) in
(settleTableForSet t k updateTable)
| _-> someMiddleTable;;


let rec oneSetCheckAtLevelKAndIncrement mode (k:int)(r:float)(oneSet: string list)(someMiddleTable: indicator->string->float)
(rules: Rule.r list) (mutuallyRecursiveSets: string list list)=
if (oneSetWithInRangeAtLevelK r someMiddleTable k oneSet) 
then (settleTableForSet oneSet k someMiddleTable)
else (oneSetCheckAtLevelKAndIncrement mode
(k+1) r oneSet 
(oneSetAtLevelK mode (k+1) oneSet someMiddleTable rules mutuallyRecursiveSets)
rules mutuallyRecursiveSets
);;

 
(* the settleOneSet is the helper function for fillInTableBySets 
This one calculates the Z value from depth 0 to depth k needed for a set of nonterminals and settle down their values
*)
let rec settleOneSet mode (oneSet: string list) (someMiddleTable: indicator->string->float) (rules: Rule.r list) 
(r: float)(mutuallyRecursiveSets: string list list)=
(* fun (k':indicator) (s':string) -> 5.0;; *)
(oneSetCheckAtLevelKAndIncrement mode
1 r oneSet 
(oneSetAtLevelK mode 1 oneSet someMiddleTable rules mutuallyRecursiveSets)
rules mutuallyRecursiveSets
);;


(*the fillInTableBySets takes in a ordered mutuallyRecursiveSets, an initialTable, rule list and threshold and
returns a newTable which can tell you what Z value each non-terminal has within the threshold*)
let rec fillInTableBySets mode (mutuallyRecursiveSets: string list list) (initialTable: indicator->string->float)
(rules: Rule.r list) (r: float) 
(*  = fun (k':indicator) (s':string) -> 5.0;; *)
=match mutuallyRecursiveSets with
|h::t-> let updateTable= (settleOneSet mode h initialTable rules r mutuallyRecursiveSets) in
(fillInTableBySets mode t updateTable rules r)
|_-> initialTable;;


let getTable mode (rules: Rule.r list) (r: float)
(*  = fun (k':indicator) (s':string) -> 5.0;; *)
= fillInTableBySets mode (getOrderedMutuallyRecursiveSets rules) initialTable rules r ;;



(* All the work will go here. *)
(* The arguments are an existing grammar (list of rules) to be normalized and that grammar's start symbol. 
 * The return value is a pair; the first member is the total probability mass associated with the start 
 * symbol in the argument grammar, and the second member is the list of rules with reweighted probabilities. 
 *)
let renormalize_grammar mode rules start_symbol =
    (*** dummy code showing manipulation of rules ***)
    List.iter (fun r ->
        (* get_nonterm and get_weight are from rule.ml; float_of_weight is from util.ml, see also other weight functions there *)
        Printf.printf "This rule expands %s with weight %f\n" (get_nonterm r) (float_of_weight (get_weight r)) ;
        match (get_expansion r) with
        | PublicTerminating str -> Printf.printf "   and the right-hand side is the terminal '%s'\n" str
        | PublicNonTerminating (nts,_) -> Printf.printf "   and the right-hand side has nonterminals %s\n" (show_list (fun x -> x) (Nelist.to_list nts))
                                                                                                            (* show_list is another util.ml function *)
    ) rules ;
    Printf.printf "Mode is %s\n" (match mode with Naive -> "naive" | Newton -> "Newton's") ;
    (*** end dummy code ***)
    (getTable mode rules 0.0000001 Settled start_symbol, rules)

let main () =
    let mode = ref Naive in
    let grammar_file = ref "" in
    let speclist = Arg.align([  ("-g",      Arg.Set_string(grammar_file),           " WMCFG grammar file (obligatory)") ;
                                ("-naive",  Arg.Unit(fun () -> mode := Naive),      " use naive method") ;
                                ("-newton", Arg.Unit(fun () -> mode := Newton),     " use Newton's method") ;
                            ]) in
    let usage_msg = Printf.sprintf "Usage: %s -g <grammar file>" Sys.argv.(0) in
    let superfluous_arg s = raise (Arg.Bad (Printf.sprintf "Bad extra argument: %s" s)) in
    Arg.parse speclist superfluous_arg usage_msg ;
    if (!grammar_file = "") then (
        Printf.eprintf "Must provide a grammar file\n" ;
        Arg.usage speclist usage_msg
    ) else (
        (* Everything's OK, let's do our thing ... *)
        let (rules,start_symbol) = Grammar.get_input_grammar (!grammar_file) in
        let (prob, new_rules) = renormalize_grammar (!mode) rules start_symbol in



(*************************************************************************************************)
(*tests for functions*)

(* test for fillInTableBySets #use ./renormalize -g grammars/Yi_test_grammar/simplegrammar.wmcfg *)
     (* let mutuallyRecursiveSets4= [["C"];["A";"B"];["S"]] in
        let range=0.00001 in
        let finalTable=fillInTableBySets mutuallyRecursiveSets4 initialTable rules range in
        List.iter(fun x-> 
            Printf.printf "this is %s and the probability of it within the range %f is %f \n"
            x range (finalTable Settled x) 

        ) ["A";"B";"C";"S"]; *)



(* test for oneSetWithInRangeAtLevelK #use ./renormalize -g grammars/Yi_test_grammar/simplegrammar.wmcfg*)
      (* let someMiddleTable = add (add (add (add (add initialTable (Depth 2) "B" 0.65) (Depth 2) "C" 2.0) (Depth 2) "A" 0.1) (Depth 1) "A" 0.0) (Depth 1) "B" 0.75 in
        if(oneSetWithInRangeAtLevelK 0.1 someMiddleTable 2 ["A";"B"]) 
        then Printf.printf "oneNonTerminalWithInRangeAtLevelK passed test1 \n"
        else Printf.printf "oneNonTerminalWithInRangeAtLevelK not passing test1 \n";

        let someMiddleTable2 = add (add (add (add (add initialTable (Depth 2) "B" 0.65) (Depth 2) "C" 2.0) (Depth 2) "A" 0.1) (Depth 1) "A" 0.0) (Depth 1) "B" 0.75 in
        if not(oneSetWithInRangeAtLevelK 0.01 someMiddleTable2 2 ["A";"B"]) 
        then Printf.printf "oneNonTerminalWithInRangeAtLevelK passed test2 \n"
        else Printf.printf "oneNonTerminalWithInRangeAtLevelK not passing test2 \n"; *)



(*test for naiveOneNonTeminalOneRuleAtLevelK #use ./renormalize -g grammars/Yi_test_grammar/simplegrammar.wmcfg*)
      (*let mutuallyRecursiveSets3= [["C"];["A";"B"]] in 
        let someMiddleTable = add (add (add initialTable (Depth 1) "C" 0.5) (Depth 2) "C" 2.0) Settled "C" 1.0 in
        let prob_A_level1= (naiveOneNonTeminalAtLevelK mode 1 "A" (findRule "A" rules) someMiddleTable mutuallyRecursiveSets3) in
        Printf.printf "test for naiveOneNonTeminalAtLevelK mode for A at depth 1 the answer should be 0 \n";
        Printf.printf "the answer is actually %f \n" prob_A_level1;

        let prob_B_level1= (naiveOneNonTeminalAtLevelK mode 1 "B" (findRule "B" rules) someMiddleTable mutuallyRecursiveSets3) in
        Printf.printf "test for naiveOneNonTeminalAtLevelK mode for B at depth 1 the answer should be 0.75 \n";
        Printf.printf "the answer is actually %f \n" prob_B_level1; *)        
      (*let mutuallyRecursiveSets3= [["C"];["A";"B"]] in 
        let someMiddleTable = add (add (add (add (add initialTable (Depth 1) "C" 0.5) (Depth 2) "C" 2.0) Settled "C" 1.0) (Depth 1) "A" 0.0) (Depth 1) "B" 0.75in
        let prob_A_level2= (naiveOneNonTeminalAtLevelK mode 2 "A" (findRule "A" rules) someMiddleTable mutuallyRecursiveSets3) in
        Printf.printf "test for naiveOneNonTeminalAtLevelK mode for A at depth 2 the answer should be 0.375 \n";
        Printf.printf "the answer is actually %f \n" prob_A_level2;

        let prob_B_level2= (naiveOneNonTeminalAtLevelK mode 2 "B" (findRule "B" rules) someMiddleTable mutuallyRecursiveSets3) in
        Printf.printf "test for naiveOneNonTeminalAtLevelK mode for B at depth 2 the answer should be 0.75 \n";
        Printf.printf "the answer is actually %f \n" prob_B_level2; *)



(* test for sameSet #use ./renormalize -g grammars/wmcfg/toy.wmcfg *)
     (* let mutuallyRecursiveSets2=[["Adj";"Adj2";"Adj3"];["N"];["NBAR";"NBAR2"];["Det"];["NP"];["VP"];["S"]] in

        if not(sameSet "N" "NBAR" mutuallyRecursiveSets2) then 
        Printf.printf "sameSet passed test1 \n"
        else Printf.printf "sameSet not passing test1 \n";

        if (sameSet "NBAR2" "NBAR" mutuallyRecursiveSets2) then 
        Printf.printf "sameSet passed test2 \n"
        else Printf.printf "sameSet not passing test2 \n";


        if not(sameSet "Adj3" "S" mutuallyRecursiveSets2) then 
        Printf.printf "sameSet passed test3 \n"
        else Printf.printf "sameSet not passing test3 \n"; *)



(* test for contains #use ./renormalize -g grammars/wmcfg/toy.wmcfg*)
      (* 
        if (contains "hi" ["p";"l";"hi"]) then Printf.printf "first test passed for contains \n"
        else Printf.printf "first test not passed for contains \n";
        if (not (contains "hi" [])) then Printf.printf "second test passed for contains \n"
        else Printf.printf "second test not passed for contains \n"; *)



(*the test for findRule #use ./renormalize -g grammars/wmcfg/toy.wmcfg*)
      (*let modifiedlist=findRule "PP" rules in 
        List.iter (fun (a,b)-> 
            Printf.printf "this is nonterminals %s \n" (show_list (fun x -> x) a);
            Printf.printf "this is weight %f \n" b) 
            modifiedlist; *)


        Printf.printf "(* \"probability = %f\" *)\n" prob ;
    )

let _ = if (!Sys.interactive) then () else main ()

