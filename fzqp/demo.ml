#use "rasp.ml" (* #uses runner.ml *)
#use "stanford.ml"
#use "examples.ml"
#use "tree.ml";; (* John's tree stuff *)

(* We now have a runner object. *)

Runner.runner;;


0;;

(* a parser interface is an object that provides:
   parser_id:  the string id of the parser, which must be unique
   initialize : string -> int  initializes if necessary
   parse : sentence list -> parsed_sentence list
   close : () -> parsed_sentence list *)

let mydummypi = new Runner.parser_interface "dummypi"  (* id is dummypi *)
  (fun x -> 0) (* initialize is a noop *)
  (fun x -> List.map (fun (z, y) -> (z, "dummypi parsed "^y)) x) (*  parse *)
  (fun x -> []) (* close is also a noop *);;



(* sentences are (id, sentence) pairs *)
0;;
let sentences2 = [
  ("first-line", "The limerick is furtive and mean.");
  ("second-line", "You must keep it in close quarantine.");
  ("third-line", "Or it sneaks to the slums, and promptly becomes disorderly, drunk, and obscene.")
  ];;



(* We've got a couple other parser interfaces *)
0;;
Stanford.pi;; (* the Stanford parser *)
Rasp.pi;;   (* the RASP parser *)




(* we register the pis with the runner: *)
0;;
Runner.runner#add_pi Stanford.pi;;

Runner.runner#add_pi Rasp.pi;;

Runner.runner#add_pi mydummypi;;
(* When we register a pi, the runner calls its initialize method.  
   If initialize returns something other than zero, runner raises an
   exception and that pi isn't registered.  
*)




(* the runner won't let us register multiple pis with the same id: *)
0;;
Runner.runner#add_pi Stanford.pi;;



(* Now, we parse: *)
0;;
let results = Runner.runner#parse_all sentences2;;


(* results is a result list.  A result is a bracketed sexp 
     ( "pi_id", ("sentence_id", "sentence to be parsed")) *)
0;;
List.filter (fun x -> (fst x) = "Rasp") results;;

List.filter (fun x -> (fst (snd x)) = "third-line") results;;



(* As sentence ids can be arbitrary strings, we can easily imagine a corpus 
   where the ids of each sentence were much more descriptive than the ones 
   here (stolen from Kyle's website):                                       *)
0;;
let (more_examples : Runner.sentence list) =
[  ("unergative,reduced", "The horse raced past the barn fell.");
   ("unaccusative,reduced", "The cakes baked in the oven fell.");
   ("unergative,unred", "The horse that was raced past the barn fell.");
   ("unaccusative,unred", "The cakes that were baked in the oven fell.");
];;



(* we don't need that dummy pi in there anymore: *)
0;;
Runner.runner#list_pis ();;
Runner.runner#remove_pi "dummypi";;
Runner.runner#list_pis ();;


let more_results = Runner.runner#parse_all more_examples;;


(* to use this in ocaml we need to write our own string contains function: *)
0;;
#load "str.cma";;
let string_contains sub super = 
  let reg = Str.regexp_string sub in
    try
      ignore (Str.search_forward reg super 0); true
    with Not_found -> false;;

let get_phenomenon phen sentences = 
  List.filter (fun x -> string_contains phen (fst (snd x))) sentences;;


(* now, we can: *)
0;;
let unergs = get_phenomenon "unergative" more_results;;

let garden_paths = get_phenomenon "reduced" unergs;;


(* Using John's tree library, we can generate and write out dot and qtree
   trees corresponding to the analyses:                                  *)
0;;
let stanford_gp = List.filter (fun x -> (fst x) = "Stanford") garden_paths;;
let rasp_gp = List.filter (fun x -> (fst x) = "Rasp") garden_paths;;
 
let st_gp_tree = Tree.read_sexp (snd (snd (List.hd stanford_gp)));;
let ra_gp_tree = Tree.read_sexp (snd (snd (List.hd rasp_gp)));;



print_string "uh oh.";;




(* I'm going to cheat a little here: *)
let ra_gp_tree = Tree.read_sexp "(TOP  (S (NP (AT The:1) (NN1 horse:2))   (VP (VVD race+ed:3)    (PP (II past:4) (S (NP (AT the:5) (NNL1 barn:6)) (VVD fall+ed:7)))))  (. .:8))";;

Tree.write_file "st_gp_tree.dot" 
  (Tree.dot_of_tree_labeled st_gp_tree "Stanford");;

Tree.write_file "ra_gp_tree.dot" 
  (Tree.dot_of_tree_labeled ra_gp_tree "RASP");;


Sys.command("make st_gp_tree.svg ra_gp_tree.svg");;

Sys.command("firefox st_gp_tree.svg ra_gp_tree.svg");;
