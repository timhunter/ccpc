#use "rasp.ml" (* #uses runner.ml *)
#use "stanford.ml"
#use "examples.ml"
#use "tree.ml";; (* John's tree stuff *)

(* derived from the in-class demo *)

let (more_examples : Runner.sentence list) =
[  ("unergative,reduced", "The horse raced past the barn fell.");
   ("unaccusative,reduced", "The cakes baked in the oven fell.");
   ("unergative,unred", "The horse that was raced past the barn fell.");
   ("unaccusative,unred", "The cakes that were baked in the oven fell.");
];;



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

Tree.write_file "st_gp_tree.dot" 
  (Tree.dot_of_tree_labeled st_gp_tree "Stanford");;

Tree.write_file "ra_gp_tree.dot" 
  (Tree.dot_of_tree_labeled ra_gp_tree "RASP");;

(*
Sys.command("make st_gp_tree.svg ra_gp_tree.svg");;

Sys.command("firefox st_gp_tree.svg ra_gp_tree.svg");;
*)
