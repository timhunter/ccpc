
#use "mcfg2.ml"

let larsonian_grammar_file = "../grammars/mcfgs/larsonian.mcfg";;
let larsonian_deemptied_file =  "../grammars/mcfgs/larsonian.deemptied.mcfg";;

let larsonian_pi = new Runner.parser_interface "mcfgcky2-larsonian" Mcfg2.initialize (Mcfg2.parse larsonian_grammar_file) Mcfg2.close;;

let deemptied_pi = new Runner.parser_interface "mcfgcky2-deemptied" Mcfg2.initialize (Mcfg2.parse larsonian_deemptied_file) Mcfg2.close;;

Runner.runner#add_pi larsonian_pi;;
Runner.runner#add_pi deemptied_pi;;

let larsonian_test_sentences = Runner.Util.make_sentence_list 
  (Runner.Util.read_file "larsonian.txt")

let n = Runner.Util.range 6
let m = [7;8;9;10;11;12]
let o = [13;14;15;16;17;18]
let p = [19;20;21;22;23]

let sents n = List.map (fun x -> List.nth larsonian_test_sentences x) n
let all_sentences = larsonian_test_sentences  

(* for distributing the job *)
let first_six = sents n
let second_six = sents m
let third_six  = sents o
let last_five = sents p

let p x = Runner.runner#parse_all x
(* let results = Runner.runner#parse_all all_sentences *)

let after n list =
  let rec aux c = function
      [] -> []
    | h::t -> if c < n then aux (c+1) t else h::(aux (c+1) t)
  in
    aux 0 list
    
