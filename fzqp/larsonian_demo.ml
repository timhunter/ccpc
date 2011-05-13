
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

let test_sentences = List.map (fun x -> List.nth larsonian_test_sentences x) n
let all_sentences = larsonian_test_sentences  

let results = Runner.runner#parse_all test_sentences
