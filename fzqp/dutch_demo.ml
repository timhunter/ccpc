
#use "mcfg2.ml"

let ctrl_grammar =  "../grammars/mcfgs/nl7-ctrl.mcfg";;
let noctrl_grammar = "../grammars/mcfgs/nl7-noctrl.mcfg";;

let ctrl_pi = new Runner.parser_interface "mcfgcky2-ctrl" Mcfg2.initialize (Mcfg2.parse ctrl_grammar) Mcfg2.close;;

let noctrl_pi = new Runner.parser_interface "mcfgcky2-noctrl" Mcfg2.initialize (Mcfg2.parse noctrl_grammar) Mcfg2.close;;

Runner.runner#add_pi ctrl_pi;;
Runner.runner#add_pi noctrl_pi;;

let test_sentences = Runner.Util.make_sentence_list 
  (Runner.Util.read_file "dutch-sentences.txt")


let results = Runner.runner#parse_all test_sentences

(*
omdat ik Cecilia helpen.
omdat ik Cecilia Hank zag helpen.
omdat ik Cecilia Hank nijlpaarden zag helpen voeren.*)
