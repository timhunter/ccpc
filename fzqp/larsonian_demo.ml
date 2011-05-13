#use "mcfg2.ml"

let larsonian_pi = new Runner.parser_interface "mcfgcky2-larsonian" Mcfg2.initialize Mcfg2.parse Mcfg2.close;;

let deemptied_pi = new Runner.parser_interface "mcfgcky2-deemptied" Mcfg2.initialize Mcfg2.parse Mcfg2.close;;

Runner.runner#add_pi_with_args larsonian_pi "../grammars/mcfgs/larsonian.mcfg";;

Runner.runner#add_pi_with_args deemptied_pi "../grammars/mcfgs/larsonian.deemptied.mcfg";;

let larsonian_test_sentences = Runner.Util.make_sentence_list 
  (Runner.Util.read_file "larsonian.txt")

let ten_of_each = Runner.Util.n_of_each 10 larsonian_test_sentences

let results = Runner.runner#parse_all larsonian_test_sentences
