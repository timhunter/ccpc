open Util
open Rule
open Parser
open Read 
open Chart
open Rational

(************************************************************************************************
Accepts as command-line args (i) a grammar file
                             (ii) a file containing a list of sentences.
Parses each of the sentences with the given grammar.
For each rule, keeps count of (i) number of time the rule is used
                              (ii) number of times the rule ``could have been'' used.
Outputs a weighted version of the input grammar, given prefixing `num / denom' to each rule.
************************************************************************************************)

let _START_SYMBOL_ = "S"

type rule_counter = (Rule.r, int) Hashtbl.t
type nonterm_counter = (string, int) Hashtbl.t


let increment counter_tbl key weight =
	let n = try (Hashtbl.find !counter_tbl key) with Not_found -> 0.0 in
	Hashtbl.replace !counter_tbl key (n +. weight)

let read_sentences filename =
	let channel = open_in filename in
	let rec get_all_lines ch acc =
		try
			get_all_lines ch ((input_line ch) :: acc)
		with End_of_file -> close_in ch ; acc
	in
	reverse_tr (get_all_lines channel [])

let rec process_tree weight sentence chart rule_uses nonterm_uses tree =
	let item = Derivation.get_root_item tree in
	let children = Derivation.get_children tree in
	let nt = Chart.get_nonterm item in
	let r =
		match (List.filter (fun(is,rule,w) -> is = map_tr Derivation.get_root_item children) (Chart.get_routes item chart)) with
		| (is,rule,w)::[] -> rule
		| _ -> failwith "That's weird: there should be exactly one route from these particular antecedents"
	in
	increment rule_uses r weight;
	increment nonterm_uses nt weight;
	List.iter (process_tree weight sentence chart rule_uses nonterm_uses) children

let process_sentence (rules : Rule.r list) rule_uses nonterm_uses (sentence : string) =
        let w = float_of_string (List.hd (Util.split ' ' sentence)) in
	let split_sentence = List.tl (Util.split ' ' sentence) in
	let chart = Parser.deduce (-1) rules (Parser.Sentence split_sentence) in
	let goal_items = Chart.goal_items chart _START_SYMBOL_ (List.length split_sentence) in
	let goal_derivations = List.concat (map_tr (Derivation.get_derivations chart) goal_items) in
	if (goal_items = []) then
		Printf.eprintf "Warning: no parse found for sentence \"%s\"\n" sentence
	else
		List.iter (process_tree w split_sentence chart rule_uses nonterm_uses) goal_derivations

let run_training grammar_file sentence_file =

	let grammar = Grammar.get_input_grammar grammar_file in
	let (sentences : string list) = read_sentences sentence_file in

	let rule_uses = ref (Hashtbl.create (List.length grammar)) in
	let nonterm_uses = ref (Hashtbl.create ((List.length grammar)/10)) in
	List.iter (process_sentence grammar rule_uses nonterm_uses) sentences ;

	let print_weighted_rule r =
		let num = try Hashtbl.find !rule_uses r with Not_found -> 0.0 in
		let denom = try Hashtbl.find !nonterm_uses (Rule.get_nonterm r) with Not_found -> 0.0 in
		Printf.printf "%f / %f    %s\n" num denom (Rule.to_string r)
	in
	List.iter print_weighted_rule grammar

let print_usage () =
	Printf.eprintf "Usage: %s <grammar-file> <sentence-file>\n" Sys.argv.(0)

let main () =
	if (Array.length Sys.argv = 3) then
		run_training Sys.argv.(1) Sys.argv.(2)
	else
		print_usage ()

let _ = if (!Sys.interactive) then () else main ()
