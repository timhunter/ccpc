open Util
open Rule
open Fsa

(* Convert a string to a list of characters *)
let rec string_to_list str =
  match str with
  | "" -> []
  | s  -> (String.get s 0) :: string_to_list (String.sub s 1 ((String.length s) - 1))

let is_candidate nonterm =
	let isalpha c = (('a' <= c) && (c <= 'z')) || (('A' <= c) && (c <= 'Z')) in
	match (string_to_list nonterm) with
	| ('S' :: cs) -> (List.filter isalpha cs = [])
	| _ -> false

let choose_start_symbol nonterms =
	let candidates = List.filter is_candidate nonterms in
	match (uniques candidates) with
	| (x::[]) -> x
	| _ -> failwith "Couldn't identify unique start symbol in this grammar"

let get_input_grammar grammar_file =
  let rules = 
  try 
    let channel = open_in grammar_file in 
    let lexbuf = Lexing.from_channel channel in 
    Read.mcfgrule Lexer.token lexbuf
  with _ -> failwith ("Can't parse input mcfg file "^grammar_file^"\n")
  in
  let start_symbol = choose_start_symbol (List.map Rule.get_nonterm rules) in
  (rules, start_symbol)

(******************************************************************************************)

let get_guillaumin_dict filename =

	let channel =
		try open_in filename
		with Sys_error _ -> failwith (Printf.sprintf "Couldn't open dict file %s" filename)
	in

	let regex = Str.regexp "^\\([a-z]+[0-9]+\\) : (\\(::? .*\\))$" in

	let result = Hashtbl.create 100 in
	begin
		try
			while true; do
				let line = input_line channel in
				if (Str.string_match regex line 0) then
					let category   = Str.matched_group 1 line in
					let features   = Str.matched_group 2 line in
					Hashtbl.add result category features
				else if (line <> "") then
					Printf.eprintf "WARNING: Ignoring unexpected line in dictionary file: %s\n" line
			done
		with End_of_file ->
			close_in channel
	end ;
	result

(************************************************************************************************************)

(* Extract the intersection grammar *)
(* See Albro's dissertation, appendix C section C.4 *)

let make_new_rule sit_nonterm rights func range_lists weight =
  let new_rights = List.map2 situate rights range_lists in
  let new_agenda_items = List.map2 Chart.create_item rights range_lists in
  (Rule.create_nonterminating (sit_nonterm, new_rights, func, weight), new_agenda_items)

(* This implements step (b) on p.293 of Albro's dissertation. *)
(* Given a particular item that we have just pulled off the agenda, it returns a list of 
   new agenda items and a list of new grammar rules. *)
let new_intersection_grammar_rules chart item =
  let sit_nonterm = situate (Chart.get_nonterm item) (Chart.get_ranges item) in
  let routes = Chart.get_routes item chart in
  let make_rule_for_route ((items, rule) : Chart.route) : (Rule.r * Chart.item list) =
    match (Rule.get_expansion rule) with
    | PublicTerminating str -> (
        let fsa_weight =
            assert (items = []) ;   (* If this route used a terminating rule, there can't be any antecedent items *)
            match (Chart.get_ranges item) with
            | [Range(fsa,Some (i,j))] -> let w = weight_of_arc fsa (i,j) str in
                                         assert (compare_weights w (weight_from_float 0.0) <> 0) ;    (* w should be non-zero *)
                                         w
            | [Range(_,None)] -> assert (str = " ") ; weight_from_float 1.0     (* The FSA assigns ``no cost'' for cover-nothing transitions *)
            | _ -> assert false ; (* Since rule is a terminating rule, the item it derived should cover exactly one range *)
        in
        let new_rule_weight = mult_weights fsa_weight (Rule.get_weight rule) in
        (Rule.create_terminating (sit_nonterm, str, new_rule_weight), [])
      )
    | PublicNonTerminating (nts,func) -> (
        assert (Nelist.to_list nts = map_tr Chart.get_nonterm items) ;
        (make_new_rule sit_nonterm (map_tr Chart.get_nonterm items) func (map_tr Chart.get_ranges items) (Rule.get_weight rule))
      )
  in
  let results_to_combine : (Rule.r * Chart.item list) list = List.rev_map make_rule_for_route routes in
  let new_rules = map_tr fst results_to_combine in
  let new_agenda_items = concatmap_tr snd results_to_combine in
  (new_rules, new_agenda_items)

(********************************************************************)
(*** A queue with a memory of everything that's ever been in it. ****)
type 'a myqueue = ('a Queue.t) * (('a,unit) Hashtbl.t)
let create_myqueue () = (Queue.create (), Hashtbl.create 1000)
let contains mq x = Hashtbl.mem (snd mq) x
let pop_myqueue mq = Queue.pop (fst mq)
let is_empty_myqueue mq = Queue.is_empty (fst mq)
let add_if_new mq x =
	if (not (contains mq x)) then (
		Queue.add x (fst mq) ;
		Hashtbl.add (snd mq) x ()
	) else ()
(********************************************************************)

(* This is a recursive version of the while-loop on p.292-293 of Albro's dissertation. *)
(* q is the agenda, maintained as mutable state. *)
(* grammar_so_far is the accumulating list of rules that will comprise the intersection grammar. *)
(* The function new_intersection_grammar_rules implements the guts of the process, and returns the 
   new agenda items and the new grammar rules that are added to q and grammar_so_far in a single iteration. *)
let rec build_intersection_grammar chart q grammar_so_far =
  if (is_empty_myqueue q) then
    grammar_so_far
  else
    let trigger = pop_myqueue q in
    let (new_rules, new_agenda_items) = new_intersection_grammar_rules chart trigger in
    List.iter (fun item -> add_if_new q item) new_agenda_items ;             (* Add new agenda items to q *)
    build_intersection_grammar chart q (grammar_so_far @ new_rules)          (* Go round again, with new grammar rules added *)

let intersection_grammar chart start_symbol fsa = 
  let q = create_myqueue () in
  add_if_new q (Chart.goal_item start_symbol fsa) ;
  let new_start_symbol = Rule.situate start_symbol [Range(fsa, goal_span fsa)] in
  let new_rules = build_intersection_grammar chart q [] in
  (new_rules, new_start_symbol)

module SituatedNode =
  struct
    type t = { name : string ; spans : range list }

    let create itm =
      match (Chart.get_nonterm itm) with
	" " -> { name = "EMPTY" ; spans = (Chart.get_ranges itm) }
      | nonempty -> { name = nonempty ; spans = (Chart.get_ranges itm) }

    let compare x y =
      let rec cmp l1 l2 = match (l1,l2) with
	  ([],[]) -> 0 (* identical lists *)
	| ([],_) -> -1
	| (_,[]) -> 1
	| (x::xs,y::ys) -> match (compare x y) with
	    0 -> cmp xs ys
	    | otheranswer -> otheranswer in
      match (cmp x.spans y.spans) with
	  0 -> compare x.name y.name
	| otherwise -> otherwise

    let equal x y = (x=y)

    let hash = Hashtbl.hash
  end

(* apply the relation m to "old" and "current". Used to calculate subgraph widths in get_subgraph below *)
let extremum m old current = match current with
  | Range(_,   Some (i,j)) -> m (m (index_of i) (index_of j)) old
  | Range(fsa, None)       -> let i = start_state fsa in
                              let j  = end_state fsa in
                              m (m (index_of i) (index_of j)) old

module SituatedGraph =
  struct
  include Graph.Imperative.Digraph.Concrete(SituatedNode)

  let default_edge_attributes _ = []
  let edge_attributes _ = []

  let vertex_name x = ("\""^(situate x.SituatedNode.name x.SituatedNode.spans)^"\"")

  let default_vertex_attributes _ = [ `Shape (`Plaintext) ]
  let vertex_attributes v = [] (* if v.SituatedSymbol.highlight then [ `Fontcolor (0xdc143c) ] else [] *)

  let graph_attributes _ = []

  let get_subgraph v =
    let leftmost = List.fold_left (extremum min) min_int v.SituatedNode.spans in
    let rightmost = List.fold_left (extremum max) max_int v.SituatedNode.spans in

    let width = abs (rightmost - leftmost) in
      Some ({ Graph.Graphviz.DotAttributes.sg_name = (string_of_int width);
	      Graph.Graphviz.DotAttributes.sg_attributes=[] })
  end

(* analogous to new_intersection_grammar_rules above *)
let update_graph graph chart item =
  let routes = Chart.get_routes item chart in
  let parentnode = SituatedNode.create item in
  
  let do_route ((items, rule) : Chart.route) =
    match (Rule.get_expansion rule) with
	PublicTerminating str -> let leaf = match str with
	    " "  -> ({ SituatedNode.name="EMPTY" ; SituatedNode.spans=[] }  : SituatedNode.t) 
	  | nonempty ->  ({ SituatedNode.name=nonempty ; SituatedNode.spans=[] }  : SituatedNode.t) in
				 begin
				   SituatedGraph.add_edge graph parentnode leaf;
				   []
				 end
      | PublicNonTerminating (nts,_) -> let children = List.map SituatedNode.create items in
	begin
	  List.iter (SituatedGraph.add_edge graph parentnode) children;
	  items (* should return a list of new agenda items somehow *)
         end
  in
    concatmap_tr do_route routes;;
      

(* analogous to build_intersection_grammar only the output is a SituatedGraph *)
let rec build_graph chart q graph =
  if (is_empty_myqueue q) then
    graph
  else
    let trigger = pop_myqueue q in
    let new_agenda_items = update_graph graph chart trigger in
    List.iter (fun item -> add_if_new q item) new_agenda_items ;
    build_graph chart q graph

module DotWriteSituatedGraph = Graph.Graphviz.Dot(SituatedGraph)

let drawgraph chart goal_items symbols filename =
  let q = create_myqueue () in
  List.iter (fun item -> add_if_new q item) goal_items ;
  let gr = build_graph chart q (SituatedGraph.create ~size:(Chart.length chart) ()) in
  let _ = print_string ("graph has "^(string_of_int (SituatedGraph.nb_vertex gr))^" vertices and "^(string_of_int (SituatedGraph.nb_edges gr))^" edges.\n") in
  let oc = Unix.open_process_out ("sed 's/digraph G {/digraph G { clusterrank=none;  ordering=out;/' >| "^filename) in
    begin
      DotWriteSituatedGraph.output_graph oc gr;
      close_out oc
    end


  
