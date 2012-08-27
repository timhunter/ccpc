
(***********************************************************************************)

let latex_start = ["\\documentclass{article}"; 
                   "\\usepackage{fullpage}";
                   "\\usepackage{tikz}";
                   "\\tikzset{every tree node/.style={align=center,anchor=north}} % allow newlines inside nodes of qtree-style trees";
                   "\\tikzset{every node/.style={align=center,anchor=north}}      % and other nodes too";
                   "\\tikzset{level distance=30pt, sibling distance=100pt}";
                   "\\tikzset{every picture/.style={baseline=-12pt}} % put the baseline at the top of every tikz picture";
                   "\\begin{document}"]

let latex_end = ["\\end{document}"]

(***********************************************************************************)

(* FIXME: Copied, then very minor changes, from visualize.ml! *)
(* Reads from the dict file a returns a mapping from guillaumin-generated 
   preterminals (eg. "t123") to feature sequences (eg. ":: =N D -f") *)
let get_guillaumin_dict filename =

	let channel =
		try open_in filename
		with Sys_error _ -> failwith (Printf.sprintf "Couldn't open dict file %s" filename)
	in

	let regex = Str.regexp "^\([a-z]+[0-9]+\) : (\(::? .*\))$" in

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

(***********************************************************************************)

module MyList : sig

        type ('a,'b) t   (* a non-empty list of 'a, with a 'b labelling each link *)

        val singleton : 'a -> ('a,'b) t
        val hd : ('a,'b) t -> 'a
        val unpack : ('a,'b) t -> 'a * ('b * ('a,'b) t) option
        val extend : ('a,'b) t -> 'b -> 'a -> ('a,'b) t
        val contents : ('a,'b) t -> 'a list
        val labels : ('a,'b) t -> 'b list
        val append : ('a,'b) t -> 'b -> ('a,'b) t -> ('a,'b) t
        val split : ('a,'b) t -> 'a -> (('a,'b) t * 'b * ('a,'b) t)
        val rotate : ('a,'b) t -> 'a -> ('a,'b) t

end = struct

        type ('a,'b) t = End of 'a | NonEnd of ('a * 'b * ('a,'b) t)

        let singleton x = End(x)
        let hd     = function End(x) -> x        | NonEnd(x,_,_) -> x
        let unpack = function End(x) -> (x,None) | NonEnd(x,y,rest) -> (x,Some(y,rest))

        let rec extend lst b a =
                match lst with
                | End(x) -> NonEnd(x,b,End(a))
                | NonEnd(x,y,rest) -> NonEnd(x, y, extend rest b a)

        let rec contents lst =
                match lst with
                | End(x) -> [x]
                | NonEnd(x,_,rest) -> x::(contents rest)

        let rec labels lst =
                match lst with
                | End(_) -> []
                | NonEnd(_,y,rest) -> y::(labels rest)

        let rec append lst1 lbl lst2 =
                match lst1 with
                | End(x) -> NonEnd(x,lbl,lst2)
                | NonEnd(x,y,rest) -> NonEnd(x, y, (append rest lbl lst2))

        (* Splits the list at the first link which leads to element e. *)
        (* The third component of the result should begin with element e. *)
        (* Result type is:   ('a,'b) t * 'b * ('a,'b) t      *)
        let split lst e =
                let rec helper acc link rest =
                        match rest with
                        | End(x) ->            if x = e then (acc,link,rest) else raise Not_found
                        | NonEnd(x,y,rest') -> if x = e then (acc,link,rest) else (helper (extend acc link x) y rest')
                in
                match lst with
                | End(x) -> failwith "split: Can't split a one-element list!"
                | NonEnd(x,y,rest) -> helper (singleton x) y rest

        (* Assuming lst starts and ends with the same element, rotate it so that 
         * it starts and ends with e. *)
        let rotate lst e =
                (* First element in before must be the same as the last element in after *)
                let (before,link,after) = split lst e in
                let rec overwrite_end lst1 lst2 =   (* appends the two, by clobbering the end of lst1 *)
                        match lst1 with
                        | End(x) -> lst2
                        | NonEnd(x,y,rest) -> NonEnd(x, y, overwrite_end rest lst2)
                in
                extend (overwrite_end after before) link e

end

(***********************************************************************************)

module type HISTORY = sig

        type 'a choice = 'a list * 'a * 'a list
        type 'a history

        val singleton : 'a -> 'a history
        val root : 'a history -> 'a
        val last : 'a history -> 'a list * 'a * 'a list
        val extend : 'a history -> Util.weight -> 'a choice -> 'a history
        val to_list : 'a history -> 'a list
        val try_ending_cycle : 'a history -> ('a history * 'a history) option
        val weight_product : 'a history -> Util.weight
        val latex_history_tikz : ('a -> string) -> 'a history -> string
        val show_history : ('a -> string) -> ('a history) -> string
        val rotate : 'a -> 'a history -> 'a history
        val canonicalise : ('a -> 'a -> int) -> 'a history -> 'a history

end

module MyListHistory : HISTORY = struct

        type 'a choice = 'a list * 'a * 'a list
        type 'a history = ('a, 'a list * 'a list * Util.weight) MyList.t

        let singleton x = MyList.singleton x

        let root lst = MyList.hd lst

        let last lst =
                let rec helper (ls,rs) l =
                        match (MyList.unpack l) with
                        | (x, None) -> (ls,x,rs)
                        | (_, Some((ls',rs',_),rest)) -> helper (ls',rs') rest
                in helper ([],[]) lst

        let extend lst wt (ls,x,rs) = MyList.extend lst (ls,rs,wt) x

        let to_list lst = MyList.contents lst

        let weight_product lst =
                let weights = Util.map_tr (fun (_,_,x) -> x) (MyList.labels lst) in
                let one = Util.make_weight (Num.num_of_int 1) (Num.num_of_int 1) in
                List.fold_left Util.mult_weights one weights

        let latex_history_tikz f hist =
                let rec worker h =   (* worker should produce a string of the form "node {...} ..." *)
                        let make_node s = Printf.sprintf "node %s" s in
                        let make_child s = Printf.sprintf "child {%s}" s in
                        let make_child_list xs = String.concat " " (Util.map_tr make_child (Util.map_tr make_node (Util.map_tr f xs))) in
                        match (MyList.unpack h) with
                        | (x, None)                  -> Printf.sprintf "node %s" (f x)
                        | (x, Some((ls,rs,wt),rest)) -> Printf.sprintf "node %s %s %s %s" (f x) (make_child_list ls) (make_child (worker rest)) (make_child_list rs)
                        (* "node {foo} child {node {bar}} child {node {baz}}" *)
                in
                Printf.sprintf "\\tikz \\path %s ;" (worker hist)

        let show_history f h =
                Printf.sprintf "%s %s"
                (Util.show_weight_float (weight_product h))
                (String.concat " -- " (Util.map_tr f (to_list h)))

        let rotate x hist = MyList.rotate hist x

        let canonicalise cmp hist =
                let start = List.hd (List.sort cmp (MyList.contents hist)) in
                rotate start hist

        let try_ending_cycle lst =
                match (MyList.unpack lst) with
                | (_, None) -> None   (* lst is too short to have any cycles *)
                | _ ->
                        let (_,endpoint,_) = last lst in
                        let (before, link, after) = MyList.split lst endpoint in
                        (* after begins with the first occurrence of endpoint in the list *)
                        match (MyList.unpack after) with
                        | (_, None) -> None                                          (* after is just the last link in the list, hence no cycle *)
                        | _ -> Some (MyList.extend before link endpoint, after)      (* after starts somewhere else, in the middle of the list, hence it's a cycle *)

end

module H = MyListHistory

(* returns: string list list *)
let get_child_lists rules nt =
        let relevant_rules = List.filter (fun r -> Rule.get_nonterm r = nt) rules in
        let get_children r =
                match Rule.get_expansion r with
                | Rule.PublicTerminating _ -> []
                | Rule.PublicNonTerminating (nelist,_) -> Nelist.to_list nelist
        in
        Util.map_tr get_children relevant_rules

(* returns: (weight * string list) list *)
let get_child_lists' rules nt =
        let relevant_rules = List.filter (fun r -> Rule.get_nonterm r = nt) rules in
        let get_children r =
                match Rule.get_expansion r with
                | Rule.PublicTerminating _ -> (Rule.get_weight r, [])
                | Rule.PublicNonTerminating (nelist,_) -> (Rule.get_weight r, Nelist.to_list nelist)
        in
        Util.map_tr get_children relevant_rules

let rec choices lst =
        match lst with
        | [] -> []
        | (x::xs) -> let rest = choices xs in
                     let add_x (ls,y,rs) = (x::ls,y,rs) in
                     ([],x,xs) :: (List.map add_x rest)

(* The symbol to start searching from is the one at the end of the history h *)
let rec find_cycles_inner rules h =
        let (_,nt,_) = H.last h in
        match (H.try_ending_cycle h) with
        | Some (before,cycle) -> [(before,cycle)]
        | None ->
                let process_child_list children wt =
                        let choices_with_weights = Util.map_tr (fun choice -> (wt,choice)) (choices children) in
                        List.concat (Util.map_tr (fun (wt,choice) -> find_cycles_inner rules (H.extend h wt choice)) choices_with_weights)
                in
                List.concat (Util.map_tr (fun (wt,children) -> process_child_list children wt) (get_child_lists' rules nt))

let find_cycles rules start_symbol =
        Util.map_tr (fun (b,c) -> (H.canonicalise compare c, b)) (find_cycles_inner rules (H.singleton start_symbol))

let verbatim s =
        let escape_underscores x = Str.global_replace (Str.regexp "_") "\\_" x in
        Printf.sprintf "%s" (escape_underscores s)

let latex_tree_node dict nt =
        match dict with
        | None -> Printf.sprintf "{%s}" (verbatim nt)
        | Some tbl -> try let features = Hashtbl.find tbl (Grammar.desituate nt) in
                          let features_cleaned = Str.global_replace (Str.regexp ": ") ":~" features in
                          let components = Str.split (Str.regexp ";") features_cleaned in
                          let combined = "(" ^ (String.concat ", " components) ^ ")" in
                          Printf.sprintf "{%s\\\\[-0.2\\baselineskip]\\texttt{%s}}" (verbatim nt) (verbatim combined)
                      with Not_found -> Printf.sprintf "{%s}" (verbatim nt)

let compare_by_weight h1 h2 =
        let w1, w2 = H.weight_product h1, H.weight_product h2 in
        Util.compare_weights w1 w2

(* returns:  ('a * 'b list) list *)
let group assocs : (('a * 'b) list) =
        let tbl = Hashtbl.create 10 in
        List.iter (fun (a,b) -> Hashtbl.add tbl a b) assocs ;
        let keys = Hashtbl.fold (fun k v l -> if List.mem k l then l else k::l) tbl [] in
        let result = Util.map_tr (fun k -> (k, Hashtbl.find_all tbl k)) keys in
        result

let latex_cycle dict (cycle, ways_to_cycle) =

        let caption = "Cost of cycle: " ^ (Util.show_weight_float (H.weight_product cycle)) in
        let start = ["\\begin{figure}"; "\\footnotesize"; Printf.sprintf "\\caption{%s}" caption] in

        let tikz_diagram = H.latex_history_tikz (latex_tree_node dict) cycle in
        (* let summarise w = Printf.sprintf "%s %s --> ... --> %s" (Util.show_weight_float (H.weight_product w)) (H.root w) (let (_,x,_) = H.last w in x) in *)
        let summarise w = Printf.sprintf "\\item %s" (H.show_history (fun s -> s) w) in
        let sorted = Util.reverse_tr (List.sort compare_by_weight ways_to_cycle) in
        let ways_in_lines = ["Ways to this cycle";"\\begin{itemize}"] @ (Util.map_tr (fun w -> (verbatim (summarise w)) ^ "\\\\") sorted) @ ["\\end{itemize}"] in
        let middle = [tikz_diagram] @ [""] @ ways_in_lines in

        let finish = ["\\end{figure}"] in
        start @ middle @ finish

(*
 * First argument:              a wmcfg grammar file
 * Second argument (optional):  a guillaumin-generated dict file
 *)
let main () =
        let grammar_file =
                if (Array.length Sys.argv > 1) then
                        Sys.argv.(1)
                else
                        failwith "Need a grammar file on the command line"
        in
        let dict_file =
                if (Array.length Sys.argv > 2) then
                        Some Sys.argv.(2)
                else
                        None
        in
        let (rules, start_symbol) = Grammar.get_input_grammar grammar_file in
        let dict = match dict_file with None -> None | Some f -> Some get_guillaumin_dict f in
        let broken_paths = find_cycles rules start_symbol in    (* a broken path is a pair of histories, the first of which is a cycle *)
        let sort xs = Util.reverse_tr (List.sort (fun (c1,_) (c2,_) -> compare_by_weight c1 c2) xs) in
        let cycles = sort (group broken_paths) in
        let cycles' = Util.map_tr fst cycles in   (* Just the cycles themselves, no "ways in" *)
        prerr_endline (String.concat "\n" (Util.map_tr (H.show_history (fun x -> x)) cycles')) ;
        let prerr_cycle (cycle, ways_to_cycle) =
                prerr_endline "===============" ;
                prerr_endline (H.show_history (fun x -> x) cycle) ;
                prerr_endline "    Ways into this cycle:" ;
                List.iter (fun w -> prerr_endline ("    " ^ (H.show_history (fun x -> x) w))) ways_to_cycle ;
                prerr_endline "===============" ;
                () in
        List.iter prerr_cycle cycles ;
        print_endline (String.concat "\n" latex_start) ;
        print_endline "%\\section{Weights of cycles}" ;
        print_endline "%\\begin{enumerate}" ;
        print_endline (String.concat "\n" (Util.map_tr ((^) "%\\item ") (Util.map_tr (fun h -> Util.show_weight_float (H.weight_product h)) cycles'))) ;
        print_endline "%\\end{enumerate}" ;
        print_endline "%\\clearpage" ;
        print_endline "%\\section{Trees of cycles}" ;
        List.iter (fun (c,ws) -> (List.iter print_endline (latex_cycle dict (c,ws)))) cycles ;
        print_endline (String.concat "\n" latex_end)

let _ = if (!Sys.interactive) then () else main ()

